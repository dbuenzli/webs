(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Webs [Unix] tooling.

    {ul
    {- TODO add [recv_file].}} *)

open Webs

(** {1:listeners Connection listeners} *)

type listener =
[ `Host of string * int (** Bind a socket on given hostname and port. *)
| `Sockaddr of Unix.sockaddr (** Bind a socket on given address. *)
| `Fd of Unix.file_descr (** Listen on that socket, the client is
                             responsible for closing it. *) ]
(** The type for specifying the socket to listen for connections on. *)

val listener_localhost : listener
(** [listener_localhost] is [`Host ("localhost", 8000)]. *)

val fd_of_listener : listener -> (Unix.file_descr * bool, string) result
(** [fd_of_listener l] is [Ok (fd, close)] a file descriptor [fd] for
    the specification [l] and [close] is [true] if the client is in
    charge of closing it. Unless [l] was [`Fd], [fd] has
    {{:Unix.set_close_on_exec}close on exec} set to [true]. *)

val listener_of_string :
  ?default_port:int -> string -> (listener, string) result
(** [listener_of_string s] parses a listen specification from [s]. The format
    is [ADDR[:PORT]] or [PATH] for a Unix domain socket. [default_port]
    is used if no port is specified, defaults to [8000].

    The function here should not be ignored. *)

(** The function here should not be ignored *)

val pp_listener : Format.formatter -> listener -> unit
(** [pp_listener] formats an unspecified representation of listen values. *)

(** {1:connection Connections} *)

type Resp.connection += Fd of Unix.file_descr (** *)
(** The type for Unix response connections. The file descriptor on which
    the response is written. *)

(** Tools for writing connectors. *)
module Connector : sig

  (** {1:req Reading requests} *)

  val read : Unix.file_descr -> bytes -> start:int -> len:int -> int
  (** [read fd b ~start ~len] reads at most [len] bytes of [fd] into [b]
      starting at [start]. *)

  val req_body_reader :
    max_req_body_byte_size:int -> body_length:int option -> Unix.file_descr ->
    bytes -> first_start:int -> first_len:int ->
    (unit -> (bytes * int * int) option)
  (** [resp_body_reader fd b ~start ~start_len] is a body reader for [fd] that
      using buffer [b] and assuming the first [first_len] bytes are already in
      [b] at [first_start]. *)

  (** {1:resp Writing responses} *)

  val write : Unix.file_descr -> bytes -> start:int -> len:int -> unit
  (** [write fd b ~start ~len] writes [len] byte of [b] starting at [start]
      on [fd]. Raises [Unix.Unix_error] but handles [Unix.EINTR]. *)

  val resp_body_writer : Webs.Resp.t -> Webs.Resp.t * (Unix.file_descr -> unit)
  (** [resp_body_writer r] is a body writer for [r] along with an upated
      response [r] in case the response is a file. *)
end

(** {1:sendfile Sending files}

    See {{!page-web_service_howto.serving_files}this section} of the
    web service howto. To use the following you need a connector
    that supports {{!Webs_unix.connection}Unix response connections}.
    *)

(** {2:etag Etags} *)

type etagger =
  Http.fpath -> Unix.file_descr -> Unix.stats -> (Http.Etag.t, string) result
(** The type for functions for determining file etags. The function is
    given the filepath, a file descriptor open on it and its file stat
    record. *)

val default_etagger : etagger
(** [default_etagger] implements the [nginx] etag scheme namely
    the file stat's mtime and size written in lowercase
    hexadecimal as [hex(mtime)-hex(size)]. *)

(** {2:dir_resp Directory responses} *)

type dir_resp =
  etagger:etagger -> mime_types:Http.Mime_type.file_ext_map option -> Req.t ->
  Http.fpath -> (Resp.t, Resp.t) result
(** The type for functions for directory responses. Given an
    [etagger], [mime_types], the request and the file path to
    (existing) directory, the function should follow up with the
    response. If the response is a static file [etagger] and
    [mime_types] should be used to determine its etag and MIME
    type. *)

val dir_404 : dir_resp
(** [dir_404] is a directory response that errors with
    {!Webs.Http.not_found_404}. *)

val dir_index_file : string -> (dir_resp, string) result
(** [dir_index_file file] serves the file [file] in the directory via
    {!send_file}. Errors if [file] contains a directory seperator or
    is [".."]. *)

(** {2:send Sending} *)

val send_file :
  ?dir_resp:dir_resp -> ?etagger:etagger ->
  ?mime_types:Http.Mime_type.file_ext_map -> Req.t -> Http.fpath ->
  (Resp.t, Resp.t) result
(** [send_file ~dir_resp ~etagger ~mime_types r file] responds to [r] by
    sending file [file], use {!Webs.Req.to_absolute_filepath} to
    determine one from [r] safely.

    More precisely it proceeds as follows:

    {ol
    {- If [file] is a directory, continues with [dir_resp] (defaults to
       {!dir_404}).}
    {- If [file] is a file, an etag [t] is computed for [file] using
      [etagger] (defaults to {!default_etagger}).}}

    Equipped with the actual [file] and its [etag] the function
    proceeds to:

    {ol
    {- {{!Webs.Http.Etag.eval_if_match}Evaluate} [r]'s [if-match] header
       condition (if any) with tag [t]. If that is [false], errors with
       a {!Webs.Http.precondition_failed_412} response.}
    {- {{!Webs.Http.Etag.eval_if_none_match}Evaluate} [r]'s [if-none-match]
       header condition with tag [t]. If that is [false], responds with
       {!Webs.Http.not_modified_304}}
    {- If [r] is a [HEAD] request, respond with {!Webs.Http.ok_200} and
       an empty body at that point.}
    {- If [r] has a [range] header,
       {{!Webs.Http.Etag.eval_if_range}evaluate} [r]'s [if-range] header
       (if any) with tag [t]. If that is [false], the range request is
       turned into a full response otherwise the first satisfiable
       range is served with {!Webs.Http.partial_content_206}, if
       there is no satisifiable range, errors with
       {!Webs.Http.range_not_satisfiable_416}.}
    {- If [r] has no [range] header or the [if-range] condition
       failed respond with {!Webs.Http.ok_200} and a body with [file]'s
       content.}}

    The content type of the response is determined using
    {!Webs_kit.Mime_type.of_filepath} with [mime_types] and [file].

    Responses include an {!Http.H.last_modified} header derived from
    the file modification time. Apparently without this you do not hit
    the browser memory cache in blink based browsers.

    In addition to the errors mentioned above, the function also errors
    with a:

    {ul
    {- {!Webs.Http.bad_request_400} on any header decoding
       errors.}
    {- {!Webs.Http.method_not_allowed_405} response, if
       [r]'s method is different from [`GET] or [`HEAD].}
    {- {!Webs.Http.range_not_satisfiable_416} if there is a byte
       range request and none of the ranges can be satisfied.}
    {- {!Webs.Http.not_found_404} response, if there's any
       on file system call error (e.g. [EPERM]).
       A human readable explanation can be found in the response's
       {{!Webs.Resp.explain}explanation} which stays on the server.}}

    Data is sent using a {!Webs.Resp.Direct} response using the
    {!Webs_unix.Fd} connection with the {!val:sendfile} system call or
    a fallback if not available. For range requests a single range is
    responded with; there is no [multipart/byteranges] support for the
    time being. *)

(** {1:missing Missing [Unix] bindings} *)

val realpath : string -> string
(** [realpath p] is an absolute pathname for [p] obtained by resolving
    all extra [/] characters, relative path segments and symbolic links.
    Raises [Unix.Unix_error].

   {b Note.} This can be removed once an OCaml version with
   {{:https://github.com/ocaml/ocaml/pull/10047}this PR} is required. *)

val sendfile :
  src:Unix.file_descr -> off:int -> len:int -> Unix.file_descr -> int
  (** [sendfile ~src ~off ~len ] writes [len] bytes starting at [off]
      in [src] to [dst]. Raises [Unix.Unix_error], you'll also need to
      do the [Unix.EINTR] dance. Raises [Sys_error] if unsupported on
      the platform. *)


(** Measuring time.

    Support to measure monotonic wall-clock time. *)
module Time : sig
   (** {1:span Monotonic time spans} *)

  type span
  (** The type for non-negative monotonic time spans. They represent
      the difference between two clock readings with nanosecond precision
      (1e-9s). *)

  (** Time spans *)
  module Span : sig

    (** {1:span Time spans} *)

    type t = span
    (** See {!type:span}. *)

    val zero : span
    (** [zero] is a span of 0ns. *)

    val one : span
    (** [one] is a span of 1ns. *)

    val max : span
    (** [max_span] is a span of [2^64-1]ns. *)

    val add : span -> span -> span
    (** [add s0 s1] is [s0] + [s1]. {b Warning.} Rolls over on overflow. *)

    val abs_diff : span -> span -> span
    (** [abs_diff s0 s1] is the absolute difference between [s0] and [s1]. *)

    (** {1:preds Predicates and comparisons} *)

    val equal : span -> span -> bool
    (** [equal s0 s1] is [s0 = s1]. *)

    val compare : span -> span -> int
    (** [compare s0 s1] orders span by increasing duration. *)

    (** {1:conv Conversions} *)

    val to_uint64_ns : span -> int64
    (** [to_uint64_ns s] is [s] as an {e unsigned} 64-bit integer nanosecond
        span. *)

    val of_uint64_ns : int64 -> span
    (** [of_uint64_ns u] is the {e unsigned} 64-bit integer nanosecond span [u]
        as a span. *)

    val pp_ms : Format.formatter -> span -> unit
    (** [pp_ms ppf s] prints [s] as an unsigned 64-bit integer millisecond
        span. *)

    val pp_ns : Format.formatter -> span -> unit
    (** [pp_ns ppf s] prints [s] as an unsigned 64-bit integer nanosecond
        span. *)
  end

  (** {1:monotonic_counters Monotonic wall-clock time counters} *)

  type counter
  (** The type for monotonic wall-clock time counters. *)

  val counter : unit -> counter
  (** [counter ()] is a counter counting from now on. *)

  val count : counter -> span
  (** [count c] is the monotonic time span elapsed since [c] was created. *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
