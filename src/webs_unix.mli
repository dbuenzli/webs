(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Webs [Unix] tooling. *)

open Webs

(** {1:sendfile Sending files}

    Can be used with connectors that support {{!Webs_unix.connection}Unix
    response connections}. See {{!web_service_howto.serving_files}this
    section} of the web service howto. *)

val etag_of_file : string -> Unix.file_descr -> (string option, string) result
(** [etag_of_file _ fd] is an [etag] for [fd]. It uses the [nginx]
    scheme [hex(mtime)-hex(size)]. *)

val send_file :
  ?etag:(string -> Unix.file_descr -> (string option, string) result) ->
  docroot:string -> Req.t -> (Resp.t, Resp.t) result
(** [send_file ~docroot req].

    {ul
    {- An etag is computed or the file via [etag] which is given
       the filename and its open file descriptor. [etag] defaults
       {!etag_of_file}, if you
       compute your own respect the
       {{:https://tools.ietf.org/html/rfc7232#section-2.3}[entity-tag]}
       format. } }

    {b TODO.} Implement the full logic with etags, conditionals
    and ranges. *)
(*
{ul
{- The file is resolved with respect to [docroot], an [etag] is
   computed via [etag] (defaults to the nginx scheme:
   [hex(mtime)-hex(size)]). If the [etag] matches the file body response
   [etag] an empty {!Http.s304_not_modified} is returned. Otherwise
   the file is send with a direct body via {!sendfile} if the system
   call is available; otherwise it fallbacks to a body stream with
   reads and writes.}
    {- [Error r'] is returned with {!Http.s404_forbidden} in case
       of file permission error (TODO should we rather 404 with a reason ?)
       or {!Http.s500_server_error} in various other cases}}
    {- A {!Webs.Http.s200_ok} or {!Webs.Http.s206_partial_content}
      (if applicable) status.}
    {- A file request body with {!Webs.Req.path} as the file path
       and a possible range of bytes extracted from a
       {!Webs.Http.H.range} present in [r].}
    {- Headers [set] set (defaults to {!Webs.Http.H.empty}).}}
    In the bad cases the response is an empty response without [set]
    and with status:
    {ul
    {- {!Webs.Http.s400_bad_request} if {!Webs.Req.path} is empty}
    {- {!Webs.Http.s416_range_not_satisfiable} if no sense could
        be made out of the {!Webs.Http.H.range} header.}
    {- {!Webs.Http.s405_not_allowed} if {!Webs.Req.meth} is
       neither [`GET] nor [`HEAD].}}
*)

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
(** [fd_of_listener l] is [fd, close] a file descriptor for the specification
    [l], [close] is [true] if the client is in charge of closing it. Unless
    [l] was [`Fd], [fd] has {{:Unix.set_close_on_exec}close on exec}
    set to [true]. *)

val listener_of_string :
  ?default_port:int -> string -> (listener, string) result
(** [listener_of_string s] parses a listen specification from [s]. The format
    is [ADDR[:PORT]] or [PATH] for a Unix domain socket. [default_port]
    is used if no port is specified, defaults to [8000]. *)

val pp_listener : Format.formatter -> listener -> unit
(** [pp_listener] formats an unspecified representation of listen values. *)

(** {1:connection Connections} *)

type Resp.connection += Fd of Unix.file_descr (** *)
(** The type for Unix response connections. *)

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

(** {1:missing Missing [Unix] bindings} *)

val realpath : string -> string
(** [realpath p] is an absolute pathname for [p] obtained by resolving
    all extra [/] characters, relative path segments and symbolic links.
    Raises [Unix.Unix_error].

   {b Note.} This can be removed once a OCaml version with
   {{:https://github.com/ocaml/ocaml/pull/10047}this PR} is required. *)

val sendfile :
  src:Unix.file_descr -> off:int -> len:int -> Unix.file_descr -> int
  (** [sendfile ~src ~off ~len ] writes [len] bytes starting at [off]
      in [src] to [dst]. Raises [Unix.Unix_error], you'll also need to
      do the [Unix.EINTR] dance. Raises [Sys_error] if unsupported on
      the platform. *)

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
