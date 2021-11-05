(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** HTTP/1.1 gateway connector.

    This connector serves a bounded number of concurrent requests by
    taking connection and HTTP/1.1 requests on a
    {!Webs_unix.listener}. If only for [https] support, it should only
    ever be used behind an HTTP gateway or locally for development.

    See the {{!page-web_service_howto}Web service howto} manual for
    instructions to connect a minimal example to an HTTP gateway. *)

open Webs

(** {1:connector Connector} *)

val default_max_connections : int
(** [default_max_connection] is [100]. *)

type t
(** The type for HTTP/1.1 connectors. Each connection is served using
    one {!Thread} drawn from a pool. *)

val create :
  ?listener:Webs_unix.listener -> ?log:(Connector.log_msg -> unit) ->
  ?max_connections:int -> ?max_req_body_byte_size:int ->
  ?max_req_headers_byte_size:int -> ?service_path:Http.path ->
  unit -> t
(** [create ()] is a new connector with following parameters:
    {ul
    {- [listener] specifies the socket to listen to on.
       Defaults to {!Webs_unix.listener_localhost}}
    {- [log] logs connector log messages. Defaults to
       {!Webs.Connector.default_log} with trace messages.}
    {- [max_connections] is the maximal number of allowed concurrent
       connections. Defaults to {!default_max_connections}.}
    {- [max_req_body_byte_size] is the maximal request body size in bytes.
       FIXME not enforced.}
    {- [max_req_headers_byte_size] is the maximal allowed size in bytes for
       to the request line and headers. Defaults to [64Ko].}
    {- [service_path] is the path at which the root of the service is being
       served. This path is stripped from the
       path found in request targets to yield the {!Webs.Req.path} of served
       requests. The connector responds with a {!Webs.Http.bad_request_400}
       if the strip fails. The value of the service path can be found
       in the {!Webs.Req.service_path} of the requests to serve.
       Defaults to [[""]].}} *)

val listener : t -> Webs_unix.listener
(** [listener c] is the connection listener of [c]. See {!create}. *)

val log : t -> Connector.log_msg -> unit
(** [log c] is the log of [c]. See {!create}. *)

val max_connections : t -> int
(** [max_connection c] is the maximal number of concurrent connections
    for [c]. See {!create}. *)

val max_req_body_byte_size : t -> int
(** [max_connection c] is the maximal body size in bytes for requests
    handled by [c]. See {!create}. *)

val max_req_headers_byte_size : t -> int
(** [max_connection c] is the maximal headers size in bytes  for requests
    handled by [c]. See {!create}. *)

val service_path : t -> Http.path
(** [service_path c] is the service path of [c]. See {!create}. *)

val serving : t -> bool
(** [serving c] is [true] iff [c] a {!serve} is going on. *)

(** {1:serving Serving} *)

val serve : ?handle_stop_sigs:bool -> t -> Http.service -> (unit, string) result
(** [serve c s] runs service [s] with connector [c]. This blocks,
    serving requests with [s] until {!stop} is called on [c] or
    a [SIGINT] or [SIGTERM] signal is received if [handle_stop_sigs] is [true]
    (default). If [serving c] is [true] this returns immedialy with [Ok ()].

    Note that the server may respond before the request hits [service], notably:
    {ul
    {- If [max_req_headers_byte_size] or [max_req_body_byte_size]
       are exceeded the server responds to the client with a
       {!Webs.Http.payload_too_large_413}.}
    {- If the basics to parse the {!Req} data structure and setup the
       body stream is not there, the server responds with
       {!Webs.Http.bad_request_400}}
    {- If a {!Webs.Http.expect} header is found TODO}}

    {b Signals.} When [serve] is entered {!Stdlib.Sys.sigpipe} is made
    to be ignored and a handler for {!Stdlib.Sys.sigint} and
    {!Stdlib.Sys.sigstop} is installed if [handle_stop_sigs] is
    [true]. The previous values are restored when the function
    returns. *)

val stop : t -> unit
(** [stop s] stops [s]. If [s] is blocked on [serve] this makes it
    stop accepting new connection. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers

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
