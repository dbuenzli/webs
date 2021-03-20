(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** HTTP/1.1 gateway connector.

    This connector serves an unbounded number of requests by taking
    connection and HTTP/1.1 requests on a {!Webs_unix.listener}.  If
    only for [https] support, it should only ever be used behind an
    HTTP gateway or locally for development.

    See the {{!page-web_service_howto}Web service howto} manual for
    instructions to connect a minimal example to an HTTP gateway. *)

open Webs

(** {1:connector Connector} *)

type t
(** The type for HTTP/1.1 connectors. Implemented using one {!Thread}
    per connection. *)

val create :
  ?log:(Connector.log_msg -> unit) -> ?max_connections:int ->
  ?max_req_headers_byte_size:int -> ?max_req_body_byte_size:int ->
  ?listener:Webs_unix.listener -> unit -> t
(** [create ()] is a new connector with following parameters:
    {ul
    {- [listen] specifies the socket to listen to on.
       Defaults to {!Webs_unix.listener_localhost}}
    {- [max_connections] is the maximal number of allowed concurrent
       connections.}
    {- [max_req_headers_byte_size] is the maximal allowed size in bytes for
       to the request line and headers. Defaults to [64Ko].}
    {- [max_req_body_byte_size] is the maximal request body size in bytes.
       FIXME not enforced, unclear where this is to put the limit on, for
       streaming bodies, if we cut the line the service might end up
       being confused (but then it should also cater for that possibility).}
    {- [log] logs connector log messages. Defaults to
       {!Webs.Connector.default_log} with trace messages.}} *)

val listener : t -> Webs_unix.listener
(** [listener c] is the connector connection listener. *)

val serve : t -> Webs.service -> (unit, string) result
(** [serve c s] runs service [s] with connector [c]. This blocks,
    serving requests with [s] until {!stop} is called on [c].

    The {!Webs.Req.service_root} of requests is decoded from the
    custom HTTP header [x-service-root] this should be set
    appropriately by your gateway.

    Note that the server may respond before the request hits [service], notably:
    {ul
    {- If [max_req_headers_byte_size] or [max_req_body_byte_size]
       are exceeded the server responds to the client with a
       {!Webs.Http.s413_payload_too_large}.}
    {- If the basics to parse the {!Req} data structure and setup the
       body stream is not there, the server responds with
       {!Webs.Http.s400_bad_request}}
    {- If a {!Webs.Http.H.expect} header is found TODO}}

    {b Signals.} When [serve] is entered {!Stdlib.Sys.sigpipe} is made
    to be ignored. The previous value is restored when the function returns. *)

val stop : t -> unit
(** [stop s] stops [s]. If [s] is blocked on [serve] this makes it
    stop accepting new connection.

    {b TODO.} At the moment this does not ensure all connections
    get terminated cleanly. *)

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
