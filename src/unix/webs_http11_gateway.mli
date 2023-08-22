(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** HTTP/1.1 gateway connector.

    This connector serves a bounded number of concurrent HTTP/1.1
    requests using one {!Thread} per request drawn from a thread pool.

    See the {{!page-http_service_howto}HTTP service howto} for
    instructions to use this connector with an HTTP gateway.

    {b Warning.} This should only ever be used behind a proper HTTP gateway
    or on [localhost] for development. In particular:
    {ul
    {- It has no [https] support.}
    {- It has no mitigations against slow HTTP attacks.}
    {- The [max_request_body_byte_size] parameter of {!make} is not enforced
       for now.}} *)

open Webs

(** {1:connector Connector} *)

val default_max_connections : int
(** [default_max_connection] is [100]. *)

type t
(** The type for HTTP/1.1 gateway connectors. *)

val make :
  ?listener:Webs_listener.t -> ?log:(Http.Connector.Log.msg -> unit) ->
  ?max_connections:int -> ?max_request_body_byte_size:int ->
  ?max_request_headers_byte_size:int -> ?service_path:Http.Path.t ->
  unit -> t
(** [make ()] is a new connector with:
    {ul
    {- [listener] the socket to listen to on.
       Defaults to {!Webs_listener.localhost_8000}}
    {- [log] the connector message logger. Defaults to
       {!Webs.Http.Connector.Log.default} with trace enabled.}
    {- [max_connections] the maximal number of allowed concurrent
       connections. Defaults to {!default_max_connections}. Indirectly
       this defines the size of the {!Webs_thread_pool}. That is the number
       of threads in your program}
    {- [max_request_body_byte_size] the maximal request body size in bytes.
       Defaults to {!Webs.Http.Connector.Default.max_request_body_byte_size}.
       {b Warning} Not enforced for now.}
    {- [max_request_headers_byte_size] is the maximal allowed size in bytes for
       to the request line and headers. Defaults to
       {!Webs.Http.Connector.Default.max_request_headers_byte_size}.}
    {- [service_path] is the path on which the root of the service is
       served by the gateway. Defaults to {!Webs.Http.Path.root}.}} *)

val listener : t -> Webs_listener.t
(** [listener c] is the connection listener of [c]. See {!make}. *)

val log : t -> Http.Connector.Log.msg -> unit
(** [log c] is the log of [c]. See {!make}. *)

val max_connections : t -> int
(** [max_connection c] is the maximal number of concurrent connections
    for [c]. See {!make}. *)

val max_request_body_byte_size : t -> int
(** [max_request_body_byte_size c] is the maximal body size in bytes
    for requests handled by [c]. {b Warning} not enforced for now. See
    {!make}. *)

val max_request_headers_byte_size : t -> int
(** [max_request_headers_byte_size c] is the maximal headers size in
    bytes for requests handled by [c]. See {!make}. *)

val service_path : t -> Http.Path.t
(** [service_path c] is the service path of [c]. The path on which
    the root of the service is served by the gateway.
    See {!Webs.Http.Request.service_path}. *)

val serving : t -> bool
(** [serving c] is [true] iff [c] a {!serve} is going on. *)

(** {1:serving Serving} *)

val serve :
  ?handle_stop_sigs:bool -> t -> (Http.Request.t -> Http.Response.t) ->
  (unit, string) result
(** [serve c service] runs the service [service] with connector [c].

    If [serving c] is [true] this returns immedialy with [Ok ()].
    Otherwise this blocks and serves requests by calling [service]
    in a thread drawn from a thread pool limited by {!max_connections}
    until either:

    {ul
    {- {!stop} is called on [c]}
    {- A [SIGINT] or [SIGTERM] signal is received when [handle_stop_sigs] is
       [true] (default).}}

    The function waits for ongoing requests to finish before
    returning. After the function returns it is possible to
    call [serve] again on [c].

    The connector supports:
    {ul
    {- {!Webs_unix.Fd.Writer} body writers.}
    {- The
       {{!page-connector_conventions.service_requests}
       service request} conventions.}
    {- The {{!page-connector_conventions.service_responses}
       service response} conventions.}
    {- The {{!page-connector_conventions.service_connector_responses}
       connector responses} conventions.}}

    {b Signals.} When [serve] is entered and [handle_stop_sigs] is
    [true] (default), {!Stdlib.Sys.sigpipe} is made to be ignored and
    a handler calling {!stop}[ c] is installed on {!Stdlib.Sys.sigint}
    and {!Stdlib.Sys.sigstop}. The signal handling that existed before
    the function was called is restored when the function returns. *)

val stop : t -> unit
(** [stop c] stops [c]. If [serving c] is [true] this makes it stop
    accepting new connections, waits for the last requests to finish
    and unblocks the {!serve} call. *)
