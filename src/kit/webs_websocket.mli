(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Websockets upgrade support.

    {b References.}
    {ul
    {- I. Fette et al. RFC 6455
    {{:https://www.rfc-editor.org/rfc/rfc6455}
    {e The WebSocket Protocol}}. 2011}} *)

open Webs

(** {1:keys Keys} *)

type key = string
(** The type for websocket keys. These are random 16 bytes
    encoded in base64 with padding. *)

val random_key : ?crypto_random:Webs_crypto_random.t -> unit -> key
(** [random_key ()] is a random websocket key sourced from the generator
    [crypto_random] (default to {!Webs_crypto_random.get}). *)

(** {1:upgrade_client Upgrading from the client} *)

val url_schemes : (Url.scheme * Url.Authority.port) list
(** [url_schemes] has the list of URL schemes ([http], [https], [ws], [wss])
    and their default ports for handshaking. Can be used with
    {!Webs.Url.to_endpoint}.  *)

val request_upgrade_of_url :
  ?key:key -> ?headers:Http.Headers.t -> ?log:string -> Url.t ->
  (key * Http.Request.t, string) result
(** [request_upgrade_of_url ~url] is a [`GET] request constructed like
    {!Webs.Http.Request.of_url} and with headers
    {!add_request_upgrade_headers}[ ?key headers] added and returns [key].
    The scheme of [url] must be one of ["http"], ["https"], ["ws"] or
    ["wss"]. *)

val accept_upgrade : key:key -> Http.Response.t -> (unit, string) result
(** [accept_upgrade ~key response] finishes the upgrade request. It checks
    that:
    {ul
    {- The [response] is {!Webs.Http.Status.switching_protocols_101}}
    {- The headers satisify {!has_websocket_upgrade}.}
    {- The {!sec_webocket_accept} header has the right value as per [key]}}
    After this the underlying connection can be used to exchange
    websocket frames. *)

(** {1:upgrade_service Upgrading from the service} *)

val is_request_upgrade : Http.Request.t -> bool
(** [is_request_upgrade request] is [true] iff the headers of
    [request] satisfy {!has_websocket_upgrade}. *)

val upgrade_request :
  Http.Request.t -> (Http.Response.t, Http.Response.t) result
(** [upgrade_request request] responds to upgrade [request] to a
    websocket. *)

(** {1:header_names Headers} *)

val has_websocket_upgrade : Http.Headers.t -> bool
(** [has_websocket_upgrade headers] checks that [headers]:
    {ul
    {- Has a {!Webs.Http.Headers.connection} header with an ["upgrade"] value.}
    {- Has a {!Webs.Http.Headers.upgrade} with a ["websocket"] value.}} *)

val accept_header_value_of_key : key -> string
(** [accept_header_value_of_key k] is a value for the {!sec_websocket_accept}
    header for a key [k] value of a {!sec_websocket_key} header. *)

val add_request_upgrade_headers :
  ?key:key -> Http.Headers.t -> key * Http.Headers.t
(** [add_request_upgrade_headers ~key headers] adds headers for a Webosocket
    upgrade using [key] (defaults to {!random_key}) to [headers] and returns
    [key]. *)

(** {2:names Names} *)

val sec_websocket_accept : Http.Headers.Name.t
(** {{:https://www.rfc-editor.org/rfc/rfc6455#section-11.3.3}
    [sec-websocket-accept]} *)

val sec_websocket_extensions : Http.Headers.Name.t
(** {{:https://www.rfc-editor.org/rfc/rfc6455#section-11.3.2}
    [sec-websocket-extensions]} *)

val sec_websocket_key : Http.Headers.Name.t
(** {{:https://www.rfc-editor.org/rfc/rfc6455#section-11.3.1}
    [sec-websocket-key]} *)

val sec_websocket_protocol : Http.Headers.Name.t
(** {{:https://www.rfc-editor.org/rfc/rfc6455#section-11.3.4}
    [sec-websocket-protocol]} *)

val sec_websocket_version : Http.Headers.Name.t
(** {{:https://www.rfc-editor.org/rfc/rfc6455#section-11.3.5}
    [sec-websocket-version]} *)
