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

type crypto_random = int -> string
(** The type for cryptographically secure random bytes generation.
    Calling the function with [n] must return [n] cryptographically
    secure random bytes. *)

type key = string
(** The type for websocket keys. These are random 16 bytes
    encoded in base64 with padding. *)

val random_key : ?crypto_random:crypto_random -> unit -> key
(** [random_key ()] is a random websocket key sourced from the generator
    [crypto_random] (default to {!Webs_cryptorand.get_random}). *)

val accept_header_value_of_key : key -> string
(** [accept_header_value_of_key k] is a value for the {!sec_websocket_accept}
    header for a key [k] value of a {!sec_websocket_key} header. *)

(** {1:upgrade Upgrading} *)

val add_request_upgrade_headers :
  ?key:key -> Http.Headers.t -> key * Http.Headers.t
(** [upgrade_headers ~key headers] adds headers for a Webosocket upgrade
    using [key] (defaults to {!random_key}) to [headers] and returns
    [key]. *)

val request_upgrade_of_url :
  ?key:key -> ?headers:Http.Headers.t -> ?log:string ->
  ?version:Http.Version.t -> Url.t -> (key * Http.Request.t, string) result
(** [request_upgrade_of_url ~url] is a [`GET] request constructed like
    {!Http.Request.of_url} and with headers
    {!add_request_upgrade_headers}[ ?key headers] added. *)

val headers_upgradable : Http.Headers.t -> bool
(** [upgradable hs] is [true] iff [hs] has a header
    {!Webs.Http.Headers.connection} with an ["upgrade"] value and a
    header {!Webs.Http.Headers.upgrade} with a ["websocket"] value. *)

val request_upgradable : Http.Request.t -> bool
(** [request_upgradable r] is [true] iff [Http.Request.headers r]
    satisfies {!headers_upgradable}. *)

val upgrade_response :
  Http.Request.t -> (Http.Response.t, Http.Response.t) result
(** [upgrade_response] responds to upgrade the request to a websocket *)

(** {1:header_names Headers names} *)

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
