(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Websocket upgrades.

    {b References.}
    {ul
    {- I. Fette et al.
    {{:https://tools.ietf.org/html/rfc6455}
    {e The WebSocket Protocol}}. 2011}} *)

open Webs

(** {1:upgrade Upgrading} *)

val upgradable : Http.Headers.t -> bool
(** [upgradable hs] is [true] iff [hs] has {!Webs.Http.H.connection}
    with an ["upgrade"] and {!Webs.Http.H.upgrade} with a ["websocket"]. *)

val upgrade : Http.Request.t -> Http.Response.t
(** [upgrade] responds to upgrade the request to a websocket.

    {b FIXME} handle protocols, doc details. *)

(** {1:headers Headers} *)

val sec_websocket_accept : Http.Name.t
(** {{:https://tools.ietf.org/html/rfc6455#section-11.3.3}
    [sec-websocket-accept]} *)

val sec_websocket_extensions : Http.Name.t
(** {{:https://tools.ietf.org/html/rfc6455#section-11.3.2}
    [sec-websocket-extensions]} *)

val sec_websocket_key : Http.Name.t
(** {{:https://tools.ietf.org/html/rfc6455#section-11.3.1}
    [sec-websocket-key]} *)

val sec_websocket_protocol : Http.Name.t
(** {{:https://tools.ietf.org/html/rfc6455#section-11.3.4}
    [sec-websocket-protocol]} *)

val sec_websocket_version : Http.Name.t
(** {{:https://tools.ietf.org/html/rfc6455#section-11.3.5}
    [sec-websocket-version]} *)

(** {1:handshake Handshake} *)

val accept_key : string -> string
(** [accept_key k] is a value for the [sec-websocket-accept] header
    from the [k] value of the [sec-websocket-key] header. *)
