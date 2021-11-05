(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Websocket upgrades.

    {b References.}
    {ul
    {- I. Fette et al.
    {{:https://tools.ietf.org/html/rfc6455}
    {e The WebSocket Protocol}}. 2011}} *)

open Webs

(** {1:upgrade Upgrading} *)

val upgradable : Http.headers -> bool
(** [upgradable hs] is [true] iff [hs] has {!Webs.Http.H.connection}
    with an ["upgrade"] and {!Webs.Http.H.upgrade} with a ["websocket"]. *)

val upgrade : Http.service
(** [upgrade] responds to upgrade the request to a websocket.

    {b FIXME} handle protocols, doc details. *)

(** {1:headers Headers} *)

val sec_websocket_accept : Http.name
(** {{:https://tools.ietf.org/html/rfc6455#section-11.3.3}
    [sec-websocket-accept]} *)

val sec_websocket_extensions : Http.name
(** {{:https://tools.ietf.org/html/rfc6455#section-11.3.2}
    [sec-websocket-extensions]} *)

val sec_websocket_key : Http.name
(** {{:https://tools.ietf.org/html/rfc6455#section-11.3.1}
    [sec-websocket-key]} *)

val sec_websocket_protocol : Http.name
(** {{:https://tools.ietf.org/html/rfc6455#section-11.3.4}
    [sec-websocket-protocol]} *)

val sec_websocket_version : Http.name
(** {{:https://tools.ietf.org/html/rfc6455#section-11.3.5}
    [sec-websocket-version]} *)

(** {1:handshake Handshake} *)

val accept_key : string -> string
(** [accept_key k] is a value for the [sec-websocket-accept] header
    from the [k] value of the [sec-websocket-key] header. *)

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
