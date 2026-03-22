(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** HTTP URL schemes.

    @canonical Webs.Http.Scheme *)

(** {1:schemes Schemes} *)

type t =
| Http
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-http-uri-scheme}
    http URL scheme}. *)
| Https
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-https-uri-scheme}
    https URL scheme}. *)
(** The type for
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-http-related-uri-schemes}
    HTTP URL schemes}. *)

val tcp_port : t -> int
(** [tcp_port s] is [80] for [Http] and 443 for [Https]. *)

(** {1:converting Converting} *)

val decode_of_url : Webs__url.t -> (t, string) result
(** [decode_of_url url] decodes an HTTP URL scheme from [url]. *)

val encode : t -> string
(** [encode s] encodes [s] to the corresponding URL scheme. *)

(** {1:predicates Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal] tests schemes for equality. *)

val compare : t -> t -> int
(** [compare] is a total order on schemes compatible with {!equal}. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats schemes for inspection. *)
