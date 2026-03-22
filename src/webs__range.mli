(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Range requests.

    Support for {{:https://www.rfc-editor.org/rfc/rfc9110#name-range-requests}
    range requests}.

    @canonical Webs.Http.Range *)

(** {1:bytes Byte ranges} *)

type bytes =
| First of int (** First given offset to last offset *)
| Last of int (** At most last given [n] bytes. *)
| Range of { first : int; (** First offset. *)
             last : int (** Last offset. *) } (** *)
(** The type for byte range specifications. Offsets are zero-based and
    bounds are inclusive. *)

val eval_bytes : length:int -> bytes -> (int * int) option
(** [eval_bytes ~length b] given a representation length [len] and byte
    range [b] returns a concrete zero-based byte range or [None] if
    the range cannot be satisfied for [length].  *)

(** {1:ranges Ranges} *)

type t =
| Bytes of bytes list (** Byte ranges. *)
| Other of string * string (** Range unit and value. *)
(** The type for ranges. *)

(** {1:converting Converting} *)

val decode : string -> (t, string) result
(** [decode s] decodes an
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-range}HTTP [Range]}
    value. *)

val encode : t -> string
(** [encode r] serializes [r] to a
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-range}HTTP [Range]}
    value. It's the client duty to make sure ranges are valid. *)

(** {1:predicates Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal] tests ranges for equality. *)

val compare : t -> t -> int
(** [compare] is a total order on ranges compatible with {!equal}. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats ranges for inspection. *)
