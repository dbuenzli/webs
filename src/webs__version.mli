(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Versions.

    @canonical Webs.Http.Version *)

(** {1:versions Versions} *)

type t = int * int
(** The type for
    {{:https://www.rfc-editor.org/rfc/rfc9112#name-http-version}
    HTTP versions}. Both integers must be in the interval [\[0;9\]]. *)

(** {1:constants Constants} *)

val none : t
(** [none] is a version for when there is none. See also {!is_none}. *)

val v11 : t
(** [v11] is [(1, 1)]. *)

val v20 : t
(** [v20] is [(2, 0)]. *)

val v30 : t
(** [v30] is [(3, 0)]. *)

(** {1:predicates Predicates and comparisons} *)

val is_none : t -> bool
(** [is_none v] is [true] iff [v] is {!none}. *)

val equal : t -> t -> bool
(** [equal] tests versions for equality. *)

val compare : t -> t -> int
(** [compare] is a total order on versions compatible with {!equal}. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats versions for inspection as ["HTTP/%d.%d"]. *)
