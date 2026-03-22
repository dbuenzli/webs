(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Entity tags.

    Support for
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-conditional-requests}
    conditional requests}.

    @canonical Webs.Http.Etag *)

(** {1:etags Etags} *)

type t
(** The type for
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-etag}etags}. *)

val make : weak:bool -> string -> t
(** [make ~weak tag] is the etag [tag]. [weak] indicates if the etag is
    weak.

    {b Warning.}  The function does not check that the bytes of
    [tag] are valid; each should be one of [0x21], \[[0x23];[0x7E]\]
    or \[[0x80];[0xFF]\]. *)

val tag : t -> string
(** [tag e] is the entity tag of [e]. *)

val is_weak : t -> bool
(** [is_weak e] is [true] iff [e] is weak. *)

(** {1:matching Matching} *)

val weak_match : t -> t -> bool
(** [weak_match e0 e1] is [true] iff [e0] and [e1]
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-comparison-2}
    weakly match}. *)

val strong_match : t -> t -> bool
(** [strong_match e0 e1] is [true] iff [e0] and [e1]
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-comparison-2}
    strongly match}. *)

(** {1:conds Etag conditions} *)

type cond = [ `Any | `Etags of t list (** *) ]
(** The type for etags conditions. This represents the value
    of {!Http.Headers.if_match} or {!Http.Headers.if_none_match} headers. *)

val decode_cond : string -> (cond, string) result
(** [decode_cond s] parses an etag condition from [s]. *)

val encode_cond : cond -> string
(** [encode_cond c] serializes condition [c]. *)

val eval_if_match : cond -> t option -> bool
(** [eval_if_match c t] evaluates the logic of an
    {!Http.Headers.if_match} header condition [c] on an entity represented
    by [t] ([None] means the representation does not exist). This is:
    {ul
    {- [true] if [c] is [None] (no condition).}
    {- [true] if [t] is [Some _] and [c] is [Some `Any].}
    {- [true] if [t] is [Some etag], [c] is [Some (`Etags etags)] and
       [etag] {{!strong_match}strongly matches} one of the [etags].}
    {- [false] otherwise.}} *)

val eval_if_none_match : cond -> t option -> bool
(** [eval_if_none_match c t] evaluates the logic of an
    {!Http.Headers.if_none_match} header condition [c] on an entity
    represented by [t] ([None] means the representation does not exist).
    This is:
    {ul
    {- [true] if [t] is [None] and [c] is [Some `Any].}
    {- [true] if [t] is [Some etag], [c] is [Some (`Etags etags)] and
       [etag] {{!weak_match}weakly matches} none of the [etags]}
    {- [false] otherwise.}} *)

val eval_if_range : t -> t option -> bool
(** [eval_if_range req t] evaluates the logic of an
    {!Http.Headers.if_range} header
    etag [req] on an entity represented by [t] ([None] means the
    representation does not exist). This is:
    {ul
    {- [true] if [t] is [Some etag] and [etag] {{!strong_match}strongly
        matches} [req]}
    {- [false] otherwise.}} *)

(** {1:converting Converting} *)

val decode : string -> (t, string) result
(** [decode s] decodes an
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-etag}HTTP
    [entity-tag]} from [s]. *)

val encode : t -> string
(** [encode etag] is [etag] as an
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-etag}HTTP
    [entity-tag]}. *)

(** {1:predicates Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal] tests etags for equality. *)

val compare : t -> t -> int
(** [compare] is a total order on etags compatible with {!equal}. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats etags for inspection. *)
