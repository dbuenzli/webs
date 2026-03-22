(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Queries.

    A datatype and codecs to handle the quirky queries found in
    URLs and some media types.

    @canonical Webs.Http.Query *)

(** {1:queries Queries} *)

type t
(** The type for queries as key-values maps. Both keys and values
    are properly decoded. Note that keys can map to multiple
    values. *)

val empty : t
(** [empty] is the empty key-values map. *)

val define : string -> string -> t -> t
(** [define k v q] is [q] with [k] bound only to value [v]. See also
    {!append_value}. *)

val undefine : string -> t -> t
(** [undefine k q] is [q] with [k] unbound. *)

val append_value : string -> string -> t -> t
(** [append_value k v q] is [q] with [k] bound to [find_all k q @ [v]].
    See also {!def}. *)

(** {1:lookups Lookups} *)

val find_first : string -> t -> string option
(** [find_first k q] is the value of [k]'s first binding in [q], if any. *)

val find_all : string -> t -> string list
(** [find_all k q] are all the values bound to [k] or the empty
    list if [k] is unbound. *)

val fold : (string -> string -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f q acc] folds over all the key-value bindings. For keys
    with multiple values folds over them in the same order
    as given by {!find_all}. *)

(** {1:predicates Predicates and comparisons} *)

val is_empty : t -> bool
(** [is_empty q] is true if [q] is {!empty}. *)

val mem : string -> t -> bool
(** [mem k q] is true [iff] key [k] is bound in [q]. *)

val equal : t -> t -> bool
(** [equal] tests queries for equality. For multi-valued keys, value order
    matters. Values are compared using binary equality. *)

val compare : t -> t -> int
(** [compare] is a total order on queries compatible with {!equal}. *)

(** {1:conv Converting} *)

val decode : string -> t
(** [decode s] decodes the
    {{:https://url.spec.whatwg.org/#application/x-www-form-urlencoded}
    [application/x-www-form-urlencoded]} [s] to a query. If a key
    is defined more than once, the first definition is returned by
    {!find_first} and the left-to-right order preserved by
    {!find_all}'s list. The input string is not checked for UTF-8
    validity. *)

val encode : t -> string
(** [encode q] encodes [q] to an
    {{:https://url.spec.whatwg.org/#application/x-www-form-urlencoded}
    [application/x-www-form-urlencoded]}
    string. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats queries for inspection. *)
