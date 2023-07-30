(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)


(** Service environments {b TODO} try to do without.

    Environments are heterogenous key-value maps attached to requests and
    responses. They can be used by services and layers to store and share
    data. *)
module Env : sig

  (** {1:keys Keys} *)

  type 'a key
  (** The type for keys for whose lookup value is of type ['a]. *)

  val key : unit -> 'a key
  (** [key ()] is a new key. *)

  (** {1:env Environments} *)

  type t
  (** The type for environments. *)

  val empty : t
  (** [empty] is the empty environent. *)

  val is_empty : t -> bool
  (** [is_empty e] is [true] iff [e] is empty. *)

  val mem : 'a key -> t -> bool
  (** [mem k e] is [true] iff [k] is bound in [e]. *)

  val add : 'a key -> 'a -> t -> t
  (** [add k v e] is [e] with [k] bound to [v]. *)

  val remove : 'a key -> t -> t
  (** [remove k e] is [e] with [k] unbound. *)

  val find : 'a key -> t -> 'a option
  (** [find k e] is the value of [k]'s binding in [e], if any. *)

  val get : 'a key -> t -> 'a
  (** [get k m] is the value of [k]'s binding in [m].
      @raise Invalid_argument if [k] is not bound in [m]. *)

  val override : t -> by:t -> t
  (** [override m ~by] are the definitions of both [m] and [m'] with
      those of [~by] taking over. *)
end
