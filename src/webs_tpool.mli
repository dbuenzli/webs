(*---------------------------------------------------------------------------
   Copyright (c) 2021 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Thread pools. *)

type t
(** The type for thread pools. *)

val create : int -> t
(** [create n] is a tread pool with at most [n] threads. *)

val exec : t -> (unit -> unit) -> unit
(** [exec p f k].

    {b Warning.} There's no protection against spurious exns in [f]. *)

val finish : t -> unit
(** [finish p] blocks until all tasks are done and destroys the pool. *)
