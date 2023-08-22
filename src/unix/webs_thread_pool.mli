(*---------------------------------------------------------------------------
   Copyright (c) 2021 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Thread pools. *)

type t
(** The type for thread pools. *)

val make : int -> t
(** [make n] is a thread pool with at most [n] threads. *)

val exec : t -> (unit -> unit) -> unit
(** [exec pool task] schedules task [task] for execution on the pool.

    {b Warning.} There's no protection against spurious exceptions
    raised from [task]. *)

val finish : t -> unit
(** [finish pool] blocks until all tasks are done and destroys the pool. *)
