(*---------------------------------------------------------------------------
   Copyright (c) 2024 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Cryptographically secure pseudorandom bytes.

    This modules provides cryptographically secure pseudorandom bytes
    by using operating system sources. It is possible to change the underlying
    primitive by side effect. *)

type t = int -> string
(** The type for cryptographically secure pseudorandom bytes generation.
    Calling this function with [n] must return [n] cryptographically
    secure random bytes without blocking (except if it is in early OS
    boot phase). *)

val get : int -> string
(** [get_random n] returns [n] cryptographically secure pseudorandom
    bytes.

    Theoretically this function should not block, except
    perhaps if you try to use it in early OS boot phase. It may raise
    an exception that must not be caught as it indicate a serious
    error condition in the system and the program should be aborted.

    Defaults to {!Bytesrw_sysrandom.string}. *)

val set_primitive : t -> unit
(** [set_primitive random] sets the function used by {!get} to [random]. *)
