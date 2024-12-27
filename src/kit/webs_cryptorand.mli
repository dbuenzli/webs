(*---------------------------------------------------------------------------
   Copyright (c) 2024 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Cryptographically secure pseudorandom bytes and entropy.

    This modules provides cryptographically secure pseudorandom bytes
    and entropy by using operating system sources. See the invidual functions
    for details on the used C functions. *)

(** {1:random Pseudo random bytes} *)

val get_random : int -> string
(** [get_random n] returns [n] cryptographically secure pseudorandom bytes.
    Theoretically this function should not block, except perhaps if you
    try to use it in early OS boot phase.

    This uses:
    {ul
    {- {{:https://www.man7.org/linux/man-pages/man2/getrandom.2.html}
       [getrandom]} on Linux.}
    {- {{:https://man.openbsd.org/arc4random.3}[arc4random_buf]} on
       other Unixes.}
    {- {{:https://docs.microsoft.com/en-us/windows/win32/api/ntsecapi/nf-ntsecapi-rtlgenrandom}[RtlGenRandom]} on Windows
       ({{:https://bugzilla.mozilla.org/show_bug.cgi?id=504270}safe to use}
       despite the availability warning).}
    {- Raises {!Sys_error} otherwise.}}

    Raises {!Sys_error} in case of problem. If this happens do not
    try to handle the exception, log it at the toplevel of your program
    and abort the program or the server request. It likely indicates a serious
    error condition in the system. *)

(** {1:entropy Entropy} *)

val get_entropy : int -> string
(** [get_entropy n] returns [n] bytes of entropy from your operating system.
    The function {b blocks} until enough entropy is gathered. [n] must
    be smaller or equal to 256.

    This uses:
    {ul
    {- {{:https://pubs.opengroup.org/onlinepubs/9799919799/functions/getentropy.html}[getentropy]} on POSIX systems.}
    {- {{:https://docs.microsoft.com/en-us/windows/win32/api/ntsecapi/nf-ntsecapi-rtlgenrandom}[RtlGenRandom]} on Windows
       ({{:https://bugzilla.mozilla.org/show_bug.cgi?id=504270}safe to use}
       despite the availability warning).}
    {- Raises {!Sys_error} otherwise.}}

    Raises {!Sys_error} in case of problem. If this happen do not try
    to handle the exception, log it at the toplevel of your program
    and abort the program or the server request. It likely indicates a
    serious error condition in the system. *)
