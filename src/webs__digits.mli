(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Decimal digits codec.

    This encodes and decodes non-negative as found in headers values
    with OCaml [int] values (see {!max_int}). The module detects overflows
    and turns them into errors.

    @canonical Webs.Http.Digits *)

val decode : string -> (int, string) result
(** [decode s] is the non-empty sequence of
    {{:https://www.rfc-editor.org/rfc/rfc5234#appendix-B.1}decimal
    digits} [s] as a non-negative integer. *)

val encode : int -> string
(** [encode n] is the non-negative integer [n] as a sequence of
    {{:https://www.rfc-editor.org/rfc/rfc5234#appendix-B.1}decimal
    digits}

    @raise Invalid_argument if [n] is negative. *)
