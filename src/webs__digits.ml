(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let err_digits_neg d = Webs__base.Fmt.str "negative number (%d)" d
let err_digits_char c = Webs__base.Fmt.str "%C is not a digit" c
let err_digits_overflow = "sequence of digits overflows"

let decode s =
  (* https://www.rfc-editor.org/rfc/rfc5234#appendix-B.1 *)
  if s = "" then Error Webs__base.err_empty_string else
  let rec loop k acc max =
    if k > max then Ok acc else
    let c = s.[k] in
    if not (Webs__base.is_digit c) then Error (err_digits_char c) else
    let acc = acc * 10 + Webs__base.digit_to_int c in
    if acc < 0 then Error err_digits_overflow else
    loop (k + 1) acc max
  in
  loop 0 0 (String.length s - 1)

let encode n =
  if n < 0 then invalid_arg (err_digits_neg n) else string_of_int n
