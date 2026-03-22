(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = int * int

(* Constants *)

let none = (-1, 0)
let v11 = (1, 1)
let v20 = (2, 0)
let v30 = (3, 0)

(* Predicates and comparisons *)

let equal = Repr.equal
let compare = Repr.compare
let is_none v = equal none v

(* Formatting *)

let pp ppf (maj, min as v) =
  if is_none v
  then Format.pp_print_string ppf "<none>"
  else Webs__base.Fmt.pf ppf "HTTP/%d.%d" maj min
