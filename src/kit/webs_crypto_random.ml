(*---------------------------------------------------------------------------
   Copyright (c) 2024 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = int -> string
let primitive = Atomic.make Bytesrw_sysrandom.string
let get n = (Atomic.get primitive) n
let set_primitive p = Atomic.set primitive p
