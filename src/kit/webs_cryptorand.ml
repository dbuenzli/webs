(*---------------------------------------------------------------------------
   Copyright (c) 2024 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

external _getrandom : Bytes.t -> unit = "ocaml_webs_getrandom"
external _getentropy : Bytes.t -> unit = "ocaml_webs_getentropy"

let get_random n =
  let b = Bytes.create n in _getrandom b; Bytes.unsafe_to_string b

let get_entropy n =
  if n > 256 (* This is the minimal value mandated by POSIX *)
  then invalid_arg (Printf.sprintf "Requested bytes (%d) exceeds 256" n)
  else let b = Bytes.create n in _getentropy b; Bytes.unsafe_to_string b
