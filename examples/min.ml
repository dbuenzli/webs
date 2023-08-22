(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* A minimal example. *)

open Webs

let service = Http.Request.echo
let main () = Webs_quick.serve service
let () = if !Sys.interactive then () else exit (main ())
