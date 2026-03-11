(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Service from the quick start *)

open Webs

let service = Http.Request.echo
let main () = Webs_quick.serve service
let () = if !Sys.interactive then () else exit (main ())
