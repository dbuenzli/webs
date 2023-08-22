(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Examples from the docs. *)

open Webs

let service _ = Http.Response.empty Http.Status.not_found_404
let main () = Webs_quick.serve service
let () = if !Sys.interactive then () else exit (main ())
