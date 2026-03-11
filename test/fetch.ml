(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Fetch from the quick start *)

open Webs
open Result.Syntax

let main () =
  Result.retract @@ Result.map_error (fun e -> prerr_endline e; 1) @@
  let url = Sys.argv.(1) in
  let* httpc = Webs_spawn_client.make () in
  let* page = Http_client.get httpc ~follow:true ~url in
  print_endline page;
  Ok 0

let () = if !Sys.interactive then () else exit (main ())
