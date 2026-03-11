(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Uses the HTTP/1.1 connector to serve the echo service. *)

open Webs

let service = Http.Request.echo

let main () =
  let s = Webs_http11_gateway.make () in
  prerr_endline "Listening on http://localhost:8000";
  match Webs_http11_gateway.serve s service with
  | Ok () -> 0
  | Error e -> prerr_endline ("Error: " ^ e); 1

let () = if !Sys.interactive then () else exit (main ())
