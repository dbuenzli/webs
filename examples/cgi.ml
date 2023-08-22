(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Uses the CGI connector to serve the echo service. *)

open Webs

let service = Http.Request.echo

let main () =
  let c = Webs_cgi.make ~extra_vars:["SERVER_SOFTWARE"] () in
  match Webs_cgi.serve c service with
  | Ok () -> 0
  | Error e -> prerr_endline ("Error: " ^ e); 1

let () = if !Sys.interactive then () else exit (main ())
