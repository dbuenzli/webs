(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs

let service = Http.Request.echo
let main () =
  let c = Webs_cgi.create ~extra_vars:["SERVER_SOFTWARE"] () in
  match Webs_cgi.serve c service with
  | Ok () -> exit 0
  | Error e -> prerr_endline ("Error: " ^ e); exit 1

let () = if !Sys.interactive then () else main ()
