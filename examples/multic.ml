(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let service = Webs.Http.Request.echo
let cgi_server s = let c = Webs_cgi.create () in Webs_cgi.serve c s
let web_server s = let c = Webs_httpc.create () in Webs_httpc.serve c s
let select = function `Cgi -> cgi_server | `Web -> web_server

let main () = match (select `Cgi) service with
| Ok () -> exit 0
| Error e -> prerr_endline ("Error: " ^ e); exit 1

let () = main ()
