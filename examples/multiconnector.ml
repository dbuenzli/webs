(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Uses multiple connectors in the same program: the CGI and the
   persistent HTTP/1.1 gateway connectors.

   Note that in general devising a persistent application process
   entails program design choices that makes it hard to use with the
   CGI model. So take this with a bag of salt. *)

open Webs

let service = Http.Request.echo

let cgi_server s = Webs_cgi.serve (Webs_cgi.make ()) s
let web_server s = Webs_http11_gateway.serve (Webs_http11_gateway.make ()) s
let select_server = function `Cgi -> cgi_server | `Web -> web_server

let connector = `Cgi (* Adjust to your wish *)

let main () = match (select_server connector) service with
| Ok () -> 0 | Error e -> prerr_endline ("Error: " ^ e); 1

let () = if !Sys.interactive then () else exit (main ())
