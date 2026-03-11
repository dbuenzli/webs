(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Uses HTTP basic authentication to restrict access to the root
   of a service.

   Try to avoid using this, see docs in [Webs_basic_auth]. If you
   still do, only use over https otherwise credentials travel in plain
   text on the network.

   Note this is a quick and dirty version doing everything you should
   not be doing regarding credentials management. See [basicauth.ml]
   for better practices.

   Have a look at [login_cookie.ml] for a login based on an
   authenticated cookie. *)

open Webs
let ( let* ) = Result.bind

let homepage =
{|<!DOCTYPE html>
<html lang="en">
<head><meta charset="utf-8"><title>Sloppy basic authenthication</title></head>
<body><h1>Hello</h1>That was sloppily protected.</body>
</html>
|}

let sloppy_restrict request =
  let sloppy_check ~username:_ ~password =
    (* DON'T DO THAT. Hash your passwords and compare them with constant time
       comparison functions. See [basicauth.ml]. *)
    if String.equal password "1234" then Ok () else Error `Wrong_password
  in
  let realm = "Pre-release site" and check = sloppy_check in
  let* _username = Webs_basic_auth.enticate ~check ~realm request in
  Ok ()

let service request =
  Http.Response.result @@
  let* () = sloppy_restrict request in
  match Http.Request.path request with
  | [""] ->
      let* `GET = Http.Request.allow Http.Method.[get] request in
      Ok (Http.Response.html Http.Status.ok_200 homepage)
  | _ ->
      Http.Response.not_found_404 ()

let main () = Webs_quick.serve service
let () = if !Sys.interactive then () else exit (main ())
