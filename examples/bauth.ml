(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs
open Webs_kit

let ( let* ) = Result.bind

module Page = struct
  let home =
{|<!DOCTYPE html>
<html lang="en">
  <head> <meta charset="utf-8"><title>Basic auth</title></head>
  <body><h1>Basic Auth</h1><a href="admin">Login</a></body>
</html>
|}

  let admin user = Printf.sprintf
{|<!DOCTYPE html>
<html lang="en">
  <head><meta charset="utf-8"><title>Admin - Basic auth</title></head>
  <body><h1>Admin</h1><p>Hello %s.</p></body>
</html>
|} user
end

(* Really, don't do that *)

let users = ["ping", "pong"; "pang", "poum"]
let check ~user ~pass = match List.assoc_opt user users with
| Some p when p = pass -> Ok ()
| Some p -> Error `Wrong_password
| None -> Error `User_unknown

let admin p req user = match p with
| [] | [""] -> Ok (Http.Response.html Http.Status.ok_200 (Page.admin user))
| _ -> Http.Response.not_found_404 ()

let service req =
  Http.Response.result @@ match Http.Request.path req with
  | [""] ->
      let* `GET = Http.Request.allow Http.Method.[get] req in
      Ok (Http.Response.html Http.Status.ok_200 Page.home)
  | "admin" :: p ->
      let* user, req = Basic_auth.enticate ~check ~realm:"Service admin" req in
      let* `GET = Http.Request.allow Http.Method.[get] req in
      admin p req user
  | _ -> Http.Response.not_found_404 ()

let main () = Webs_cli.quick_serve ~name:"bauth" service
let () = if !Sys.interactive then () else main ()
