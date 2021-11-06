(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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
| [] | [""] -> Ok (Http.Resp.html Http.ok_200 (Page.admin user))
| _ -> Http.Resp.not_found_404 ()

let service req =
  Http.Resp.result @@ match Http.Req.path req with
  | [""] ->
      let* `GET = Http.Req.allow Http.Meth.[get] req in
      Ok (Http.Resp.html Http.ok_200 Page.home)
  | "admin" :: p ->
      let* user, req = Basic_auth.enticate ~check ~realm:"Service admin" req in
      let* `GET = Http.Req.allow Http.Meth.[get] req in
      admin p req user
  | _ -> Http.Resp.not_found_404 ()

let main () = Webs_cli.quick_serve ~name:"bauth" service
let () = if !Sys.interactive then () else main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
