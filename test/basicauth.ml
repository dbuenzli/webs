(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Uses HTTP basic authentication to access a protected page.

   Try to avoid using this, see docs in [Webs_basic_auth]. If you
   still do, only use over https otherwise credentials travel in plain
   text on the network.

   Have a look at [login_cookie.ml] for a login based on an
   authenticated cookie. *)

open Webs
let ( let* ) = Result.bind

(* Page generation *)

let escape_html s = s (* WARNING you need something sensitive here *)
let page ~title ~body = Printf.sprintf
{|<!DOCTYPE html>
<html lang="en">
<head><meta charset="utf-8"><title>%s</title></head>
<body><h1>%s</h1>%s</body>
</html>
|} (escape_html title) (escape_html title) body

let homepage =
  let title = "Basic authentication" in
  let body = "<a href=\"protected\">Protected</a>" in
  page ~title ~body

let protected_page ~username =
  let title = "Protected" in
  let body = Printf.sprintf "<p>Hello %s.</p>" (escape_html username) in
  page ~title ~body

(* Credentials handling *)

let make_password password =
  (* These parameters are ok in 2023. Adapt them to the year you find
     yourself reading this. *)
  let key_length = 32 and iterations = 600_000 and salt_length = 16 in
  let salt = Webs_hash.Sha_256.random_salt ~length:salt_length () in
  let key =
    Webs_hash.Sha_256.pbkdf2_hmac ~key_length ~iterations ~password ~salt ()
  in
  `Pbkdf2_hmac_sha_256 (iterations, salt, key)

let check_credentials users ~username ~password =
  (* Note, this implementation allows to time the existence of a username. *)
  match List.assoc_opt username users with
  | None -> Error `Unknown_username
  | Some (`Pbkdf2_hmac_sha_256 (iterations, salt, key)) ->
      let key_length = String.length key in
      let password_key =
        Webs_hash.Sha_256.pbkdf2_hmac ~key_length ~iterations ~password ~salt ()
      in
      if Webs_hash.Sha_256.equal_key key password_key
      then Ok ()
      else Error `Wrong_password

let read_users ~file () =
  try
    (* In practice should read users and password info from [file]. *)
    Ok [ "ping", make_password "pong";
         "pang", make_password "poum" ]
  with Sys_error e -> Http.Response.server_error_500 ~log:e ()

(* Service *)

let protected request path ~username = match path with
| [] ->
    let* `GET = Http.Request.allow Http.Method.[get] request in
    Ok (Http.Response.html Http.Status.ok_200 (protected_page ~username))
| _ -> Http.Response.not_found_404 ()

let service read_users request =
  Http.Response.result @@ match Http.Request.path request with
  | [""] ->
      let* `GET = Http.Request.allow Http.Method.[get] request in
      Ok (Http.Response.html Http.Status.ok_200 homepage)
  | "protected" :: path ->
      let* users = read_users () in
      let realm = "Protected page" and check = check_credentials users in
      let* username = Webs_basic_auth.enticate ~check ~realm request in
      protected request path ~username
  | _ ->
      Http.Response.not_found_404 ()

let main () = Webs_quick.serve (service (read_users ~file:"/dev/null"))

let () = if !Sys.interactive then () else exit (main ())
