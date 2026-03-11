(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Uses a client stored session for login.

   Sessions expire whenever the service shutdowns since the private
   authentication key for authenticating session cookies is not persisted
   and regenerated when the service starts.

   This code uses the default Webs cookie parameters, this means that
   the cookie will only be sent by clients to the server over https or
   over http to localhost. *)

open Webs
let ( let* ) = Result.bind

(* Pages *)

let escape_html s = s (* WARNING you need something sensitive here *)

let page ~title ~body = Printf.sprintf
{|<!DOCTYPE html>
<html lang="en">
<head><meta charset="utf-8"><title>%s</title></head>
<body><h1>%s</h1>%s</body>
</html>
|} (escape_html title) (escape_html title) body

let homepage =
  let title = "Welcome" in
  let body = "<p>Go to the <a href=\"restricted\">restricted area</a></p>" in
  page ~title ~body

(* FIXME we need a form here to make a POST *)
let restrictedpage user =
  let title = "The restricted area!" in
  let body = Printf.sprintf
      "<p>You are %s. So interesting ! <a href=\"logout\">Click here</a> \
       to logout.</p>" (escape_html user)
  in
  page ~title ~body

let loginpage msg =
  let title = "Login to the restricted area" in
  let body = Printf.sprintf
{|<p>%s</p>
     <form method="POST" action="/login" >
  <label>Email</label>
  <input name="email" type="email" value="user@example.org" required/>
  <label>Password</label>
  <input name="password" type="password" value="hihi" required/>
  <input type="submit" value="Login" />
</form>|} (escape_html msg)
in
page ~title ~body

(* User credentials *)

module User = struct

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
          Webs_hash.Sha_256.pbkdf2_hmac
            ~key_length ~iterations ~password ~salt ()
        in
        if Webs_hash.Sha_256.equal_key key password_key
        then Ok ()
        else Error `Wrong_password

  let read_users ~file () =
    (* In practice should read users from persistent storage *)
    Ok [ "ping", make_password "pong";
         "pang", make_password "poum" ]


  let check email pass = match email, pass with
  | Some "user@example.org", Some "hihi" -> true
  | _, _ -> false
end

let session ~private_key =
  Webs_session.client_stored ~private_key ~name:"webs_login" ()

let user_state =
  let encode = Fun.id and decode = Result.ok in
  Webs_session.State.make ~encode ~decode ~equal:String.equal ()

let home req =
  let* `GET = Http.Request.allow Http.Method.[get] req in
  Ok (Http.Response.html Http.Status.ok_200 homepage)

let restricted ~login user request = match user with
| None ->
    let log = "not logged" in
    let status = Http.Status.found_302 in
    let resp = Http.Request.redirect_to_path request status login ~log in
    Ok (None, resp)
| Some u ->
    let* `GET =
      Webs_session.for_error user (Http.Request.allow Http.Method.[get] request)
    in
    Ok (user, Http.Response.html Http.Status.ok_200 (restrictedpage u))

let login_user ~and_goto:goto user request =
  let redirect user ~path =
    let log = user ^ " logged" in
    let status = Http.Status.see_other_303 in
    Http.Request.redirect_to_path request status path ~log
  in
  let* m =
    Webs_session.for_error user @@
    Http.(Request.allow Method.[get;post]) request
  in
  match m with
  | `GET ->
      begin match user with
      | Some u -> Ok (user, redirect u ~path:goto)
      | None -> Ok (None, Http.Response.html Http.Status.ok_200 (loginpage ""))
      end
  | `POST ->
      begin match Http.Request.to_query request with
      | Error r ->
          (* FIXME Resp.with_body on [r] *)
          let err = "Something wrong happened. Try again." in
          Ok (None,
              Http.Response.html Http.Status.bad_request_400 (loginpage err))
      | Ok q ->
          let email = Http.Query.find_first "email" q in
          let password = Http.Query.find_first "password" q in
          match User.check email password with
          | true ->
              let u = Option.get email in
              Ok (Some u, redirect u ~path:goto)
          | false ->
              let log = "bad credentials" in
              let err = "Incorrect email or password. Try again." in
              Ok (None,
                  Http.Response.html ~log Http.Status.forbidden_403
                    (loginpage err))
      end

let logout_user ~and_goto user request =
  let* `POST =
    Webs_session.for_error None (Http.Request.allow Http.Method.[post] request)
  in
  let log = Option.map (fun u -> u ^ "logged out") user in
  let status = Http.Status.see_other_303 in
  Ok (None, Http.Request.redirect_to_path request status and_goto ?log)

let service ~private_key request =
  let serve user req =
    let user = Option.join @@ Result.to_option user (* drop sess. on error *) in
    Http.Response.result @@
    let* () = Webs_session.for_error user (Http.Request.clean_path request) in
    match Http.Request.path request with
    | [ "" ] -> Webs_session.for_result user @@ home request
    | ["restricted"] -> restricted ~login:["login"] user request
    | ["login"] -> login_user ~and_goto:["restricted"] user request
    | ["logout"] -> logout_user ~and_goto:[""] user req
    | _ -> Webs_session.for_result user (Http.Response.not_found_404 ())
  in
  Webs_session.setup user_state (session ~private_key) serve request

let main () =
  let private_key = Webs_authenticatable.Private_key.random_hs256 () in
  Webs_quick.serve ~name:"login" (service ~private_key)


let () = if !Sys.interactive then () else exit (main ())
