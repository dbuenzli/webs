(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs
open Webs_kit

let ( let* ) = Result.bind
let strf = Format.asprintf

module Page = struct
  let frame ~title ~body = strf
{|<!DOCTYPE html>
<html lang="en">
  <head><meta charset="utf-8"><title>%s</title></head>
  <body>%s</body>
</html>|} title body

  let home =
    frame ~title:"Welcome" ~body:
{|<h1>Welcome!</h1>
<p>Go to the <a href="restricted">restricted area</a></p>|}

  (* FIXME we need a form here to make a POST *)
  let restricted user =
    frame ~title:"Restricted" ~body:(strf
{|<h1>Welcome to the restricted area!</h1>
<p>You are %s. So interesting ! <a href="logout">Click here</a> to logout.</p>
|} user)

  let login msg =
    frame ~title:"Formality" ~body:(strf
{|<h1>Login to the restricted area</h1>
<p>%s</p>
<form method="POST" action="/login" >
  <label>Email</label>
  <input name="email" type="email" value="user@example.org" required/>
  <label>Password</label>
  <input name="password" type="password" value="hihi" required/>
  <input type="submit" value="Login" />
</form>|} msg)
end

module User = struct
  let check email pass = match email, pass with
  | Some "user@example.org", Some "hihi" -> true
  | _, _ -> false
end

let private_key = Authenticatable.random_private_key () (* expires on restart *)
let session =
  Session.with_authenticated_cookie ~private_key ~name:"webs_login" ()

let user_state = Session.State.string

let home req =
  let* _m = Req.Allow.(meths [get] req) in
  Ok (Resp.html Http.ok_200 Page.home)

let restricted ~login user req = match user with
| None ->
    let explain = "not logged" in
    Ok (None, Req.service_redirect ~explain Http.found_302 login req)
| Some u ->
    let* _m = Session.for_error user Req.Allow.(meths [get] req) in
    Ok (user, Resp.html Http.ok_200 (Page.restricted u))

let login_user ~and_goto:goto user req =
  let redirect user ~path =
    let explain = user ^ " logged" in
    Req.service_redirect ~explain Http.see_other_303 path req
  in
  let* m = Session.for_error user @@ Req.Allow.(meths [get; post] req) in
  match m with
  | `GET ->
      begin match user with
      | Some u -> Ok (user, redirect u ~path:goto)
      | None -> Ok (None, Resp.html Http.ok_200 (Page.login ""))
      end
  | `POST ->
      begin match Req.to_query req with
      | Error r ->
          (* FIXME Resp.with_body on [r] *)
          let err = "Something wrong happened. Try again." in
          Ok (None, Resp.html Http.bad_request_400 (Page.login err))
      | Ok q ->
          let email = Http.Query.find "email" q in
          let password = Http.Query.find "password" q in
          match User.check email password with
          | true ->
              let u = Option.get email in
              Ok (Some u, redirect u ~path:goto)
          | false ->
              let explain = "bad credentials" in
              let err = "Incorrect email or password. Try again." in
              Ok (None,
                  Resp.html ~explain Http.forbidden_403 (Page.login err))
      end


let logout_user ~and_goto user req =
  let* _m = Session.for_error None (Req.Allow.(meths [post] req)) in
  let explain = Option.map (fun u -> u ^ "logged out") user in
  Ok (None, Req.service_redirect ?explain Http.see_other_303 and_goto req)

let service req =
  let serve user req =
    Resp.result @@ match Req.path req with
    | [ "" ] -> Session.for_result user @@ home req
    | ["restricted"] -> restricted ~login:["login"] user req
    | ["login"] -> login_user ~and_goto:["restricted"] user req
    | ["logout"] -> logout_user ~and_goto:[""] user req
    | _ -> Session.for_result user (Resp.not_found_404 ())
  in
  Session.setup user_state session serve req

let main () = Webs_cli.quick_serve ~name:"login" service
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
