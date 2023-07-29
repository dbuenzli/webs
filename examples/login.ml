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

let session ~private_key =
  Session.client_stored ~private_key ~name:"webs_login" ()

let user_state =
  Session.State.v ~eq:String.equal ~encode:Fun.id ~decode:Result.ok ()

let home req =
  let* `GET = Http.Req.allow Http.Meth.[get] req in
  Ok (Http.Resp.html Http.Status.ok_200 Page.home)

let restricted ~login user req = match user with
| None ->
    let explain = "not logged" in
    Ok (None,
        Http.Req.service_redirect ~explain Http.Status.found_302 login req)
| Some u ->
    let* `GET = Session.for_error user (Http.Req.allow Http.Meth.[get] req) in
    Ok (user, Http.Resp.html Http.Status.ok_200 (Page.restricted u))

let login_user ~and_goto:goto user req =
  let redirect user ~path =
    let explain = user ^ " logged" in
    Http.Req.service_redirect ~explain Http.Status.see_other_303 path req
  in
  let* m = Session.for_error user @@ Http.Req.allow Http.Meth.[get;post] req in
  match m with
  | `GET ->
      begin match user with
      | Some u -> Ok (user, redirect u ~path:goto)
      | None -> Ok (None, Http.Resp.html Http.Status.ok_200 (Page.login ""))
      end
  | `POST ->
      begin match Http.Req.to_query req with
      | Error r ->
          (* FIXME Resp.with_body on [r] *)
          let err = "Something wrong happened. Try again." in
          Ok (None, Http.Resp.html Http.Status.bad_request_400 (Page.login err))
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
                  Http.Resp.html ~explain Http.Status.forbidden_403
                    (Page.login err))
      end


let logout_user ~and_goto user req =
  let* `POST = Session.for_error None (Http.Req.allow Http.Meth.[post] req) in
  let explain = Option.map (fun u -> u ^ "logged out") user in
  Ok (None, Http.Req.service_redirect ?explain Http.Status.see_other_303
        and_goto req)

let service ~private_key req =
  let serve user req =
    let user = Option.join @@ Result.to_option user (* drop sess. on error *) in
    Http.Resp.result @@ match Http.Req.path req with
    | [ "" ] -> Session.for_result user @@ home req
    | ["restricted"] -> restricted ~login:["login"] user req
    | ["login"] -> login_user ~and_goto:["restricted"] user req
    | ["logout"] -> logout_user ~and_goto:[""] user req
    | _ -> Session.for_result user (Http.Resp.not_found_404 ())
  in
  Session.setup user_state (session ~private_key) serve req

let main () =
  let private_key = Authenticatable.random_private_key_hs256 () in
  Webs_cli.quick_serve ~name:"login" (service ~private_key)


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
