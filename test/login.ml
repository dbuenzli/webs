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


(* TODO
   * State and error don't compose well.
   * bauth.ml is nicer but it doesn't need to wrap to save the state. *)

let key = Authenticatable.random_key () (* expires on restart *)
let session = Session.with_authenticated_cookie ~key ~name:"webs_login" ()
let user = Session.State.string

let check email pass = match email, pass with
| Some "user@example.org", Some "hihi" -> true
| _, _ -> false

let restricted ~login req user = match user with
| None ->
    let explain = "not logged" in
    Ok (None, Req.service_redirect ~explain Http.s302_found login req)
| Some u ->
    let* _m = Session.for_error user (Req.allow [`GET] req) in
    Ok (user, Resp.html Http.s200_ok (Page.restricted u))

let login_user ~and_goto req user =
  let goto_restricted user =
    let explain = user ^ " logged" in
    Req.service_redirect ~explain Http.s303_see_other and_goto req
  in
  let* m = Session.for_error user @@ Req.allow [`GET; `POST] req in
  match m with
  | `GET ->
      begin match user with
      | Some u -> Ok (user, goto_restricted u)
      | None -> Ok (None, Resp.html Http.s200_ok (Page.login ""))
      end
  | `POST ->
      begin match Req.to_query req with
      | Error r ->
          (* FIXME Resp.with_body on [r] *)
          let err = "Something wrong happened. Try again." in
          Ok (None, Resp.html Http.s400_bad_request (Page.login err))
      | Ok q ->
          let email = Http.Query.find "email" q in
          let password = Http.Query.find "password" q in
          match check email password with
          | true ->
              let u = Option.get email in
              Ok (Some u, goto_restricted u)
          | false ->
              let explain = "bad credentials" in
              let err = "Incorrect email or password. Try again." in
              Ok (None,
                  Resp.html ~explain Http.s403_forbidden (Page.login err))
      end
  | _ -> assert false (* FIXME Req.allow typing *)

let logout_user ~and_goto req user =
  let* _m = Session.for_error None (Req.allow [`GET] req) in
  let explain = Option.map (fun u -> u ^ "logged out") user in
  Ok (None, Req.service_redirect ?explain Http.s303_see_other and_goto req)

let service req =
  Resp.result @@ match Req.path req with
  | [ "" ] ->
      let* _m = Req.allow [`GET] req in
      Ok (Resp.html Http.s200_ok Page.home)
  | ["restricted"] ->
      Session.setup' user session (restricted ~login:["login"]) req
  | ["login"] ->
      Session.setup' user session (login_user ~and_goto:["restricted"]) req
  | ["logout"] ->
      Session.setup' user session (logout_user ~and_goto:[""]) req
  | _ ->
      Error (Resp.v Http.s404_not_found)

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
