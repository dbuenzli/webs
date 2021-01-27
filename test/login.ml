(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs
open Webs_kit

let ( let* ) = Result.bind
let strf = Format.asprintf

module Page = struct
  let index =
{|<!DOCTYPE html>
<html lang="en">
<head><meta charset="utf-8"><title>Login</title></head>
<body>
<h1>Welcome!</h1><p>Go to the <a href="restricted">restricted area</a></p>
</body>
</html>|}

  let restricted user = strf
{|<!DOCTYPE html>
<html lang="en">
<head><meta charset="utf-8"><title>Restricted</title></head>
<body>
<h1>Welcome to the restricted area!</h1>
<p>You are %s. So interesting ! <a href="logout">Click here</a> to logout.</p>
</body>
</html>|} user

  let login msg = strf
{|<!DOCTYPE html>
<html lang="en">
  <head><meta charset="utf-8"><title>Formality</title></head>
  <body>
  <h1>Login to the restricted area</h1>
  <p>%s</p>
  <form method="POST" action="/login" >
    <label>Email</label>
    <input name="email" type="email" value="user@example.org" required/>
    <label>Password</label>
    <input name="password" type="password" value="hihi" required/>
    <input type="submit" value="Login" />
   </form>
  </body>
</html>
|} msg
end

(* TODO
   * State and error don't compose well.
   * Need to solve the root of service problem see TODO.md
   * Generalize to arbirary path and redirect to it after
   * bauth.ml is nicer but it doesn't need to wrap to save the state. *)

let key = Authenticatable.random_key () (* expires on restart *)
let session = Session.with_authenticated_cookie ~key ~name:"webs_login" ()
let user = Session.State.string

let check email pass = match email, pass with
| Some "user@example.org", Some "hihi" -> true
| _, _ -> false

let restricted_serve req user = match user with
| Some u -> Ok (user, Resp.html Http.s200_ok (Page.restricted u))
| None ->
    Ok (None, Resp.redirect ~explain:"not logged" Http.s302_found "/login")

let login_user req user =
  let goto_restricted user =
    let explain = user ^ " logged" in
    Resp.redirect ~explain Http.s303_see_other "/restricted"
  in
  match user with
  | Some u -> Ok (user, goto_restricted u)
  | None ->
      match Req.meth req with
      | `GET -> Ok (None, Resp.html Http.s200_ok (Page.login ""))
      | `POST ->
          begin match Req.to_query req with
          | Error _ ->
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
      | _ -> assert false

let logout_user req user =
  let explain = match user with None -> "" | Some u -> u ^ " logged out" in
  Ok (None, Resp.redirect ~explain Http.s303_see_other "/")

let service req =
  Resp.result @@ match Req.path req with
  | [ "" ] ->
      let* req = Req.allow [`GET] req in
      Ok (Resp.html Http.s200_ok Page.index)
  | ["restricted"] ->
      let* req = Req.allow [`GET] req in
      Session.setup' user session restricted_serve req
  | ["login"] ->
      let* req = Req.allow [`GET; `POST] req in
      Session.setup' user session login_user req
  | ["logout"] ->
      let* req = Req.allow [`GET] req in
      Session.setup' user session logout_user req
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
