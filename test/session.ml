(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Simple session handler using an authenticated to store the session
   state, a counter, on the client.

   Note that the session state travels on every request so you
   should keep it reasonable small.

   Sessions expire whenever the service shutdowns since the private
   authentication key for authenticating session cookies is not
   persisted and regenerated when the service starts. *)

open Webs
let ( let* ) = Result.bind

let countpage count = Printf.sprintf
{|<!DOCTYPE html>
<html lang="en">
<head><meta charset="utf-8"><title>Session</title></head>
<body>
y  <h1>Session</h1>
  <p>The current session value is %d.</p>
  <a href="?prev">Prev.</a> <a href="?nop">Nop.</a> <a href="?next">Next.</a>
</body>
</html>
|} count

let session ~private_key =
  Webs_session.client_stored ~private_key ~name:"webs_count" ()

let state =
  let encode i = string_of_int i in
  let decode s = Option.to_result ~none:"not an int" (int_of_string_opt s)in
  Webs_session.State.make ~encode ~decode ~equal:Int.equal ()

let count c request =
  let c = Option.join @@ Result.to_option c (* drop session on errors *) in
  let c = Option.value ~default:0 c in
  let c' = match (Http.Request.query request) with
  | Some "next" -> c + 1
  | Some "prev" -> c - 1
  | Some _ | None -> c
  in
  let headers = Http.Headers.(def cache_control "no-store" empty) in
  Some c', Http.Response.html Http.Status.ok_200 ~headers (countpage c')

let service ~private_key request =
  Http.Response.result @@ match Http.Request.path request with
  | [""] ->
      let* `GET = Http.Request.allow Http.Method.[get] request in
      Ok (Webs_session.setup state (session ~private_key) count request)
  | _ ->
      Http.Response.not_found_404 ()

let main () =
  let private_key = Webs_authenticatable.Private_key.random_hs256 () in
  Webs_quick.serve (service ~private_key)

let () = if !Sys.interactive then () else exit (main ())
