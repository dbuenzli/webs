(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs
open Webs_kit

let ( let* ) = Result.bind
let strf = Format.asprintf

let count count = strf
{|<!DOCTYPE html>
<html lang="en">
  <head><meta charset="utf-8"><title>Formality</title></head>
  <body>
  <p>The current value is %d.</p>
  <a href="?next">Next.</a> <a href="?nop">Nop.</a> <a href="?prev">Prev.</a>
  </body>
</html>
|} count

(* sessions expires on restart *)

let session ~private_key =
  Session.client_stored ~private_key ~name:"webs_count" ()

let state =
  let encode i = string_of_int i in
  let decode s = Option.to_result ~none:"not an int" (int_of_string_opt s)in
  Session.State.v ~eq:Int.equal ~encode ~decode ()

let count c req =
  let c = Option.join @@ Result.to_option c (* drop session on errors *) in
  let c = Option.value ~default:0 c in
  let c' = match (Http.Request.query req) with
  | Some "next" -> c + 1
  | Some "prev" -> c - 1
  | Some _ | None -> c
  in
  Some c', Http.Response.html Http.Status.ok_200 (count c')

let service ~private_key req =
  Http.Response.result @@ match Http.Request.path req with
  | [""] ->
      let* `GET = Http.Request.allow Http.Method.[get] req in
      Ok (Session.setup state (session ~private_key) count req)
  | _ ->
      Http.Response.not_found_404 ()

let main () =
  let private_key = Authenticatable.random_private_key_hs256 () in
  Webs_cli.quick_serve ~name:"count_session" (service ~private_key)

let () = if !Sys.interactive then () else main ()
