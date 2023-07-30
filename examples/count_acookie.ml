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
  <p>The current value is %d (expires in 10s).</p><a href="">Next.</a>
  </body>
</html>
|} count

let cookie_name = "count"

let get_expirable_count ~private_key ~now req =
  let now = Some now and name = cookie_name in
  match Authenticated_cookie.find ~private_key ~now ~name req with
  | Error e -> 0
  | Ok None -> 0
  | Ok (Some (_, s)) -> Option.value ~default:0 (int_of_string_opt s)

let set_expirable_count ~private_key ~now ~count r =
  let expire = Some (now + 10) in
  let data = string_of_int count in
  Authenticated_cookie.set ~private_key ~expire ~name:cookie_name data r

let service ~private_key req =
  Http.Response.result @@ match Http.Request.path req with
  | [""] ->
      let* `GET = Http.Request.allow Http.Method.[get] req in
      let now = truncate (Unix.gettimeofday ()) in
      let c = get_expirable_count ~private_key ~now req in
      let resp = Http.Response.html Http.Status.ok_200 (count c) in
      Ok (set_expirable_count ~private_key ~now ~count:(c + 1) resp)
  | _ ->
      Http.Response.not_found_404 ()

let main () =
  let private_key = Authenticatable.random_private_key_hs256 () in
  Webs_cli.quick_serve ~name:"count_acookie" (service ~private_key)

let () = if !Sys.interactive then () else main ()
