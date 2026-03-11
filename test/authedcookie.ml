(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Stores an authenticated cookie with a counter on the client.

   The data of the cookie expires after 5s which resets the counter.
   Expiration happens via the data authentication, not the cookie
   max-age mechanism.

   The counter also resets whenever the service restarts since the
   private authentication key is not persisted and we simply reset the
   counter on authentication errors.

   This code uses the default Webs cookie parameters, this means that
   the cookie will only be sent by clients to the server over https or
   over http to localhost. *)

open Webs
let ( let* ) = Result.bind

let countpage ~count = Printf.sprintf
{|<!DOCTYPE html>
<html lang="en">
<head><meta charset="utf-8"><title>Formality</title></head>
<body>
  <p>The current value is %d. Expires and resets in 5s.</p>
  <a href="">Next.</a>
</body>
</html>
|} count

let cookie_name = "count"

let get_expirable_count ~private_key ~now request =
  let name = cookie_name and now = Some now and init = 0 in
  match Webs_authenticated_cookie.find ~private_key ~now ~name request with
  | Ok (Some (_, s)) -> Option.value ~default:init (int_of_string_opt s)
  | Ok None | Error _ -> init

let set_expirable_count ~private_key ~now ~count response =
  let name = cookie_name and expire = Some (now + 5) in
  let data = string_of_int count in
  Webs_authenticated_cookie.set ~private_key ~expire ~name data response

let service ~private_key request =
  Http.Response.result @@ match Http.Request.path request with
  | [""] ->
      let* `GET = Http.Request.allow Http.Method.[get] request in
      let now = truncate (Unix.gettimeofday ()) in
      let count = get_expirable_count ~private_key ~now request + 1 in
      let headers = Http.Headers.(def cache_control "no-store" empty) in
      let page = countpage ~count in
      let response = Http.Response.html Http.Status.ok_200 ~headers page in
      let response = set_expirable_count ~private_key ~now ~count response in
      Ok response
  | _ ->
      Http.Response.not_found_404 ()

let main () =
  let private_key = Webs_authenticatable.Private_key.random_hs256 () in
  Webs_quick.serve (service ~private_key)

let () = if !Sys.interactive then () else exit (main ())
