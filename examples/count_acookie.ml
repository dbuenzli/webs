(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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
  Resp.result @@ match Req.path req with
  | [""] ->
      let* `GET = Req.Allow.(meths [get] req) in
      let now = truncate (Unix.gettimeofday ()) in
      let c = get_expirable_count ~private_key ~now req in
      let resp = Resp.html Http.ok_200 (count c) in
      Ok (set_expirable_count ~private_key ~now ~count:(c + 1) resp)
  | _ ->
      Resp.not_found_404 ()

let main () =
  let private_key = Authenticatable.random_private_key_hs256 () in
  Webs_cli.quick_serve ~name:"count_acookie" (service ~private_key)

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
