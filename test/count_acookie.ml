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

let key = Authenticatable.random_key () (* expires on restart *)
let cookie_name = "count"

let get_expirable_count ~key ~now req =
  Option.value ~default:0 @@ Option.join @@ Option.map int_of_string_opt @@
  Authenticated_cookie.get ~key ~now ~name:cookie_name req

let set_expirable_count ~key ~now ~count r =
  let expire = Some (now + 10) in
  let data = string_of_int count in
  Authenticated_cookie.set ~key ~expire ~name:cookie_name data r

let service req =
  Resp.result @@ match Req.path req with
  | [""] ->
      let* req = Req.allow [`GET] req in
      let now = truncate (Unix.gettimeofday ()) in
      let c = get_expirable_count ~key ~now req in
      let resp = Resp.html Http.s200_ok (count c) in
      Ok (set_expirable_count ~key ~now ~count:(c + 1) resp)
  | _ ->
      Error (Resp.v Http.s404_not_found)

let main () = Webs_cli.quick_serve ~name:"count_acookie" service
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
