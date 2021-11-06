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
  let c' = match (Http.Req.query req) with
  | Some "next" -> c + 1
  | Some "prev" -> c - 1
  | Some _ | None -> c
  in
  Some c', Http.Resp.html Http.ok_200 (count c')

let service ~private_key req =
  Http.Resp.result @@ match Http.Req.path req with
  | [""] ->
      let* `GET = Http.Req.allow Http.Meth.[get] req in
      Ok (Session.setup state (session ~private_key) count req)
  | _ ->
      Http.Resp.not_found_404 ()

let main () =
  let private_key = Authenticatable.random_private_key_hs256 () in
  Webs_cli.quick_serve ~name:"count_session" (service ~private_key)

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
