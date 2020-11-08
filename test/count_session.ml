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

let session = Session.with_authenticated_cookie () (* expires on restart *)
let state = Session.State.int

let count req c =
  let c = Option.value ~default:0 c in
  let c' = match (Req.query req) with
  | Some "next" -> c + 1 | Some "prev" -> c - 1 | Some _ | None -> c
  in
  Some c', Resp.html Http.s200_ok (count c')

let service req =
  Resp.result @@ match Req.path req with
  | [""] ->
      let* req = Res.allow [`GET] req in
      Ok (Session.setup state session count req)
  | _ ->
      Error (Resp.v Http.s404_not_found)

let main () = Webs_cli.quick_serve ~name:"count_session" service
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
