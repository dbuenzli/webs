(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs
open Webs_kit

let ( let* ) = Result.bind
let strf = Format.asprintf

let form meth = strf
{|<!DOCTYPE html>
<html lang="en">
  <head><meta charset="utf-8"><title>Formality</title></head>
  <body>
  <p><a href="post">Page Using POST.</a><a href="get">Page Using GET.</a></p>
  <form method="%s" action="/submit" >
    <label>Name</label>
    <input name="name" type="text" value="Accentué & échappé +"/>
    <label>Email</label>
    <input name="email" type="email" value="heyho@example.org" required/>
    <input type="submit" value="Send" />
   </form>
  </body>
</html>
|} meth

let service req =
  Http.Resp.result @@ match Http.Req.path req with
  | [ "" | "post" | "get" as m] ->
      let* `GET = Http.Req.Allow.(meths [get] req) in
      let meth = match m with "" -> "post" | m -> m in
      Ok (Http.Resp.html Http.ok_200 (form meth))
  | ["submit"] ->
      let* m = Http.Req.Allow.(meths [get; post] req) in
      let* q = Http.Req.to_query req in
      let q = strf "@[<v>%a@,%a@]" Http.Meth.pp m Http.Query.pp q in
      Ok (Http.Resp.text Http.ok_200 q)
  | _ ->
      Http.Resp.not_found_404 ()

let main () = Webs_cli.quick_serve ~name:"formality" service
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
