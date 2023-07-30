(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
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
  Http.Response.result @@ match Http.Request.path req with
  | [ "" | "post" | "get" as m] ->
      let* `GET = Http.Request.allow Http.Method.[get] req in
      let meth = match m with "" -> "post" | m -> m in
      Ok (Http.Response.html Http.Status.ok_200 (form meth))
  | ["submit"] ->
      let* m = Http.Request.allow Http.Method.[get; post] req in
      let* q = Http.Request.to_query req in
      let q = strf "@[<v>%a@,%a@]" Http.Method.pp m Http.Query.pp q in
      Ok (Http.Response.text Http.Status.ok_200 q)
  | _ ->
      Http.Response.not_found_404 ()

let main () = Webs_cli.quick_serve ~name:"formality" service
let () = if !Sys.interactive then () else main ()
