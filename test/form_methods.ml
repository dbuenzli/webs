(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Handles an HTML form action with various methods in a uniform
   way. *)

open Webs
let ( let* ) = Result.bind

let form_with ~method' = Printf.sprintf
{|<!DOCTYPE html>
<html lang="en">
<head><meta charset="utf-8"><title>Form methods</title></head>
<body>
  <h1>Form methods</h1>
  <p>This form uses %s on submit.</p>
  <form method="%s" action="/submit" >
    <label>Name:</label>
    <input name="name" type="text" value="Accentué & échappé +"/><br>
    <label>Email:</label>
    <input name="email" type="email" value="heyho@example.org" required/><br>
    <input type="submit" value="Send" />
  </form>
  <p><a href="post">Same page with form POSTing</a>.</py>
  <p><a href="get">Same page with form GETting</a>.</p>
</body>
</html>
|} (String.uppercase_ascii method') method'

let submit method' query =
  Format.asprintf "@[<v>%a@,%a@]" Http.Method.pp method' Http.Query.pp query

let service request =
  Http.Response.result @@ match Http.Request.path request with
  | [ "" | "post" | "get" as m ] ->
      let* `GET = Http.Request.allow Http.Method.[get] request in
      let method' = match m with "" -> "post" | m -> m in
      Ok (Http.Response.html Http.Status.ok_200 (form_with ~method'))
  | ["submit"] ->
      let* method' = Http.Request.allow Http.Method.[get; post] request in
      let* query = Http.Request.to_query request in
      let result = submit method' query in
      Ok (Http.Response.text Http.Status.ok_200 result)
  | _ ->
      Http.Response.not_found_404 ()

let main () = Webs_quick.serve service
let () = if !Sys.interactive then () else exit (main ())
