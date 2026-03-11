(*---------------------------------------------------------------------------
   Copyright (c) 2023 The webs programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Serves a webpage. A bit more efficiently than [webpage.ml] by using
   etags to avoid sending asset bodies on each browser reload.

   Note that file serves done via [unix_send_file.ml] and
   [gateway_send_file.ml] do perform the etag logic (and the range
   request logic). *)

open Webs

let ( let* ) = Result.bind

let css_version = "v1" (* Change that when the CSS changes *)
let css = "h1 { color: #1a7b1a }"
let css_href = "style.css"
let css_response request =
  let etag = Http.Etag.make ~weak:false css_version in
  let headers = Http.Headers.empty in
  let* headers = Http.Request.eval_if_none_match request etag ~headers in
  let content_type = Media_type.text_css in
  Ok (Http.Response.content ~headers ~content_type Http.Status.ok_200 css)

let html = Printf.sprintf {|<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"><title>Webpage</title>
<link rel="stylesheet" type="text/css" href="%s">
</head>
<body><h1>Hello!</h1></body></html>|} css_href

let html_response request =
  let* `GET = Http.Request.allow Http.Method.[get] request in
  Ok (Http.Response.html Http.Status.ok_200 html)

let service request =
  Http.Response.result @@ match Http.Request.path request with
  | [""] -> html_response request
  | [seg] when seg = css_href -> css_response request
  | _ -> Http.Response.not_found_404 ()

let main () = Webs_quick.serve service
let () = if !Sys.interactive then () else exit (main ())
