(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs
open Webs_kit

let ( let* ) = Result.bind
let strf = Printf.sprintf

let index_page =
{|<!DOCTYPE html>
<html lang="en">
  <head> <meta charset="utf-8"><title>Server sent events</title></head>
  <body>
  <h1>Server sent events</h1>
  <button id="close">Close connection</button>
  <p id="event-sink"></p>
  <script>
    var but = document.getElementById ("close");
    var sink = document.getElementById ("event-sink");
    var src = new EventSource("events");
    src.onopen = function () { console.log ("Connected."); };
    src.onerror = function (e) { console.log ("Error: ", e); };
    src.onmessage = function (e) { sink.textContent = e.data; };
    but.onclick = function () { console.log ("closing!"); src.close (); };
  </script>
  </body>
</html>
|}

let rec event_stream yield =
  let ptime = Float.to_int @@ Unix.gettimeofday () in
  let data = Printf.sprintf "data: The server POSIX time is %d\n\n" ptime in
  yield (Some (Bytes.unsafe_of_string data, 0 , String.length data));
  Unix.sleep 1; event_stream yield

let x_accel_buffering = Http.Name.v "x-accel-buffering" (* for nginx *)
let resp_events () =
  let hs = Http.Headers.(def x_accel_buffering "no" empty) in
  let hs = Http.Headers.(def Http.cache_control "no-cache" hs) in
  let hs = Http.Headers.(def Http.content_type "text/event-stream" hs) in
  Http.Response.v Http.Status.ok_200 ~headers:hs
    ~body:(Http.Response.stream_body event_stream)

let service req =
  Http.Response.result @@ match Http.Request.path req with
  | [""] ->
      let* `GET = Http.Request.allow Http.Method.[get] req in
      Ok (Http.Response.html Http.Status.ok_200 index_page)
  | ["events"] ->
      let* `GET = Http.Request.allow Http.Method.[get] req in
      Ok (resp_events ())
  | _ ->
      Http.Response.not_found_404 ()

let main () = Webs_cli.quick_serve ~name:"sse" service
let () = if !Sys.interactive then () else main ()
