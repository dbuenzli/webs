(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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
  let hs = Http.H.(def x_accel_buffering "no" empty) in
  let hs = Http.H.(def cache_control "no-cache" hs) in
  let hs = Http.H.(def content_type "text/event-stream" hs) in
  Resp.v Http.ok_200 ~headers:hs ~body:(Resp.stream_body event_stream)

let service req =
  Resp.result @@ match Req.path req with
  | [""] ->
      let* _m = Req.Allow.(meths [get] req) in
      Ok (Resp.html Http.ok_200 index_page)
  | ["events"] ->
      let* _m = Req.Allow.(meths [get] req) in
      Ok (resp_events ())
  | _ ->
      Resp.not_found_404 ()

let main () = Webs_cli.quick_serve ~name:"sse" service
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
