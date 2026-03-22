(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* This examples shows how to do server sent events (SSE).

   It's a bad example. You should not do this because:

   - Until the client closes the connection, it will persistently eat
     one connection from the limited connection pool the
     [Webs_http11_gateway] connector draws from. This may eventually
     starve other clients.
   - It relies on the connector not performing any buffering on body
     byte writers which is not guaranteed by the connector.

   To perform SSE at scale a different connector architecture is
   needed.

   Also note that this example uses the [x-accel-buffering] header
   which is specific to nginx. You may need to adapt this if you
   are using another gateway. *)

open Result.Syntax
open Webs
open Bytesrw

let homepage =
{|<!DOCTYPE html>
<html lang="en">
<head><meta charset="utf-8"><title>Server sent events</title></head>
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
     but.onclick = function () { console.log ("Closing!"); src.close (); };
  </script>
</body>
</html>
|}

let rec event_stream ~eod w =
  let ptime = Float.to_int (Unix.gettimeofday ()) in
  let data = Printf.sprintf "data: The server POSIX time is %d\n\n" ptime in
  Bytes.Writer.write_string w data;
  event_stream ~eod w

let x_accel_buffering =
  Http.Headers.Name.make "x-accel-buffering" (* for nginx *)

let respond_events () =
  let headers =
    Http.Headers.empty
    |> Http.Headers.(define x_accel_buffering) "no"
    |> Http.Headers.(define cache_control) "no-cache"
    |> Http.Headers.(define content_type) "text/event-stream"
  in
  let body = Http.Body.of_bytes_writer event_stream in
  Http.Response.make ~headers Http.Status.ok_200 body

let service request =
  Result.retract @@ match Http.Request.path request with
  | [""] ->
      let* `GET = Http.Request.allow Http.Method.[get] request in
      Ok (Http.Response.html Http.Status.ok_200 homepage)
  | ["events"] ->
      let* `GET = Http.Request.allow Http.Method.[get] request in
      (* DON'T DO THIS. This never returns until the client closes the
         connection. With the [Webs_http11_gateway] connector this
         eventually starves the connection pool. *)
      Ok (respond_events ())
  | _ -> Http.Response.not_found_404 ()

let main () = Webs_quick.serve service
let () = if !Sys.interactive then () else exit (main ())
