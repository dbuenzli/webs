(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs

let ( let*) = Result.bind

let index = {|
<!DOCTYPE html>
<html lang="en">
  <head> <meta charset="utf-8"> <title>Websocket</title></head>
  <body>
  <h1>Websocket</h1>
  <button id="close">Close connection</button>
  <ol id="msgs"></ol>
  <script>
    var but = document.getElementById ("close");
    var ol = document.getElementById ("msgs");
    var uri = new URL(window.location);
    uri.protocol = "ws"; uri.pathname = uri.pathname + "websocket";
    var sock = new WebSocket(uri);
    sock.onopen = function ()
    { console.log ("connected!"); sock.send ("Yo! Server");};
    sock.onerror = function (e) { console.log ("Error ", e); };
    sock.onmessage = function (e) {
      let li = document.createElement("li");
      li.textContent = "event: " + e.data;
      ol.appendChild(li);
    };
    but.onclick = function () { console.log ("closing!"); sock.close (); };
  </script>
  </body>
</html>
|}

let service req =
  Resp.result @@ match Req.path req with
  | [""] ->
      let* _m = Req.allow [`GET] req in
      Ok (Resp.html Http.s200_ok index)
  | ["websocket"] ->
      let* _m = Req.allow [`GET] req in
      Ok (Webs_websocket.upgrade req)
  | _ ->
      Ok (Resp.v Http.s404_not_found)

let main () = Webs_cli.quick_serve ~name:"websocket" service
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
