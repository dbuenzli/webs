(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Performs the upgrade logic for a websocket.
   No support is provided to interact with websocket for now. *)

open Webs
let ( let* ) = Result.bind

let index = {|
<!DOCTYPE html>
<html lang="en">
<head><meta charset="utf-8"> <title>Websocket</title></head>
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

let service request =
  Http.Response.result @@ match Http.Request.path request with
  | [""] ->
      let* `GET = Http.Request.allow Http.Method.[get] request in
      Ok (Http.Response.html Http.Status.ok_200 index)
  | ["websocket"] ->
      let* `GET = Http.Request.allow Http.Method.[get] request in
      Webs_bazaar.Websocket.upgrade request
  | _ ->
      Http.Response.not_found_404 ()

let main () = Webs_quick.serve service
let () = if !Sys.interactive then () else exit (main ())
