
open Result.Syntax
open More
open Webs
open Bytesrw


let supported_schemes = [ "ws", 80]

let webssocket_handshake ~url =
  let* peer = Url.to_endpoint ~supported_schemes url in
  let* c = Net.Connection.open' ~nonblock:false ~peer () in
  let fd = Net.Connection.fd c in
  let send = Bytesrw_unix.bytes_writer_of_fd fd in
  let recv = Bytesrw_unix.bytes_reader_of_fd fd in
  let close () = Net.Connection.close_noerr c; Ok () in
  try
    Result.map_error (fun e -> ignore (close ()); e) @@
    let* key, request = Webs_websocket.request_upgrade_of_url url in
    Webs_http11.Request.write ~eod:false send request;
    let* response = Webs_http11.Response.read recv in
    let* () = Webs_websocket.accept_upgrade ~key response in
    Ok (Bytesrw_websocket.make ~close ~send ~recv Client)
  with
  | Bytes.Stream.Error e -> ignore (close ()); Bytes.Stream.error_to_result e
  | exn -> ignore (close ()); raise exn


let recv socket =
  try
    let msg = Bytesrw_websocket.recv socket in
    Log.stdout (fun m -> m "@[<v>Got message:@,%s@]" msg);
    Ok ()
  with
  | Bytes.Stream.Error (Bytesrw_websocket.Error Shutdown, _) ->
      Log.stdout (fun m -> m "Connection closed");
    Ok ()

let main () =
  Log.if_error ~use:1 @@
  let url = "ws://localhost:8080" in
  let* socket = webssocket_handshake ~url in
  let finally () = ignore (Bytesrw_websocket.close socket) in
  Fun.protect ~finally @@ fun () ->
  Log.stdout (fun m -> m "sending!");
  let () = Bytesrw_websocket.send ~text:false socket "heyd" in
  let () = Bytesrw_websocket.send ~text:false socket "ho3" in
  let () = Bytesrw_websocket.send ~text:true socket "hi" in
  let () = Bytesrw_websocket.send ~text:false socket "h" in
  let* () = recv socket in
  let () = Bytesrw_websocket.send ~text:false socket "received!" in
  let* () = recv socket in
  Ok 0

let () = if !Sys.interactive then () else exit (main ())
