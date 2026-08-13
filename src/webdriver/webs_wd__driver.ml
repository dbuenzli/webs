(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Drivers *)

open Result.Syntax
open More
open Webs
open Bytesrw

(* Commonalities *)

let kill_and_reap pid =
  (* Is there a way to cleanly shutdown the driver ? *)
  let kill = Os.Cmd.kill pid Sys.sigterm in
  let reap = Os.Cmd.spawn_wait_status pid in
  match kill, reap with
  | Ok (), Ok _ -> Ok ()
  | (Error _ as e), Ok _ | Ok _, (Error _ as e) -> e
  | Error e0, Error e1 -> Error (String.concat "\n" [e0; e1])

let websocket_handshake ~max_recv_message_byte_size ~url =
  let connect ~endpoint =
    let timeout = Mtime.Span.(5 * s) in
    let* () = Os.Socket.endpoint_wait_connectable ~timeout endpoint in
    let* fd, close, _addr =
      Os.Socket.connect_endpoint ~nonblock:false endpoint SOCK_STREAM
    in
    let send = Bytesrw_unix.bytes_writer_of_fd fd in
    let recv = Bytesrw_unix.bytes_reader_of_fd fd in
    Ok (fd, close, send, recv)
  in
  let supported_schemes =
    (* We should provide a way to support wss but that pulls-in
       quite a few more deps. *)
    ["http", 80; "ws", 80]
  in
  let* endpoint = Url.to_endpoint ~supported_schemes url in
  let* key, request = Webs_websocket.request_upgrade_of_url url in
  let* fd, close, send, recv = connect ~endpoint in
  let close () = if close then Os.Fd.close_noerr fd; Ok () in
  try
    Result.map_error failwith @@
    let () = Webs_http11.Request.write ~eod:false send request in
    let* response = Webs_http11.Response.read recv in
    let* () = Webs_websocket.accept_upgrade ~key response in
    Ok (Bytesrw_websocket.make
          ~max_recv_message_byte_size ~close ~send ~recv Client)
  with
  | Failure e -> ignore (close ()); Error e
  | Bytes.Stream.Error e -> ignore (close ()); Bytes.Stream.error_to_result e
  | exn ->
      let bt = Printexc.get_raw_backtrace () in
      ignore (close ()); Printexc.raise_with_backtrace exn bt

(* Driver configuration *)

module Config = struct

  (* Tracing *)

  type protocol_tracer = [ `Send | `Recv ] -> string -> unit

  let protocol_tracer_nop _dir _json = ()
  let protocol_tracer_default dir json =
    Log.debug @@ fun m ->
    let max = 1024 in
    let trunc = String.take_first max json in
    let truncated = if String.length json <= max then "" else "…" in
    let header = match dir with `Send -> "SEND" | `Recv -> "RECV" in
    m ~header "%a%s" Fmt.lines trunc truncated

  type drop_tracer =
    [ `Event of string
    | `Response of [ `Command | `Error of string ] * int ] -> unit

  let drop_tracer_nop _ = ()
  let drop_tracer_default = function
  | `Event method' ->
      Log.info @@ fun m ->
      m ~header:"DROP" "Event %a" Fmt.code method'
  | `Response (`Command, id) ->
      Log.info @@ fun m ->
      m ~header:"DROP" "Command response for id %a" Fmt.(code' int) id
  | `Response (`Error code, id) ->
      Log.info @@ fun m ->
      m ~header:"DROP" "Error response %s for id %a" code Fmt.(code' int) id

  (* Configuration *)

  let random_port () =
    let p = String.get_uint16_be (Bytesrw_sysrandom.string 2) 0 in
    if p < 1024 then p + 1024 else p

  type t =
  { args : Cmd.t;
    drop_tracer : drop_tracer;
    headless : bool;
    port : int;
    protocol_tracer : protocol_tracer;
    websocket_max_recv_message_byte_size : int; }

  let make
      ?(args = Cmd.empty) ?(drop_tracer = drop_tracer_default)
      ?(headless = true) ?(port = random_port ())
      ?(protocol_tracer = protocol_tracer_default)
      ?(websocket_max_recv_message_byte_size = Int.max_int)
      () =
    { args; drop_tracer; headless; port; protocol_tracer;
      websocket_max_recv_message_byte_size }

  let args c = c.args
  let drop_tracer c = c.drop_tracer
  let headless c = c.headless
  let port c = c.port
  let protocol_tracer c = c.protocol_tracer
  let websocket_max_recv_message_byte_size c =
    c.websocket_max_recv_message_byte_size

  let pp = Fmt.record
      [ Fmt.field "args" args Cmd.pp;
        Fmt.field "headless" headless Fmt.bool;
        Fmt.field "port" port Fmt.int;
        Fmt.field "websocket_max_recv_message_byte_size"
          websocket_max_recv_message_byte_size Fmt.int ]
end

(* Drivers *)

module Chrome = struct
  let spawn config =
    (* XXX this doesn't work as we get a CDP connection
       it seems we still need the chromderiver binary. *)
    let find_websocket_url ~host ~port =
      (* We can likely use Webs_http11 directly *)
      let* httpc = Webs_spawn_client.(make ()) in
      let ep = `Host (host, port) and timeout = Mtime.Span.(5 * s) in
      let* () = Os.Socket.endpoint_wait_connectable ~timeout ep in
      let cdp_url = Fmt.str "http://%s:%d/json/version" host port in
      let* json = Http.Client.get httpc ~follow:true ~url:cdp_url in
      let ws = Jsont.mem "webSocketDebuggerUrl" Jsont.string in
      Jsont_bytesrw.decode_string ws json
    in
    let* exe = match Os.Name.get () with
    | Darwin _ ->
        Ok "/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"
    | _ -> Error "TODO"
    in
    let* tmpdir = Os.Dir.tmp () in
    let user_data_dir =
      Fmt.str "--user-data-dir=%s" (Filepath.to_string tmpdir)
    in
    let port_opt = Fmt.str "--remote-debugging-port=%d" (Config.port config) in
    let headless = Cmd.(if' (Config.headless config) (arg "--headless=new")) in
    let no_first_run = "--no-first-run" in
    let args = Cmd.(arg port_opt %% headless % user_data_dir % no_first_run) in
    let* pid = Os.Cmd.spawn Cmd.(tool exe %% args %% Config.args config) in
    let close () =
      ignore (Os.Dir.delete ~recurse:true tmpdir);
      kill_and_reap pid
    in
    let* url = find_websocket_url ~host:"localhost" ~port:(Config.port config)in
    Ok (url, close)

  let spawn config =
    let port = Config.port config in
    let exe = Cmd.tool "chromedriver" in
    let cmd = Cmd.(exe % Fmt.str "--port=%d" port %% Config.args config) in
    let* pid = Os.Cmd.spawn cmd in
    let url = Fmt.str "ws://127.0.0.1:%d/session" port in
    let close () = kill_and_reap pid in
    Ok (url, close)
end

module Firefox = struct
  let spawn config =
    let exe = match Os.Name.get () with
    | Darwin _ -> "/Applications/Firefox.app/Contents/MacOS/firefox"
    | _ -> "firefox"
    in
    let* tmpdir = Os.Dir.tmp () in
    let profile = Fmt.str "--profile=%s" (Filepath.to_string tmpdir) in
    let port_opt = Fmt.str "--remote-debugging-port=%d" (Config.port config) in
    let headless = Cmd.(if' (Config.headless config) (arg "--headless")) in
    let new_instance = "--new-instance" (* XXX doesn't work on macos *) in
    let args = Cmd.(arg port_opt %% headless % profile % new_instance) in
    let* pid = Os.Cmd.spawn Cmd.(tool exe %% args %% Config.args config) in
    let close () =
      let err = kill_and_reap pid in
      ignore (Os.Dir.delete ~recurse:true tmpdir);
      err
    in
    let url = Fmt.str "ws://127.0.0.1:%d/session" (Config.port config) in
    Ok (url, close)
end
