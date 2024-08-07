(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs

let ( let* ) = Result.bind

(* Unixies *)

let io_buffer_size = 65536 (* IO_BUFFER_SIZE 4.0.0 *)
let uerror e = Error (Unix.error_message e)
let close_noerr fd = try Unix.close fd with Unix.Unix_error (e, _, _) -> ()

let shutdown_noerr fd how = try Unix.shutdown fd how with
| Unix.Unix_error (e, _, _) -> ()

let rec listen_on_fd fd ~backlog =
  try Unix.clear_nonblock fd; Unix.listen fd backlog; Ok () with
  | Unix.Unix_error (Unix.EINTR, _, _) -> listen_on_fd fd ~backlog
  | Unix.Unix_error (e, _, _) -> uerror e

let set_signal sg sg_behaviour = try Ok (Sys.signal sg sg_behaviour) with
| Sys_error e -> Error e

let set_signal_noerr sg sg_behaviour = try Sys.set_signal sg sg_behaviour with
| Sys_error _ -> ()

let restore_signal_noerr sg = function
| None -> () | Some b -> set_signal_noerr sg b

(* Connector *)

let default_max_connections = 100

type t =
  { listener : Webs_listener.t;
    log : Http.Connector.Log.msg -> unit;
    max_connections : int;
    max_request_body_byte_size : int;
    max_request_headers_byte_size : int;
    service_path : Http.Path.t;
    mutable serving : bool; }

let make
    ?(listener = Webs_listener.localhost_8000)
    ?(log = Http.Connector.Log.default ~trace:true ())
    ?(max_connections = default_max_connections)
    ?(max_request_body_byte_size =
      Http.Connector.Default.max_request_body_byte_size)
    ?(max_request_headers_byte_size =
      Http.Connector.Default.max_request_headers_byte_size)
    ?(service_path = Webs.Http.Path.root) ()
  =
  { listener; log; max_connections; max_request_body_byte_size;
    max_request_headers_byte_size; service_path; serving = false; }

let listener c = c.listener
let log c = c.log
let max_connections c = c.max_connections
let max_request_body_byte_size c = c.max_request_body_byte_size
let max_request_headers_byte_size c = c.max_request_headers_byte_size
let service_path c = c.service_path
let serving c = c.serving

(* Responses *)

let write_http11_response fd response =
  let write_body = Webs_unix.Fd.body_writer (Http.Response.body response) in
  let status = Http.Response.status response in
  let reason = Http.Response.reason response in
  let hs = Http.Response.headers response in
  let hs = Http.Headers.for_connector hs (Http.Response.body response) in
(*  let hs = Http.Headers.(def_if_undef connection "close") hs in *)
  let head =
    Http.Connector.Private.encode_http11_response_head status ~reason hs
  in
  let head = Bytes.unsafe_of_string head and length = String.length head in
  Webs_unix.Fd.write fd head ~start:0 ~length;
  write_body fd

let send_100_continue fd =
  let r = Http.Response.make Http.Status.continue_100 Http.Body.empty in
  write_http11_response fd r (* FIXME error handling *)

(* Requests *)

let nop = Fun.const ()
let handle_expect_header headers fd content =
  (* See connector_conventions.mld for more information. *)
  match Http.Headers.find ~lowervalue:true Http.Headers.expect headers with
  | Some "100-continue" ->
      let rec first =
        ref (fun () -> send_100_continue fd; first := content; content ())
      in
      fun () -> !first ();
  | None | Some _ -> content

let read_http11_request c fd =
  let content_length hs = match Http.Headers.request_body_length hs with
  | Error e -> Error (`Malformed e)
  | Ok (`Length l) -> Ok (Some l)
  | Ok `Chunked -> Error (`Not_implemented "chunked bodies") (* TODO *)
  in
  try
    let max_bytes = c.max_request_headers_byte_size in
    let buf = Bytes.create (max io_buffer_size max_bytes) in
    let crlfs, first_start, first_len =
      Webs_unix.Fd.read_http11_head_crlfs ~max_bytes buf fd
    in
    let method', raw_path, version =
      let crlf = List.hd crlfs in
      Http.Connector.Private.decode_request_line buf ~first:0 ~crlf
    in
    let headers = Http.Connector.Private.decode_headers buf ~crlfs in
    let* content_length = content_length headers in
    let lowervalue = true in
    let content_type = Http.Headers.(find ~lowervalue content_type) headers in
    let content =
      Webs_unix.Fd.body_byte_reader
        ~max_request_body_byte_size:c.max_request_body_byte_size
        ~content_length fd buf ~first_start ~first_len
    in
    let content = handle_expect_header headers fd content in
    let body = Http.Body.of_byte_reader ?content_length ?content_type content in
    let service_path = c.service_path in
    let _request =
      Http.Request.for_service_connector
        ~service_path ~version method' ~raw_path ~headers body
    in
    let request =
      let path, query =
        match Http.Path.and_query_string_of_request_target raw_path with
        | Ok v -> v | Error e -> failwith e
      in
      let service_path, path =
        if path = [] (* "*" request line FIXME not sure it's a good idea,
                        maybe we coud fail *)
        then [], [] else
        match Http.Path.strip_prefix ~prefix:c.service_path path with
        | [] -> failwith "Cannot strip service path from requested URI"
        | path -> c.service_path, path
      in
      Http.Request.make
        ~headers ~path ~query ~service_path ~version method' ~raw_path
        body
    in
    Ok request
  with
  | Failure e -> Error (`Malformed e)

(* Serving *)

let response_of_error e =
  let reason e = if e = "" then None else Some e in
  match e with
  | `Service ->
      Http.Response.empty Http.Status.server_error_500
  | `Too_large ->
      Http.Response.empty Http.Status.content_too_large_413
  | `Malformed e ->
      Http.Response.empty Http.Status.bad_request_400 ?reason:(reason e)
  | `Not_implemented e ->
      Http.Response.empty Http.Status.not_implemented_501 ?reason:(reason e)
  | `Continue_100 ->
      Http.Response.empty Http.Status.expectation_failed_417

let apply_service c service request = try Ok (service request) with
| e ->
    let bt = Printexc.get_raw_backtrace () in
    match e with
    | Out_of_memory as e -> Printexc.raise_with_backtrace e bt
    | Sys.Break as e -> Printexc.raise_with_backtrace e bt
    | Stack_overflow as e
    | e -> c.log (`Service_exn (e, bt)); Error `Service

let serve_request c fd service =
  try
    let dur = Webs_unix.Time.counter () in
    let request = read_http11_request c fd in
    let response = Result.bind request (apply_service c service) in
    let response = Result.fold ~ok:Fun.id ~error:response_of_error response in
    let () = match write_http11_response fd response with
    | exception Unix.Unix_error (Unix.EPIPE, _, _) ->
        () (* disconnect TODO log ? *)
    | () -> ()
    in
    shutdown_noerr fd Unix.SHUTDOWN_ALL;
    let dur = Webs_unix.(Time.Span.to_uint64_ns @@ Time.count dur) in
    c.log (`Trace (dur, Result.to_option request, Some response));
  with
  | e ->
      (* apply_service catches some of exns and turns them into 500.
         If we arey here we started writing the response and are beyond
         being able to respond with a 500. *)
      let bt = Printexc.get_raw_backtrace () in
      match e with
      | Out_of_memory as e -> Printexc.raise_with_backtrace e bt
      | Sys.Break as e -> Printexc.raise_with_backtrace e bt
      | Unix.Unix_error (Unix.ECONNRESET, _, _) -> c.log `Connection_reset
      | Stack_overflow as e | e -> c.log (`Connector_exn (e, bt))

let stop c = c.serving <- false

let set_stop_sig ~handle_stop_sigs c signal =
  if not handle_stop_sigs then Ok None else
  let stop_sig _ = stop c in
  Result.map Option.some (set_signal signal Sys.(Signal_handle stop_sig))

let serve ?(handle_stop_sigs = true) c service =
  if c.serving then Ok () else
  let () = c.serving <- true in
  let* sigpipe_behaviour = set_signal Sys.sigpipe Sys.Signal_ignore in
  let* sigint_behaviour = set_stop_sig ~handle_stop_sigs c Sys.sigint in
  let* sigterm_behaviour = set_stop_sig ~handle_stop_sigs c Sys.sigterm in
  let* accept_fd, close = Webs_listener.to_fd c.listener in
  let pool = Webs_thread_pool.make c.max_connections in
  let sem = Semaphore.Counting.make c.max_connections in
  let finally () =
    Webs_thread_pool.finish pool;
    if close then close_noerr accept_fd;
    set_signal_noerr Sys.sigpipe sigpipe_behaviour;
    restore_signal_noerr Sys.sigint sigint_behaviour;
    restore_signal_noerr Sys.sigterm sigterm_behaviour;
  in
  Fun.protect ~finally @@ fun () ->
  let* () = listen_on_fd accept_fd ~backlog:128 (* SOMAXCONN *) in
  let rec loop c =
    if not c.serving then Ok () else begin
      Semaphore.Counting.acquire sem;
      if not c.serving then (Semaphore.Counting.release sem; Ok ()) else
      match Unix.accept ~cloexec:false accept_fd with
      | exception Unix.Unix_error (e, _, _) ->
          Semaphore.Counting.release sem;
          if e = Unix.EINTR then loop c else uerror e
      | fd, _addr ->
          let finally () = Semaphore.Counting.release sem; close_noerr fd in
          let handle () = serve_request c fd service in
          let work () = Fun.protect ~finally handle in
          Webs_thread_pool.exec pool work;
          loop c
    end
  in
  loop c
