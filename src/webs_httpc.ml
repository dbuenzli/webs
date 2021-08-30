(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs

let strf = Printf.sprintf
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

(* Servers *)

let default_max_connections = 100

type t =
  { mutable serving : bool;
    service_path : Http.path;
    max_connections : int;
    max_req_headers_byte_size : int;
    max_req_body_byte_size : int;
    listener : Webs_unix.listener;
    log : Connector.log_msg -> unit; }

let service_path c = c.service_path
let max_connections c = c.max_connections
let listener c = c.listener

let create
    ?(log = Connector.default_log ~trace:true ())
    ?(service_path = [""])
    ?(max_connections = default_max_connections)
    ?(max_req_headers_byte_size = 64 * 1024)
    ?(max_req_body_byte_size = 10 * 1024 * 1024)
    ?(listener = Webs_unix.listener_localhost) ()
  =
  { serving = false; service_path; max_connections; max_req_headers_byte_size;
    max_req_body_byte_size; listener; log }

(* Requests *)

let read_clrfs s ~max_bytes buf fd =
  let crlfs = ref [] and crlfcrlf = ref 0 in
  let i = ref 0 and free = ref max_bytes in
  (try
    while (!free > 0) do
      match Webs_unix.Connector.read fd buf !i (Bytes.length buf - !i) with
      | 0 -> raise Exit
      | c ->
          let j = !i in
          i := !i + c; free := !free - c;
          for k = j to j + c - 1 do
            let prev = k - 1 in
            if k > 0 && Bytes.get buf prev = '\r' && Bytes.get buf k = '\n'
            then match !crlfs with
            | last :: _ when last + 2 = prev -> crlfcrlf := last; raise Exit
            | crlfs' -> crlfs := prev :: crlfs'
          done
    done
  with Exit -> ());
  if !crlfcrlf = 0 || !crlfs = [] then failwith "" else
  let body_start = !crlfcrlf + 4 in
  let body_start_len = !i - body_start in
  List.rev !crlfs, body_start, body_start_len

let decode_request_line buf crlf =
  Http.Private.decode_request_line buf ~first:0 ~crlf

let decode_headers buf crlfs =
  let rec loop acc buf last_crlf = function
  | [] -> acc
  | crlf :: crlfs ->
      let first = last_crlf + 2 in
      let name, value = Http.Private.decode_header_field buf ~first ~crlf in
      let acc =
        (* This looks ok according to RFC 7230 3.2.2 *)
        if Http.Name.equal name Http.H.set_cookie
        then Http.H.add_set_cookie value acc
        else Http.H.add name value acc
      in
      loop acc buf crlf crlfs
  in
  loop Http.H.empty buf (List.hd crlfs) (List.tl crlfs)

let body_length hs = match Http.H.request_body_length hs with
| Error e -> Error (`Malformed e)
| Ok (`Length l) -> Ok (Some l)
| Ok `Chunked -> Error (`Not_implemented "chunked bodies") (* TODO *)

let read_req c fd =
  try
    let service_path = c.service_path in
    let max_bytes = c.max_req_headers_byte_size in
    let buf = Bytes.create (max io_buffer_size max_bytes) in
    let crlfs, first_start, first_len = read_clrfs c ~max_bytes buf fd in
    let meth, target, version = decode_request_line buf (List.hd crlfs) in
    let hs = decode_headers buf crlfs in
    let max_req_body_byte_size = c.max_req_body_byte_size in
    let* body_length = body_length hs in
    let body =
      Webs_unix.Connector.req_body_reader
        ~max_req_body_byte_size ~body_length fd buf ~first_start ~first_len
    in
    Ok (Req.v ~service_path ~version meth target ~headers:hs ~body_length ~body)
  with
  | Failure e -> Error (`Malformed e)

(* Responses *)

let write_resp c fd resp =
  let resp, write_body = Webs_unix.Connector.resp_body_writer resp in
  let version = Resp.version resp and st = Resp.status resp in
  (* TODO check what to do with the connection in case of upgrade *)
  let hs = Http.H.(Resp.headers resp |> def_if_undef connection "close") in
  let r = Resp.reason resp in
  let sec = Http.Private.encode_resp_header_section version st r hs in
  let sec = Bytes.unsafe_of_string sec in
  try
    Webs_unix.Connector.write fd sec 0 (Bytes.length sec);
    write_body fd;
  with
  | Unix.Unix_error (Unix.EPIPE, _, _) -> () (* disconnect TODO log ? *)

(* Serving *)

let resp_of_error e =
  let reason e = if e = "" then None else Some e in
  match e with
  | `Service -> Resp.v Http.server_error_500
  | `Too_large -> Resp.v Http.payload_too_large_413
  | `Malformed e -> Resp.v Http.bad_request_400 ?reason:(reason e)
  | `Not_implemented e -> Resp.v Http.not_implemented_501 ?reason:(reason e)

let apply_service c service req =
  try
    let resp = service req in
    Ok resp
  with
  | e ->
      let bt = Printexc.get_raw_backtrace () in
      match e with
      | Out_of_memory as e -> Printexc.raise_with_backtrace e bt
      | Sys.Break as e -> Printexc.raise_with_backtrace e bt
      | Stack_overflow as e | e -> c.log (`Service_exn (e, bt)); Error `Service

let serve_req c fd service =
  try
    let dur = Webs_unix.Time.counter () in
    let req = read_req c fd in
    let resp = Result.bind req (apply_service c service) in
    let resp = Result.fold ~ok:Fun.id ~error:resp_of_error resp in
    write_resp c fd resp;
    shutdown_noerr fd Unix.SHUTDOWN_ALL;
    let dur = Webs_unix.(Time.Span.to_uint64_ns @@ Time.count dur) in
    c.log (`Trace (dur, Result.to_option req, Some resp));
  with
  | e ->
      (* apply_service catches some of exns and turns them into 500.
         If we are here we started writing the response and are beyond
         being able to respond with a 500. *)
      let bt = Printexc.get_raw_backtrace () in
      match e with
      | Out_of_memory as e -> Printexc.raise_with_backtrace e bt
      | Sys.Break as e -> Printexc.raise_with_backtrace e bt
      | Unix.Unix_error (Unix.ECONNRESET, _, _) -> c.log `Connection_reset
      | Stack_overflow as e | e -> c.log (`Connector_exn (e, bt))

let stop c = c.serving <- false

let set_stop_sig ~handle_stop_sigs c s =
  let stop_sig _ = stop c in
  if not handle_stop_sigs then Ok None else
  Result.map Option.some (set_signal s Sys.(Signal_handle stop_sig))

let restore_stop_sig_noerr s = function
| None -> () | Some b -> set_signal_noerr s b

let serve ?(handle_stop_sigs = true) c service =
  if c.serving then Ok () else
  let () = c.serving <- true in
  let* sigpipe_behaviour = set_signal Sys.sigpipe Sys.Signal_ignore in
  let* sigint_behaviour = set_stop_sig ~handle_stop_sigs c Sys.sigint in
  let* sigterm_behaviour = set_stop_sig ~handle_stop_sigs c Sys.sigterm in
  let* accept_fd, close = Webs_unix.fd_of_listener c.listener in
  let tpool = Webs_tpool.create c.max_connections in
  let sem = Semaphore.Counting.make c.max_connections in
  let finally () =
    Webs_tpool.finish tpool;
    if close then close_noerr accept_fd;
    set_signal_noerr Sys.sigpipe sigpipe_behaviour;
    restore_stop_sig_noerr Sys.sigint sigint_behaviour;
    restore_stop_sig_noerr Sys.sigterm sigterm_behaviour;
  in
  Fun.protect ~finally @@ fun () ->
  let* () = listen_on_fd accept_fd ~backlog:128 (* SOMAXCONN *) in
  let rec loop c =
    if not c.serving then Ok () else
    begin
      Semaphore.Counting.acquire sem;
      if not c.serving then (Semaphore.Counting.release sem; Ok ()) else
      match Unix.accept ~cloexec:false accept_fd with
      | exception Unix.Unix_error (e, _, _) ->
          Semaphore.Counting.release sem;
          if e = Unix.EINTR then loop c else uerror e
      | fd, _addr ->
          let finally () = Semaphore.Counting.release sem; close_noerr fd in
          let w () = Fun.protect ~finally @@ fun () -> serve_req c fd service in
          Webs_tpool.exec tpool w; loop c
    end
  in
  loop c

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
