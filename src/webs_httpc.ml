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

let rec accept fd = try Unix.accept ~cloexec:false fd with
| Unix.Unix_error (Unix.EINTR, _, _) -> accept fd

let rec listen_on_fd fd ~backlog =
  try Unix.clear_nonblock fd; Unix.listen fd backlog; Ok () with
  | Unix.Unix_error (Unix.EINTR, _, _) -> listen_on_fd fd ~backlog
  | Unix.Unix_error (e, _, _) -> uerror e

let set_signal sg sg_behaviour = try Ok (Sys.signal sg sg_behaviour) with
| Sys_error e -> Error e

let set_signal_noerr sg sg_behaviour = try Sys.set_signal sg sg_behaviour with
| Sys_error _ -> ()

(* Servers *)

type t =
  { mutable serving : bool;
    max_connections : int;
    max_req_headers_byte_size : int;
    max_req_body_byte_size : int;
    listener : Webs_unix.listener;
    log : Connector.log_msg -> unit; }

let listener c = c.listener

let create
    ?(log = Connector.default_log ~trace:true ())
    ?(max_connections = 64) ?(max_req_headers_byte_size = 64 * 1024)
    ?(max_req_body_byte_size = 10 * 1024 * 1024)
    ?(listener = Webs_unix.listener_localhost) ()
  =
  { serving = false; max_connections; max_req_headers_byte_size;
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

let x_service_root = Http.Name.v "x-service-root"
let decode_service_root hs = match Http.H.find x_service_root hs with
| None -> None
| Some v ->
    match Http.Path.decode v with
    | Error e -> failwith (strf "%s: %s" (x_service_root :> string) e)
    | Ok v -> Some v

let body_length hs = match Http.H.request_body_length hs with
| Error e -> Error (`Malformed e)
| Ok (`Length l) -> Ok (Some l)
| Ok `Chunked -> Error (`Not_implemented "chunked bodies") (* TODO *)

let read_req c fd =
  try
    let max_bytes = c.max_req_headers_byte_size in
    let buf = Bytes.create (max io_buffer_size max_bytes) in
    let crlfs, first_start, first_len = read_clrfs c ~max_bytes buf fd in
    let meth, target, version = decode_request_line buf (List.hd crlfs) in
    let hs = decode_headers buf crlfs in
    let service_root = decode_service_root hs in
    let max_req_body_byte_size = c.max_req_body_byte_size in
    let* body_length = body_length hs in
    let body =
      Webs_unix.Connector.req_body_reader
        ~max_req_body_byte_size ~body_length fd buf ~first_start ~first_len
    in
    Ok (Req.v ?service_root ~version meth target ~headers:hs ~body_length ~body)
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
  | `Service -> Resp.v Http.s500_server_error
  | `Too_large -> Resp.v Http.s413_payload_too_large
  | `Malformed e -> Resp.v Http.s400_bad_request ?reason:(reason e)
  | `Not_implemented e -> Resp.v Http.s501_not_implemented ?reason:(reason e)

let apply_service c service req =
  try
    let resp = service req in
    c.log (`Trace (Some req, Some resp)); (* TODO we don't want that here *)
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
    let req = read_req c fd in
    let resp = Result.bind req (apply_service c service) in
    let resp = Result.fold ~ok:Fun.id ~error:resp_of_error resp in
    write_resp c fd resp;
    shutdown_noerr fd Unix.SHUTDOWN_ALL
  with
  | e ->
      (* apply_service catches some of exns and turns them into 500.
         If we are here we started writing the response and are beyond
         being able to respond with a 500. *)
      let bt = Printexc.get_raw_backtrace () in
      match e with
      | Out_of_memory as e -> Printexc.raise_with_backtrace e bt
      | Sys.Break as e -> Printexc.raise_with_backtrace e bt
      | Stack_overflow as e | e -> c.log (`Connector_exn (e, bt))

let serve c service =
  if c.serving then Ok () else
  let () = c.serving <- true in
  let* sigpipe_behaviour = set_signal Sys.sigpipe Sys.Signal_ignore in
  let* accept_fd, close = Webs_unix.fd_of_listener c.listener in
  let finally () =
    if close then close_noerr accept_fd;
    set_signal_noerr Sys.sigpipe sigpipe_behaviour
  in
  Fun.protect ~finally @@ fun () ->
  let* () = listen_on_fd accept_fd ~backlog:128 (* SOMAXCONN *) in
  try
    while c.serving do
      (* FIXME that won't respect max_connection, 4.12 semaphores *)
      let fd, _addr = accept accept_fd in
      let finally () = close_noerr fd in
      let work () = Fun.protect ~finally @@ fun () -> serve_req c fd service in
      ignore (Thread.create work ())
    done;
    Ok ()
  with
  | Unix.Unix_error (e, _, _) -> uerror e

let stop c = c.serving <- false

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
