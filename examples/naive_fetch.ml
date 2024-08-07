(*---------------------------------------------------------------------------
   Copyright (c) 2023 The webs programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* GETs an URL. But nothing serious for nowadays: only fetches over http. *)

open Webs
let ( let* ) = Result.bind

let error fmt = Format.kasprintf Result.error fmt
let uerror e = Error (Unix.error_message e)
let log fmt = Format.fprintf Format.err_formatter ("@[" ^^ fmt ^^ "@]@.")
let log_if_error = function
| Ok () -> 0
| Error e -> log "\x1B[31;1mError\x1B[0m: %s" e; Cmdliner.Cmd.Exit.some_error

let read_http11_response fd  =
  let content_length hs = match Http.Headers.request_body_length hs with
  | Error e -> Error e
  | Ok (`Length l) -> Ok (Some l)
  | Ok `Chunked -> Error ("not implemented chunked bodies") (* TODO *)
  in
  try
    let max_bytes = Http.Connector.Default.max_request_headers_byte_size in
    let max_request_body_byte_size =
      Http.Connector.Default.max_request_body_byte_size
    in
    let io_buffer_size = 65536 (* IO_BUFFER_SIZE 4.0.0 *) in
    let buf = Bytes.create (max io_buffer_size max_bytes) in
    let crlfs, first_start, first_len =
      Webs_unix.Fd.read_http11_head_crlfs ~max_bytes buf fd
    in
    let version, status, reason =
      let crlf = List.hd crlfs in
      Http.Connector.Private.decode_status_line buf ~first:0 ~crlf
    in
    let headers = Http.Connector.Private.decode_headers buf ~crlfs in
    let* content_length = content_length headers in
    let content_type =
      Http.Headers.(find ~lowervalue:true content_type) headers
    in
    let content =
      Webs_unix.Fd.body_byte_reader ~max_request_body_byte_size ~content_length
        fd buf ~first_start ~first_len
    in
    let body = Http.Body.of_byte_reader ?content_length ?content_type content in
    Ok (Http.Response.make ~version status ~reason ~headers body)
  with
  | Failure e -> Error "malformed response"


let fetch url =
  log_if_error @@
  let* request = Http.Request.of_url `GET ~url in
  let headers = Http.Request.headers request in
  let* host, port =
    Http.Headers.decode_host (Http.Request.scheme request) headers
  in
  let* addr = match Unix.gethostbyname host with
  | exception Not_found -> error "Host %s not found" host
  | exception Unix.Unix_error (e, _, _) -> uerror e
  | entry -> Ok entry.Unix.h_addr_list.(0)
  in
  try
    let sock_fd = Unix.socket PF_INET SOCK_STREAM 0 in
    let finally () = match Unix.shutdown sock_fd SHUTDOWN_ALL with
    | exception Unix.Unix_error (_, _, _) -> () | () -> ()
    in
    Fun.protect ~finally @@ fun () ->
    Unix.connect sock_fd (ADDR_INET (addr, port));
    Webs_unix.Fd.write_http11_request sock_fd request;
    Unix.shutdown sock_fd SHUTDOWN_SEND;
    let* response = read_http11_response sock_fd in
    let body = Http.Body.to_string (Http.Response.body response) in
    let body = match body with
    | Error e -> Printf.sprintf "<Error: %s>" e
    | Ok body -> body
    in
    Format.printf "@[<v>%a@,%s@]" Http.Response.pp response body;
    Ok ()
  with
  | Unix.Unix_error (e, _, _) -> uerror e

open Cmdliner

let main () =
  let url =
    let doc = "Fetch $(docv). Only the http scheme is supported." in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"URL")
  in
  let name = Filename.basename Sys.executable_name in
  let cmd = Cmd.v (Cmd.info name) Term.(const fetch $ url) in
  Cmd.eval' cmd

let () = if !Sys.interactive then () else exit (main ())
