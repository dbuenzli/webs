(*---------------------------------------------------------------------------
   Copyright (c) 2023 The webs programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* GETs an URL. But nothing serious for nowadays: only fetches over http. *)

open Result.Syntax
open Webs

let error fmt = Format.kasprintf Result.error fmt
let uerror e = Error (Unix.error_message e)
let log fmt = Format.fprintf Format.err_formatter ("@[" ^^ fmt ^^ "@]@.")
let log_if_error = function
| Ok () -> 0
| Error e -> log "\x1B[31;1mError\x1B[0m: %s" e; Cmdliner.Cmd.Exit.some_error

let fetch url =
  log_if_error @@
  let* request = Http.Request.of_url `GET ~url in
  let headers = Http.Request.headers request in
  let scheme =  Http.Request.scheme request in
  let* host, port = Http.Headers.decode_host scheme headers in
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
    let send = Bytesrw_unix.bytes_writer_of_fd sock_fd in
    let recv = Bytesrw_unix.bytes_reader_of_fd sock_fd in
    Webs_http11.Request.write ~eod:true send request;
    Unix.shutdown sock_fd SHUTDOWN_SEND;
    let* response = Webs_http11.Response.read recv in
    let body = Http.Body.to_string (Http.Response.body response) in
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
