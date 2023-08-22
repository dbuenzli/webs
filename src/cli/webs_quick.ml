(*---------------------------------------------------------------------------
   Copyright (c) 2023 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

let ( let* ) = Result.bind

let log fmt = Format.fprintf Format.err_formatter ("@[" ^^ fmt ^^ "@]@.")
let log_if_error = function
| Ok () -> 0
| Error e -> log "\x1B[31;1mError\x1B[0m: %s" e; Cmd.Exit.some_error

let serve service listener service_path max_connections conf =
  log_if_error @@
  let* conf = conf in
  let c = Webs_http11_gateway.make ~listener ?service_path ~max_connections ()in
  let p = Webs_http11_gateway.service_path c in
  log "Listening on \x1B[1mhttp://%a%a\x1B[0m"
    Webs_listener.pp listener Webs.Http.Path.pp p;
  Webs_http11_gateway.serve c (service conf)

let conf_docroot () =
  let setup root = match Unix.realpath root with
  | dir -> log "Document root: %s" dir; Ok dir
  | exception Unix.Unix_error (e, _, _) ->
      Error (Printf.sprintf "%s: %s" root (Unix.error_message e))
  in
  let doc = "Use $(docv) as a document root for serving files." in
  let docv = "DIR" in
  let arg_info = Arg.info ["d"; "docroot"] ~doc ~docv in
  let arg = Arg.(required & opt (some string) None & arg_info) in
  Term.(const setup $ arg)

let serve' ?version ?man ?(doc = "Undocumented service") ?name ~conf service =
  let name = match name with
  | None -> Filename.basename Sys.executable_name | Some n -> n
  in
  let listener = Webs_cli.listener () in
  let service_path = Webs_cli.service_path () in
  let max_connections = Webs_cli.max_connections () in
  let cmd =
    Cmd.v (Cmd.info name ?version ~doc ?man) @@
    Term.(const (serve service) $ listener $ service_path $ max_connections $
          conf)
  in
  Cmd.eval' cmd

let serve ?version ?man ?doc ?name service =
  let conf = Term.const (Ok ()) in
  serve' ?version ?man ?doc ?name ~conf (fun () -> service)
