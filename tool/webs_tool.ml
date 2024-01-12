(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs

let ( let* ) = Result.bind
let error = Format.kasprintf Result.error

(* Logging *)

let quiet_log = ref false
let no_log fmt = Format.ifprintf Format.err_formatter fmt
let log fmt = Format.fprintf Format.err_formatter ("@[" ^^ fmt ^^ "@]@.")
let log_if_error ~use = function Ok c -> c | Error e -> log "Error: %s" e; use
let log fmt = if !quiet_log then no_log fmt else log fmt
let log_docroot d =
  let d = match d with None -> "<none>" | Some d -> d in
  log "Document root: \x1B[1m%s\x1B[0m" d

let absolute_docroot = function
| None -> Ok None
| Some dir ->
    try Ok (Some (Unix.realpath dir)) with
    | Unix.Unix_error (e, _, _) -> error "%s: %s" dir (Unix.error_message e)

(* Service *)

let service ~docroot ~dir_response ~clean_urls req =
  Http.Response.result @@ match docroot with
  | None -> Http.Response.not_found_404 ()
  | Some docroot ->
      let* `GET = Http.Request.allow Http.Method.[get] req in
      let* file = Http.Request.to_absolute_filepath ~file_root:docroot req in
      let resp = Webs_fs.send_file ~dir_response req file in
      if not clean_urls then resp else
      match resp with
      | Ok _ -> resp
      | Error r ->
          if Http.Response.status r <> Http.Status.not_found_404 then resp else
          match Webs_fs.send_file ~dir_response req (file ^ ".html") with
          | Ok _ as v -> v
          | _ -> resp

(* Server *)

let serve quiet listener docroot dir_index clean_urls =
  quiet_log := quiet;
  log_if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* docroot = absolute_docroot docroot in
  let* dir_response = match dir_index with
  | "/dev/null" -> Ok Webs_fs.dir_404
  | "NUL" when Sys.win32 -> Ok Webs_fs.dir_404
  | file -> Webs_fs.dir_index_file file
  in
  log_docroot docroot;
  let s = Webs_http11_gateway.make ~listener () in
  log "Listening on \x1B[1mhttp://%a\x1B[0m" Webs_listener.pp listener;
  let service = service ~docroot ~dir_response ~clean_urls in
  let* () = Webs_http11_gateway.serve s service in
  Ok 0

(* Command line interface *)

open Cmdliner

let listener = Webs_cli.listener ()
let docroot = Webs_cli.docroot ()
let quiet = Arg.(value & flag & info ["q";"quiet"] ~doc:"Be quiet.")
let dir_index =
  let doc = "The file to read for directory indexes. Use $(b,/dev/null) \
             to 404, on Windows $(b,NUL) works too." in
  Arg.(value & opt string "index.html" & info ["i"; "dir-index"] ~doc)

let clean_urls =
  let doc = "If an URL does not exist try with URL.html" in
  Arg.(value & flag & info ["c"; "clean-urls"] ~doc)

let serve_cmd =
  let doc = "HTTP/1.1 file server" in
  let man = [
    `S Manpage.s_description;
    `P "$(iname) is a toy file HTTP/1.1 server. Use:";
    `P "   $(iname) $(b,-d /var/www/html)";
    `P "to serve the files in $(b,/var/www/html)."; ]
  in
  Cmd.v (Cmd.info "serve" ~version:"%%VERSION%%" ~doc ~man)
    Term.(const serve $ quiet $ listener $ docroot $ dir_index $ clean_urls)

let cmd =
  let doc = "HTTP tools" in
  let man =
    [ `S Manpage.s_bugs;
      `P "This program is distributed with the Webs OCaml library.
        See https://erratique.ch/software/webs for contact information."; ]
  in
  Cmd.group (Cmd.info "webs" ~version:"%%VERSION%%" ~doc ~man)
    [serve_cmd]


let () = if !Sys.interactive then () else exit (Cmd.eval' cmd)
