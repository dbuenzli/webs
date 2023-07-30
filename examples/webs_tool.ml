(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs
open Webs_kit

let strf = Format.asprintf
let ( let* ) = Result.bind

(* Logging *)

let quiet_log = ref false
let no_log fmt = Format.ifprintf Format.err_formatter fmt
let log fmt = Format.fprintf Format.err_formatter ("@[" ^^ fmt ^^ "@]@.")
let log_if_error ~use = function Ok c -> c | Error e -> log "Error: %s" e; use
let log fmt = if !quiet_log then no_log fmt else log fmt
let log_docroot d =
  let d = match d with None -> "<none>" | Some d -> d in
  log "Document root: %s" d

let absolute_docroot = function
| None -> Ok None
| Some dir ->
    try Ok (Some (Webs_unix.realpath dir)) with
    | Unix.Unix_error (e, _, _) ->
        Error (strf "%s: %s" dir (Unix.error_message e))

(* Service *)

let service ~docroot ~dir_resp req =
  Http.Response.result @@ match docroot with
  | None -> Http.Response.not_found_404 ()
  | Some docroot ->
      let* `GET = Http.Request.allow Http.Method.[get] req in
      let* file = Http.Request.to_absolute_filepath ~root:docroot req in
      Webs_unix.send_file ~dir_resp req file

(* Server *)

let webs quiet listener docroot dir_index =
  quiet_log := quiet;
  log_if_error ~use:1 @@
  let* docroot = absolute_docroot docroot in
  let* dir_resp = match dir_index with
  | "/dev/null" -> Ok Webs_unix.dir_404
  | file -> Webs_unix.dir_index_file file
  in
  log_docroot docroot;
  let s = Webs_httpc.create ~listener () in
  log "Listening on http://%a" Webs_unix.pp_listener listener;
  let* () = Webs_httpc.serve s (service ~docroot ~dir_resp) in
  Ok 0

(* Command line interface *)

open Cmdliner

let listener = Webs_cli.listener ()
let docroot = Webs_cli.docroot ()
let quiet = Arg.(value & flag & info ["q";"quiet"] ~doc:"Be quiet.")
let dir_index =
  let doc = "The file to read for directory indexes. Use /dev/null to 404." in
  Arg.(value & opt string "index.html" & info ["i"; "dir-index"] ~doc)

let cmd =
  let doc = "Webs HTTP/1.1 file server" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) is a toy file HTTP/1.1 server. Use:";
    `P "   $(tname) $(b,-d /var/www/html)";
    `P "to serve the files in $(b,/var/www/html).";
    `S Manpage.s_bugs;
    `P "This program is distributed with the Webs OCaml library.
        See https://erratique.ch/software/webs for contact information."; ]
  in
  let exits =
    Cmd.Exit.info ~doc:"on indiscriminate error reported on stderr." 2 ::
    Cmd.Exit.defaults
  in
  Cmd.v (Cmd.info "webs" ~version:"%%VERSION%%" ~doc ~man ~exits)
    Term.(const webs $ quiet $ listener $ docroot $ dir_index)


let () = if !Sys.interactive then () else exit (Cmd.eval' cmd)

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
