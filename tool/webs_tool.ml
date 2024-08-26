(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs

let ( let* ) = Result.bind
let error = Format.kasprintf Result.error

let read_file file =
  let read file ic = try Ok (In_channel.input_all ic) with
  | Sys_error e -> Error (Printf.sprintf "%s: %s" file e)
  in
  let binary_stdin () = In_channel.set_binary_mode In_channel.stdin true in
  try match file with
  | "-" -> binary_stdin (); read file In_channel.stdin
  | file -> In_channel.with_open_bin file (read file)
  with Sys_error e -> Error e

let write_file file s =
  let write file s oc = try Ok (Out_channel.output_string oc s) with
  | Sys_error e -> Error (Printf.sprintf "%s: %s" file e)
  in
  let binary_stdout () = Out_channel.(set_binary_mode stdout true) in
  try match file with
  | "-" -> binary_stdout (); write file s Out_channel.stdout
  | file -> Out_channel.with_open_bin file (write file s)
  with Sys_error e -> Error e

(* Logging *)

let st_red_bold = "\027[31;01m"
let st_reset = "\027[m"

let quiet_log = ref false
let exec = Filename.basename Sys.executable_name
let no_log fmt = Format.ifprintf Format.err_formatter fmt
let log fmt = Format.fprintf Format.err_formatter ("@[" ^^ fmt ^^ "@]@.")
let log_if_error ~use = function
| Ok c -> c
| Error e -> log "%s: @<0>%sError@<0>%s: %s" exec st_red_bold st_reset e; use

let log fmt = if !quiet_log then no_log fmt else log fmt

(* HTTP client *)

let do_request ~method' ~headers ~body ~trace ~max_redirections ~no_follow ~url
  =
  let follow = not no_follow in
  let trace = if trace then Some Webs_spawn_client.stderr_tracer else None in
  let* httpc = Webs_spawn_client.make ?trace () in
  let* body = match body with
  | None -> Ok Http.Body.empty
  | Some b -> Result.map Http.Body.of_string (read_file b)
  in
  let headers =
    let add_header acc (n, v) = Http.Headers.def n v acc in
    List.fold_left add_header Http.Headers.empty headers
  in
  let* request = Http.Request.of_url method' ~headers ~url ~body in
  Http_client.request ~max_redirections httpc ~follow request

(* HTTP service *)

let log_docroot d =
  let d = match d with None -> "<none>" | Some d -> d in
  log "Document root: \x1B[1m%s@<0>%s" d st_reset

let absolute_docroot = function
| None -> Ok None
| Some dir ->
    try Ok (Some (Unix.realpath dir)) with
    | Unix.Unix_error (e, _, _) -> error "%s: %s" dir (Unix.error_message e)

let file_service ~docroot ~dir_response ~clean_urls req =
  Http.Response.result @@ match docroot with
  | None -> Http.Response.not_found_404 ()
  | Some docroot ->
      let* `GET = Http.Request.allow Http.Method.[get] req in
      let* file = Http.Request.to_absolute_filepath ~file_root:docroot req in
      let response = Webs_fs.send_file ~dir_response req file in
      if not clean_urls then response else
      match response with
      | Ok _ -> response
      | Error r ->
          let is_404 = Http.Response.status r = Http.Status.not_found_404 in
          if not is_404 then response else
          match Webs_fs.send_file ~dir_response req (file ^ ".html") with
          | Ok _ as v -> v | _ -> response

(* Serve *)

let file_serve ~docroot ~dir_index ~clean_urls =
  let* docroot = absolute_docroot docroot in
  let* dir_response = match dir_index with
  | "/dev/null" -> Ok Webs_fs.dir_404
  | "NUL" when Sys.win32 -> Ok Webs_fs.dir_404
  | file -> Webs_fs.dir_index_file file
  in
  log_docroot docroot;
  Ok (file_service ~docroot ~dir_response ~clean_urls)

let echo_serve () =
  log "Echoing requests with plain text 404s!";
  Ok (Http.Request.echo ?status:None)

let serve ~quiet ~listener ~docroot ~dir_index ~clean_urls ~echo =
  quiet_log := quiet;
  log_if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* service = match echo with
  | true -> echo_serve ()
  | false -> file_serve ~docroot ~dir_index ~clean_urls
  in
  let s = Webs_http11_gateway.make ~listener () in
  log "Listening on \x1B[1mhttp://%a\x1B[0m" Webs_listener.pp listener;
  let* () = Webs_http11_gateway.serve s service in
  Ok 0

(* Scrape URLs *)

let scrape_urls
    ~method' ~headers ~body ~trace ~max_redirections ~no_follow ~rel ~url
  =
  log_if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* data = match Webs_url.scheme url with
  | None -> read_file url
  | Some _ ->
      let* response =
        do_request ~method' ~headers ~body ~trace ~max_redirections ~no_follow
          ~url
      in
      match Http.Response.status response with
      | 200 -> Http.Body.to_string (Http.Response.body response)
      | st -> Error (Format.asprintf "%a" Http.Status.pp st)
  in
  let urls = Webs_url.list_of_text_scrape data in
  let is_abs u = Webs_url.kind u = `Abs in
  let urls = if rel then urls else List.filter is_abs urls in
  (if urls = [] then () else List.iter print_endline urls);
  Ok 0

(* Request *)

let request
    ~method' ~headers ~body ~dump ~trace ~max_redirections ~no_follow ~outf
    ~url
  =
  log_if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* response =
    do_request ~method' ~headers ~body ~trace ~max_redirections ~no_follow ~url
  in
  let* response =
    if not dump then match Http.Response.status response with
    | 200 -> Http.Body.to_string (Http.Response.body response)
    | st -> Error (Format.asprintf "%a" Http.Status.pp st)
    else
    Http.Response.encode_http11 ~include_body:true response
  in
  let* () = write_file outf response in
  Ok 0

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let method' =
  let doc = "$(docv) is the request HTTP method." in
  let meth = Arg.conv' Http.Method.(decode, pp) in
  Arg.(value & opt meth `GET & info ["X"; "request"] ~doc ~docv:"METHOD")

let headers =
  let doc = "$(docv) of the form $(b,key: value) is added to the request's \
             headers. Repeatable."
  in
  let header = Arg.conv' Http.Headers.(decode_http11_header, pp_header) in
  Arg.(value & opt_all header [] & info ["H"; "header"] ~doc ~docv:"HEADER")

let max_redirections =
  let doc = "$(docv) is the maximal number of redirections followed." in
  Arg.(value & opt int Http_client.default_max_redirection &
       info ["max-redirections"] ~doc ~docv:"COUNT")

let no_follow =
  let doc = "Do not follow redirections." in
  Arg.(value & flag & info ["s"; "no-follow"] ~doc)

let trace =
  let doc = "Trace spawn arguments on $(b,stderr)." in
  Arg.(value & flag & info ["t"; "trace"] ~doc)

let body =
  let doc = "Read request body from $(docv). Use $(b,-) for $(b,stdin)" in
  let absent = "Empty request body" in
  Arg.(value & opt (some string) None &
       info ["b"; "body"] ~doc ~docv:"FILE" ~absent)

let request_cmd =
  let doc = "Request an URL" in
  let man = [
    `S Manpage.s_description;
    `P "The $(iname) command requests an URL using the $(b,Webs_spawn_client) \
        connector and writes the response on $(b,stdout)" ]
  in
  Cmd.v (Cmd.info "request" ~doc ~man) @@
  let+ url =
    let doc = "The URL to request." in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"URL")
  and+ outf =
    let doc = "Write response to $(docv)." in
    let absent = "$(b,stdout)" in
    Arg.(value & opt string "-" & info ["o"] ~doc ~docv:"FILE" ~absent)
  and+ dump =
    let doc = "Include response status line and headers in output" in
    Arg.(value & flag & info ["i"; "include"] ~doc)
  and+ method' and+ headers and+ max_redirections and+ trace and+ no_follow
  and+ body in
  request
    ~method' ~headers ~body ~dump ~trace ~max_redirections ~no_follow ~outf ~url

let serve_cmd =
  let doc = "HTTP/1.1 file server" in
  let man = [
    `S Manpage.s_description;
    `P "The $(iname) command is a file HTTP/1.1 server. For example:";
    `Pre "$(iname) $(b,-d /var/www/html)"; `Noblank;
    `Pre "$(iname) $(b,--echo)   # Only echo requests";
    `P "$(b,Warning.) Do not connect to the internet, use on \
        localhost or behind an HTTP gateway, the connector has no HTTPs \
        support or mitigations against slow HTTP attacks."; ]
  in
  Cmd.v (Cmd.info "serve" ~doc ~man) @@
  let+ listener = Webs_cli.listener ()
  and+ docroot = Webs_cli.docroot ()
  and+ quiet = Arg.(value & flag & info ["q"; "quiet"] ~doc:"Be quiet.")
  and+ dir_index =
    let doc = "The file to read for directory indexes. Use $(b,/dev/null) \
               to 404, on Windows $(b,NUL) works too."
    in
    Arg.(value & opt string "index.html" & info ["i"; "dir-index"] ~doc)
  and+ clean_urls =
    let doc = "If a requested URL does not exist try with URL.html" in
    Arg.(value & flag & info ["c"; "clean-urls"] ~doc)
  and+ echo =
    let doc =
      "Do not serve files. Echo back requests as plain/text 404 for inspection."
    in
    Arg.(value & flag & info ["echo"] ~doc)
  in
  serve ~quiet ~listener ~docroot ~dir_index ~clean_urls ~echo

let scrape_urls_cmd =
  let doc = "Scrape URLs or text" in
  let man = [
    `S Manpage.s_description;
    `P "The $(iname) command scrapes URLs from a given URL, text file or \
        $(b,stdin). It assumes the text is in an US-ASCII compatible \
        encoding like UTF-8. For example:";
    `Pre "$(iname) $(b,https://example.org)"; `Noblank;
    `Pre "$(iname) $(b,README.md)"; ]
  in
  Cmd.v (Cmd.info "scrape-urls" ~doc ~man) @@
  let+ url =
    let doc = "The URL or text file to scrape. Use $(b,-) for $(b,stdin)."in
    Arg.(value & pos 0 string "-" & info [] ~doc ~docv:"URL|FILE")
  and+ rel =
    let doc = "Also output relative URLs." in
    Arg.(value & flag & info ["a"; "include-relative"] ~doc)
  and+ method' and+ headers and+ max_redirections and+ trace and+ no_follow
  and+ body in
  scrape_urls
    ~method' ~headers ~body ~trace ~max_redirections ~no_follow ~rel ~url

let cmd =
  let doc = "HTTP tools" in
  let man =
    [ `S Manpage.s_bugs;
      `P "This program is distributed with the Webs OCaml library.
        See https://erratique.ch/software/webs for contact information."; ]
  in
  Cmd.group (Cmd.info "webs" ~version:"%%VERSION%%" ~doc ~man) @@
  [serve_cmd; scrape_urls_cmd; request_cmd]

let main () = Cmd.eval' cmd
let () = if !Sys.interactive then () else exit (main ())
