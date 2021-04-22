(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs
open Cmdliner

let strf = Printf.sprintf
let ( let* ) = Result.bind

let listener_conv ~default_port =
  let parse s =
    Result.map_error (fun e -> `Msg e) @@
    Webs_unix.listener_of_string ~default_port s
  in
  Arg.conv (parse, Webs_unix.pp_listener)

let listener
    ?(opts = ["l"; "listen"]) ?docs ?(default_port = 8000)
    ?(default_listener = `Host ("localhost", default_port)) ()
  =
  let doc = strf "Listen for connections on given address and port \
                  (default to %d) or Unix domain socket." default_port
  in
  let docv = "ADDR[:PORT]|PATH" in
  let lconv = listener_conv ~default_port in
  Arg.(value & opt lconv default_listener & info opts ?docs ~doc ~docv)

let docroot ?(opts = ["d"; "docroot"]) ?docs () =
  let doc = "Use $(docv) as a document root." and docv = "DIR" in
  let arg_info = Arg.info opts ?docs ~doc ~docv in
  Arg.(value & opt (some ~none:"none" string) None & arg_info)

let positive =
  let parse s = match int_of_string_opt s with
  | None -> Error (`Msg "could not parse integer")
  | Some n when n < 0 -> Error (`Msg "integer not strictly positive")
  | Some n -> Ok n
  in
  Arg.conv ~docv:"INT" (parse, Format.pp_print_int)

let max_connections ?(opts = ["c"; "max-connections"]) ?docs () =
  let doc = "The maximal number $(docv) of concurrent connections served." in
  let docv = "INT" in
  let arg_info = Arg.info opts ?docs ~doc ~docv in
  Arg.(value & opt positive Webs_httpc.default_max_connections & arg_info)

(* Quick service *)

let log fmt = Format.fprintf Format.err_formatter ("@[" ^^ fmt ^^ "@]@.")
let log_if_error = function
| Ok () -> 0 | Error e -> log "Error: %s" e; 1

let conf_docroot () =
  let setup root = match Webs_unix.realpath root with
  | dir -> log "Document root: %s" dir; Ok dir
  | exception Unix.Unix_error (e, _, _) ->
      Error (strf "%s: %s" root (Unix.error_message e))
  in
  let doc = "Use $(docv) as a document root." and docv = "DIR" in
  let arg_info = Arg.info ["d"; "docroot"] ~doc ~docv in
  let arg = Arg.(required & opt (some string) None & arg_info) in
  Term.(const setup $ arg)

let quick_service s listener max_connections conf =
  log_if_error @@
  let* conf = conf in
  let c = Webs_httpc.create ~listener ~max_connections () in
  log "Listening on http://%a" Webs_unix.pp_listener listener;
  Webs_httpc.serve c (s conf)

let quick_serve' ?version ?man ?(doc = "Undocumented service") ~name ~conf s =
  let listener = listener () in
  let max_connections = max_connections () in
  let exits =
    Term.exit_info ~doc:"on indiscriminate error reported on stderr." 1 ::
    Term.default_exits
  in
  let term = Term.(const (quick_service s) $ listener $ max_connections $ conf)
  in
  let info = Term.info name ?version ~doc ?man ~exits in
  Term.exit_status @@ Term.eval (term, info)

let quick_serve ?version ?man ?doc ~name s =
  let conf = (Term.const (Ok ())) in
  quick_serve' ?version ?man ?doc ~name ~conf (fun () -> s)

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers

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
