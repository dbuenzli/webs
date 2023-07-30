(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
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
  let doc = strf "Listen for connections on address $(i,ADDR) and port \
                  $(i,PORT) (default to %d) or Unix domain socket $(i,PATH)."
      default_port
  in
  let docv = "ADDR[:PORT]|PATH" in
  let lconv = listener_conv ~default_port in
  Arg.(value & opt lconv default_listener & info opts ?docs ~doc ~docv)

let http_path =
  let parse s = Result.map_error (fun e -> `Msg e) (Http.Path.decode s) in
  let print ppf p = Format.pp_print_string ppf (Http.Path.encode p) in
  Arg.conv ~docv:"PATH"  (parse, print)

let service_path ?(opts = ["service-path"]) ?docs () =
  let doc =
    "$(docv) is the path at which the root of the service is being served."
  in
  let arg_info = Arg.info opts ?docs ~doc ~docv:"PATH" in
  Arg.(value & opt (some ~none:"/" http_path) None & arg_info)

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
let log_if_error = function Ok () -> 0 | Error e -> log "Error: %s" e; 1

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

let quick_service s listener service_path max_connections conf =
  log_if_error @@
  let* conf = conf in
  let c = Webs_httpc.create ~listener ?service_path ~max_connections () in
  let p = Webs_httpc.service_path c in
  log "Listening on http://%a%a" Webs_unix.pp_listener listener Http.Path.pp p;
  Webs_httpc.serve c (s conf)

let quick_serve' ?version ?man ?(doc = "Undocumented service") ~name ~conf s =
  let listener = listener () in
  let service_path = service_path () in
  let max_connections = max_connections () in
  let exits =
    Cmd.Exit.info ~doc:"on indiscriminate error reported on stderr." 1 ::
    Cmd.Exit.defaults
  in
  let term = Term.(const (quick_service s) $ listener $ service_path $
                   max_connections $ conf)
  in
  let cmd = Cmd.v (Cmd.info name ?version ~doc ?man ~exits) term in
  exit (Cmd.eval' cmd)

let quick_serve ?version ?man ?doc ~name s =
  let conf = (Term.const (Ok ())) in
  quick_serve' ?version ?man ?doc ~name ~conf (fun () -> s)
