(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs
open Cmdliner

let ( let* ) = Result.bind

let listener_conv ~default_port =
  let parse s = Webs_listener.of_string ~default_port s in
  Arg.conv' (parse, Webs_listener.pp)

let listener
    ?(opts = ["l"; "listen"]) ?docs ?(default_port = 8000)
    ?(default_listener = `Host ("localhost", default_port)) ()
  =
  let doc =
    Printf.sprintf
      "Listen for connections on address $(i,ADDR) and port \
       $(i,PORT) (default to %d) or Unix domain socket $(i,PATH)."
      default_port
  in
  let docv = "ADDR[:PORT]|PATH" in
  let lconv = listener_conv ~default_port in
  Arg.(value & opt lconv default_listener & info opts ?docs ~doc ~docv)

let http_path = Arg.conv' ~docv:"PATH"  (Http.Path.decode, Http.Path.pp)

let service_path ?(opts = ["service-path"]) ?docs () =
  let doc =
    "$(docv) is the path at which the root of the service is being served."
  in
  let arg_info = Arg.info opts ?docs ~doc ~docv:"PATH" in
  Arg.(value & opt (some ~none:"/" http_path) None & arg_info)

let docroot ?(opts = ["d"; "docroot"]) ?docs () =
  let doc = "Use $(docv) as a document root for serving files." in
  let docv = "DIR" in
  let arg_info = Arg.info opts ?docs ~doc ~docv ~absent:"no file served" in
  Arg.(value & opt (some string) None & arg_info)

let positive =
  let parse s = match int_of_string_opt s with
  | None -> Error "could not parse integer"
  | Some n when n < 0 -> Error "integer not strictly positive"
  | Some n -> Ok n
  in
  Arg.conv' ~docv:"INT" (parse, Format.pp_print_int)

let max_connections ?(opts = ["c"; "max-connections"]) ?docs () =
  let doc = "The maximal number $(docv) of concurrent connections served." in
  let docv = "INT" in
  let arg_info = Arg.info opts ?docs ~doc ~docv in
  let default = Webs_http11_gateway.default_max_connections in
  Arg.(value & opt positive default & arg_info)
