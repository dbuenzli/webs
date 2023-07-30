(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs
open Webs_kit
let ( let* ) = Result.bind

let service root req =
  Http.Response.result @@ match Http.Request.path req with
  | "assets" as pre :: _ ->
      let* `GET = Http.Request.allow Http.Method.[get] req in
      let* file = Http.Request.to_absolute_filepath ~strip:[pre] ~root req in
      Webs_unix.send_file req file
  | _ ->
      Http.Response.not_found_404 ()

let main () =
  let conf = Webs_cli.conf_docroot () in
  Webs_cli.quick_serve' ~name:"files_unix" ~conf service

let () = if !Sys.interactive then () else main ()
