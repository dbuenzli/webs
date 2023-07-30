(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs
open Webs_kit
let ( let* ) = Result.bind

let root = "/myservice-files"
let service req =
  Http.Response.result @@ match Http.Request.path req with
  | "assets" as pre :: _ ->
      let* `GET = Http.Request.allow Http.Method.[get] req in
      let* file = Http.Request.to_absolute_filepath ~strip:[pre] ~root req in
      Gateway.send_file ~header:Gateway.x_accel_redirect req file
  | _ ->
      Http.Response.not_found_404 ()

let main () = Webs_cli.quick_serve ~name:"files_gateway" service
let () = if !Sys.interactive then () else main ()
