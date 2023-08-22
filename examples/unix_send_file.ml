(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Responds to request on /assets/$FILE by serving a $FILE from
   a root directory specified on the command line via a required -d
   option.

   This handles etags and range requests. If you rather have the
   gateway serve your file see [gateway_send_file.ml]. *)

open Webs
let ( let* ) = Result.bind

let send_asset ~strip ~file_root request =
  let* file = Http.Request.to_absolute_filepath ~strip ~file_root request in
  Webs_fs.send_file request file

let service file_root request =
  Http.Response.result @@ match Http.Request.path request with
  | "assets" as pre :: _ -> send_asset ~strip:[pre] ~file_root request
  | _ -> Http.Response.not_found_404 ()

let main () =
  let conf = Webs_quick.conf_docroot () in
  Webs_quick.serve' ~conf service

let () = if !Sys.interactive then () else exit (main ())
