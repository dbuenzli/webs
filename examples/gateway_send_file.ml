(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Responds to requests on /assets/$FILE by telling the gateway to
   send the file it serves at the URL /myservice-files/$FILE.

   If you rather have the service itself serve the file, see
   [unix_send_file.ml].

   This example uses the header field name x-accel-redirect which is
   what nginx expects. You may need to adjust [send_header] if you are
   using a different gateway. A typical configuration for nginx would
   be:

   location /myservice-files/
   {
     internal;
     alias /my/files/; # final slash is important
   }
*)

open Webs
let ( let* ) = Result.bind

let send_header = Webs_gateway.x_accel_redirect
let file_root = "/myservice-files"

let send_asset ~strip ~file_root request =
  let* file = Http.Request.to_absolute_filepath ~strip ~file_root request in
  Webs_gateway.send_file ~header:send_header file

let service request =
  Http.Response.result @@ match Http.Request.path request with
  | "assets" as pre :: _ -> send_asset ~strip:[pre] ~file_root request
  | _ -> Http.Response.not_found_404 ()

let main () = Webs_quick.serve service
let () = if !Sys.interactive then () else exit (main ())
