(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs

let service = Http.Request.echo
let main () = Webs_cli.quick_serve ~name:"min" service
let () = if !Sys.interactive then () else main ()
