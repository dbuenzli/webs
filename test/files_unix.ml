(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs
open Webs_kit
let ( let* ) = Result.bind

let service root req =
  Resp.result @@ match Req.path req with
  | "assets" as pre :: _ ->
      let* _m = Req.Allow.(meths [get] req) in
      let* file = Req.to_absolute_filepath ~strip:[pre] ~root req in
      Webs_unix.send_file req file
  | _ ->
      Resp.not_found_404 ()

let main () =
  let conf = Webs_cli.conf_docroot () in
  Webs_cli.quick_serve' ~name:"files_unix" ~conf service

let () = if !Sys.interactive then () else main ()

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
