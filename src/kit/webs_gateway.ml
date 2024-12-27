(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs

let send_file ~header file =
  let headers = Http.Headers.(def header file) Http.Headers.empty in
  let log = Printf.sprintf "%s: %s" (header :> string) file in
  Ok (Http.Response.empty Http.Status.ok_200 ~headers ~log)

let x_accel_redirect = Http.Headers.name "x-accel-redirect"
let x_sendfile = Http.Headers.name "x-sendfile"
