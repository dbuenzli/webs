(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs

type dur_ns = int64
type log_msg =
[ `Service_exn of exn * Stdlib.Printexc.raw_backtrace
| `Connector_exn of exn * Stdlib.Printexc.raw_backtrace
| `Connection_reset
| `Trace of dur_ns * Http.Request.t option * Http.Response.t option ]

let no_log _ = ()

let pp_exn_backtrace ~kind ppf (e, bt) = (* so many lines. *)
  let string = Format.pp_print_string in
  let exn ppf e = string ppf (Printexc.to_string e) in
  let pp_backtrace_str ppf s =
    let stop = String.length s - 1 (* there's a newline at the end *) in
    let rec loop left right =
      if right = stop then string ppf (String.sub s left (right - left)) else
      if s.[right] <> '\n' then loop left (right + 1) else
      begin
        string ppf (String.sub s left (right - left));
        Format.pp_print_cut ppf ();
        loop (right + 1) (right + 1)
      end
    in
    if s = "" then string ppf "No backtrace available." else
    loop 0 0
  in
  Format.fprintf ppf "@[<v>Unexpected %s exception: %a@,%a@]"
    kind exn e pp_backtrace_str (Printexc.raw_backtrace_to_string bt)

let pp_service_exn ppf e = pp_exn_backtrace ~kind:"service" ppf e
let pp_connector_exn ppf e = pp_exn_backtrace ~kind:"connector" ppf e
let pp_trace ppf dur_ns req resp =
  let strf = Printf.sprintf in
  let dur = match Int64.(equal zero dur_ns) with
  | true -> ""
  | false ->
      match Int64.compare dur_ns 1_000_000L (* < 1ms *) with
      | -1 -> strf " % 3Luµs" (Int64.unsigned_div dur_ns 1_000L)
      | _ -> strf " % 3Lums" (Int64.unsigned_div dur_ns 1_000_000L)
  in
  let method' req = match Http.Request.method' req with
  | `POST | `PUT | `DELETE | `PATCH as m ->
      strf "\x1B[34m%s\x1B[0m" (Http.Method.encode m)
  | m ->  Http.Method.encode m
  in
  let query req = match Http.Request.query req with
  | None -> ""
  | Some q -> strf "?%s" q
  in
  let path req =
    strf "\x1B[1m%s\x1B[0m%s"
      (Http.Path.encode (Http.Request.path req)) (query req)
  in
  let status resp = match Http.Response.status resp with
  | st when st <= 299 -> strf "\x1B[32m%d\x1B[0m" st
  | st when st <= 399 -> strf "\x1B[93m%d\x1B[0m" st
  | st when 400 <= st && st <= 599 -> strf "\x1B[31m%d\x1B[0m" st
  | st -> string_of_int st
  in
  match req, resp with
  | Some req, Some resp ->
      let data =
        String.concat "" @@
        method' req :: " [" :: status resp :: dur :: "] " ::
        path req :: " (" :: Http.Response.reason resp :: ")" ::
        (if Http.Response.explain resp = "" then [] else
         [ " "; Http.Response.explain resp])
      in
      Format.pp_print_string ppf data
  | Some req, None ->
      let data =
        String.concat "" @@
        method' req :: " [" :: dur :: "] " ::
        path req :: " No response" :: []
      in
      Format.pp_print_string ppf data
  | None, Some resp ->
      let data =
        String.concat "" @@
        "???" :: " [" :: status resp :: dur :: "] " ::
        "Can't decode request" :: " (" :: Http.Response.reason resp :: ")" ::
        (if Http.Response.explain resp = "" then [] else
         [ " "; Http.Response.explain resp])
      in
      Format.pp_print_string ppf data
  | None, None ->
      Format.pp_print_string ppf "trace really ?"

let pp_connection_reset ppf () =
  Format.fprintf ppf "Connection reset by peer."

let pp_log_msg ppf = function
| `Trace (dur, req, resp) -> pp_trace ppf dur req resp
| `Service_exn e -> pp_service_exn ppf e
| `Connector_exn e -> pp_connector_exn ppf e
| `Connection_reset -> pp_connection_reset ppf ()

let default_log ?(ppf = Format.err_formatter) ~trace () = function
| `Trace _ when not trace -> ()
| `Trace (dur, req, resp) ->
    pp_trace ppf dur req resp; Format.pp_print_newline ppf ()
| `Service_exn e -> pp_service_exn ppf e; Format.pp_print_newline ppf ()
| `Connector_exn e -> pp_connector_exn ppf e; Format.pp_print_newline ppf ()
| `Connection_reset ->
    pp_connection_reset ppf (); Format.pp_print_newline ppf ()

(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers

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
