(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Log = struct
  type dur_ns = int64
  type msg =
  [ `Service_exn of exn * Stdlib.Printexc.raw_backtrace
  | `Connector_exn of exn * Stdlib.Printexc.raw_backtrace
  | `Connection_reset
  | `Trace of dur_ns * Webs__request.t option * Webs__response.t option ]

  let pp_service_exn ppf e =
    Webs__base.Fmt.exn_backtrace ~kind:"service" ppf e

  let pp_connector_exn ppf e =
    Webs__base.Fmt.exn_backtrace ~kind:"connector" ppf e

  let pp_connection_reset ppf () =
    Webs__base.Fmt.pf ppf "Connection reset by peer."

  let pp_trace ppf dur_ns request response =
    let strf = Printf.sprintf in
    let dur =
      if Int64.(equal zero dur_ns) then "" else
      match Int64.compare dur_ns 1_000_000L (* < 1ms *) with
      | -1 -> strf " %3Luµs" (Int64.unsigned_div dur_ns 1_000L)
      | _ -> strf " %3Lums" (Int64.unsigned_div dur_ns 1_000_000L)
    in
    let method' req = match Webs__request.method' req with
    | `POST | `PUT | `DELETE | `PATCH as m ->
        strf "\x1B[34m%s\x1B[0m" (Webs__method.encode m)
    | m ->  Webs__method.encode m
    in
    let query req = match Webs__request.query req with
    | None -> ""
    | Some q -> strf "?%s" q
    in
    let path req =
      strf "\x1B[1m%s\x1B[0m%s"
        (Webs__path.encode (Webs__request.path req)) (query req)
    in
    let status resp = match Webs__response.status resp with
    | st when st <= 299 -> strf "\x1B[32m%d\x1B[0m" st
    | st when st <= 399 -> strf "\x1B[93m%d\x1B[0m" st
    | st when 400 <= st && st <= 599 -> strf "\x1B[31m%d\x1B[0m" st
    | st -> string_of_int st
    in
    let data = match request, response with
    | Some request, Some response ->
        String.concat "" @@
        method' request :: " [" :: status response :: dur :: "] " ::
        path request :: " (" :: Webs__response.reason response :: ")" ::
        (if Webs__response.log response = "" then [] else
         [ " "; Webs__response.log response])
    | Some req, None ->
        String.concat "" @@
        method' req :: " [" :: dur :: "] " ::
        path req :: " No response" :: []
    | None, Some resp ->
        String.concat "" @@
        "???" :: " [" :: status resp :: dur :: "] " ::
        "Can't decode request" :: " (" :: Webs__response.reason resp :: ")" ::
        (if Webs__response.log resp = "" then [] else
         [ " "; Webs__response.log resp])
    | None, None -> "trace really ?"
    in
    Webs__base.Fmt.string ppf data

  let pp_msg ppf = function
  | `Trace (dur, request, response) -> pp_trace ppf dur request response
  | `Service_exn e -> pp_service_exn ppf e
  | `Connector_exn e -> pp_connector_exn ppf e
  | `Connection_reset -> pp_connection_reset ppf ()

  let quiet _ = ()
  let default ?(ppf = Format.err_formatter) ~trace () = function
  | `Trace _ when not trace -> ()
  | `Trace (dur, req, resp) ->
      pp_trace ppf dur req resp; Webs__base.Fmt.nl ppf ()
  | `Service_exn e ->
      pp_service_exn ppf e; Webs__base.Fmt.nl ppf ()
  | `Connector_exn e ->
      pp_connector_exn ppf e; Webs__base.Fmt.nl ppf ()
  | `Connection_reset ->
      pp_connection_reset ppf (); Webs__base.Fmt.nl ppf ()
end

module Default = struct
  let max_http_head_byte_size = 64 * 1024
  let max_http_body_byte_size = 10 * 1024 * 1024
end
