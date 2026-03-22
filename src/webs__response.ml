(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let get_reason status = function
| Some reason -> reason | None -> Webs__status.reason_phrase status

(* Responses *)

type t =
  { body : Webs__body.t;
    headers : Webs__headers.t;
    log : string; (* For the server *)
    reason : string;
    status : Webs__status.t;
    version : Webs__version.t; }

let make
    ?(headers = Webs__headers.empty) ?(log = "") ?reason
    ?(version = Webs__version.none) status body
  =
  let reason = get_reason status reason in
  { body; headers; log; reason; status; version }

let empty ?headers ?log ?reason status =
  make ?log ?headers ?reason status Webs__body.empty

let with_body body response =
  Webs__body.dismiss response.body; { response with body }

let with_headers headers response = { response with headers }
let override_headers ~by:headers response =
  let headers = Webs__headers.override response.headers ~by:headers  in
  { response with headers }

let with_log log response = { response with log }
let with_status ?log ?reason status response =
  let log = match log with None -> response.log | Some e -> e in
  let reason = get_reason status reason in
  { response with log; reason; status; }

(* Properties *)

let body response = response.body
let headers response = response.headers
let log response = response.log
let reason response = response.reason
let status response = response.status
let version response = response.version

(* Responding *)

(* Simple content *)

let content ?content_type ?headers ?log ?reason status s =
  let body = Webs__body.of_string ?content_type s in
  make ?headers ?log ?reason status body

let text ?headers ?log ?reason status s =
  let content_type = Webs__media_type.text_plain in
  content ~content_type ?headers ?log ?reason status s

let html ?headers ?log ?reason status s =
  let content_type = Webs__media_type.text_html in
  content ~content_type ?headers ?log ?reason status s

let json ?headers ?log ?reason status s =
  let content_type = Webs__media_type.application_json in
  content ~content_type ?headers ?log ?reason status s

(* Redirections *)

let redirect
    ?(body = Webs__body.empty) ?(headers = Webs__headers.empty) ?log ?reason
    status loc
  =
  let headers = Webs__headers.(define location loc) headers in
  make ~headers ?log ?reason status body

(* Client errors *)

let bad_request_400 ?(body = Webs__body.empty) ?headers ?log ?reason () =
  Error (make ?headers ?log ?reason Webs__status.bad_request_400 body)

let unauthorized_401 ?(body = Webs__body.empty) ?headers ?log ?reason () =
  Error (make ?headers ?log ?reason Webs__status.unauthorized_401 body)

let forbidden_403 ?(body = Webs__body.empty) ?headers ?log ?reason () =
  Error (make ?headers ?log ?reason Webs__status.forbidden_403 body)

let not_found_404 ?(body = Webs__body.empty) ?headers ?log ?reason () =
  Error (make ?headers ?log ?reason Webs__status.not_found_404 body)

let method_not_allowed_405
    ?(body = Webs__body.empty) ?(headers = Webs__headers.empty) ?log ?reason
    ~allowed ()
  =
  let methods = String.concat ", " (List.map Webs__method.encode allowed) in
  let headers = Webs__headers.(define allow methods) headers in
  let status = Webs__status.method_not_allowed_405 in
  Error (make ~headers ?log ?reason status body)

let gone_410 ?(body = Webs__body.empty) ?headers ?log ?reason () =
  Error (make ?headers ?log ?reason Webs__status.gone_410 body)

(* Server errors *)

let server_error_500 ?(body = Webs__body.empty) ?headers ?log ?reason () =
  Error (make ?headers ?log ?reason Webs__status.server_error_500 body)

let not_implemented_501 ?(body = Webs__body.empty) ?headers ?log ?reason () =
  Error (make ?headers ?log ?reason Webs__status.not_implemented_501 body)

let bad_gateway_502 ?(body = Webs__body.empty) ?headers ?log ?reason () =
  Error (make ?headers ?log ?reason Webs__status.bad_gateway_502 body)

let service_unavailable_503
    ?(body = Webs__body.empty) ?headers ?log ?reason ()
  =
  Error (make ?headers ?log ?reason Webs__status.service_unavailable_503 body)

let todo = not_implemented_501

(* Predicates and comparisons *)

let is_body_empty response = Webs__body.is_empty response.body
let compare r0 r1 =
  let c = Webs__body.compare r0.body r1.body in
  if c <> 0 then c else
  let c = Webs__headers.compare r0.headers r1.headers in
  if c <> 0 then c else
  let c = String.compare r0.log r1.log in
  if c <> 0 then c else
  let c = String.compare r0.reason r1.reason in
  if c <> 0 then c else
  let c = Webs__status.compare r0.status r1.status in
  if c <> 0 then c else
  Webs__version.compare r0.version r1.version

let equal r0 r1 = compare r0 r1 = 0

(* Error handling *)

let map_errors ~only_on_empty_body f response =
  let status = status response in
  if 400 <= status && status <= 599 &&
     (not only_on_empty_body || is_body_empty response)
  then f response
  else response

(* Formatting *)

let pp ppf response =
  let open Webs__base in
  Format.pp_open_vbox ppf 0;
  Fmt.field "version" Webs__version.pp ppf response.version; Fmt.cut ppf ();
  Fmt.field "status" Webs__status.pp ppf response.status; Fmt.cut ppf ();
  Fmt.field "reason" Fmt.qstring ppf response.reason; Fmt.cut ppf ();
  Fmt.field "log" Fmt.qstring ppf response.log; Fmt.cut ppf ();
  Webs__headers.pp ppf response.headers;
  if not (Webs__headers.is_empty response.headers) then Fmt.cut ppf ();
  Fmt.field "body" Webs__body.pp ppf response.body;
  Format.pp_close_box ppf ()
