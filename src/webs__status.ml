(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = int
let reason_phrase = function
(* 1XX *)
| 100 -> "Continue"
| 101 -> "Switching Protocols"
(* 2XX *)
| 200 -> "OK"
| 201 -> "Created"
| 202 -> "Accepted"
| 203 -> "Non-Authoritative Information"
| 204 -> "No Content"
| 205 -> "Reset Content"
| 206 -> "Partial Content"
(* 3XX *)
| 300 -> "Multiple Choices"
| 301 -> "Moved Permanently"
| 302 -> "Found"
| 303 -> "See Other"
| 304 -> "Not Modified"
| 305 -> "Use Proxy"
| 307 -> "Temporary Redirect"
| 308 -> "Permanent Redirect"
(* 4XX *)
| 400 -> "Bad Request"
| 401 -> "Unauthorized"
| 402 -> "Payment Required"
| 403 -> "Forbidden"
| 404 -> "Not Found"
| 405 -> "Method Not Allowed"
| 406 -> "Not Acceptable"
| 407 -> "Proxy Authentication Required"
| 408 -> "Request Timeout"
| 409 -> "Conflict"
| 410 -> "Gone"
| 411 -> "Length Required"
| 412 -> "Precondition Failed"
| 413 -> "Content Too Large"
| 414 -> "URI Too Long"
| 415 -> "Unsupported Media Type"
| 416 -> "Range Not Satisfiable"
| 417 -> "Expectation Failed"
| 418 -> "I'm a teapot"
| 426 -> "Upgrade Required"
(* 5XX *)
| 500 -> "Internal Server Error"
| 501 -> "Not Implemented"
| 502 -> "Bad Gateway"
| 503 -> "Service Unavailable"
| 504 -> "Gateway Time-out"
| 505 -> "HTTP Version Not Supported"
(* XXX *)
| _ -> "Unknown"

let continue_100 = 100
let switching_protocols_101 = 101
let ok_200 = 200
let created_201 = 201
let accepted_202 = 202
let non_authoritative_information_203 = 203
let no_content_204 = 204
let reset_content_205 = 205
let partial_content_206 = 206
let multiple_choices_300 = 300
let moved_permanently_301 = 301
let found_302 = 302
let see_other_303 = 303
let not_modified_304 = 304
let use_proxy_305 = 305
let temporary_redirect_307 = 307
let permanent_redirect_308 = 308
let bad_request_400 = 400
let unauthorized_401 = 401
let payement_required_402 = 402
let forbidden_403 = 403
let not_found_404 = 404
let method_not_allowed_405 = 405
let not_acceptable_406 = 406
let proxy_authentication_required_407 = 407
let request_time_out_408 = 408
let conflict_409 = 409
let gone_410 = 410
let length_required_411 = 411
let precondition_failed_412 = 412
let content_too_large_413 = 413
let uri_too_long_414 = 414
let unsupported_media_type_415 = 415
let range_not_satisfiable_416 = 416
let expectation_failed_417 = 417
let i'm_a_teapot_418 = 418
let upgrade_required_426 = 426
let server_error_500 = 500
let not_implemented_501 = 501
let bad_gateway_502 = 502
let service_unavailable_503 = 503
let gateway_time_out_504 = 504
let http_version_not_supported_505 = 505

(* Predicates and comparisons *)

let equal = Int.equal
let compare = Int.compare

(* Formatting *)

let pp ppf status =
  Webs__base.Fmt.pf ppf "@[%d – %s@]" status (reason_phrase status)
