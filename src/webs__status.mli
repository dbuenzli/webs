(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Status codes.

    @canonical Webs.Http.Status *)

(** {1:status_codes Status codes} *)

type t = int
(** The type for
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-status-codes}
    status codes}. *)

val reason_phrase : t -> string
(** [reason_phrase s] is [s]'s reason phrase. *)

(** {1:predef Predefined status codes} *)

(** {2:informational Informational 1xx} *)

val continue_100 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-100-continue}[100]} *)

val switching_protocols_101 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-101-switching-protocols}
    [101]} *)

(** {2:sucessful Sucessful 2xx} *)

val ok_200 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-200-ok}[200]} *)

val created_201 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-201-created}[201]} *)

val accepted_202 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-202-accepted}[202]} *)

val non_authoritative_information_203 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-203-non-authoritative-infor}
    [203]} *)

val no_content_204 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-204-no-content}[204]} *)

val reset_content_205 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-205-reset-content}
    [205]} *)

val partial_content_206 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-206-partial-content}
    [206]} *)

(** {2:redirection Redirection 3xx} *)

val multiple_choices_300 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-300-multiple-choices}
    [300]} *)

val moved_permanently_301 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-301-moved-permanently}
    [301]} *)

val found_302 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-302-found}[302]} *)

val see_other_303 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-303-see-other}[303]} *)

val not_modified_304 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-304-not-modified}
    [304]} *)

val use_proxy_305 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-305-use-proxy}[305]} *)

val temporary_redirect_307 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-307-temporary-redirect}
    [307]} *)

val permanent_redirect_308 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-308-temporary-redirect}
    [308]} *)

(** {2:client_error Client Error 4xx} *)

val bad_request_400 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-400-bad-request}[400]} *)

val unauthorized_401 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-401-unauthorized}
    [401]} *)

val payement_required_402 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-402-payment-required}
    [402]} *)

val forbidden_403 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-403-forbidden}[403]} *)

val not_found_404 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-404-not-found}[404]} *)

val method_not_allowed_405 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-405-method-not-allowed}
    [405]} *)

val not_acceptable_406 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-406-not-acceptable}
    [406]} *)

val proxy_authentication_required_407 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-407-proxy-authentication-re}
    [407]} *)

val request_time_out_408 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-408-request-timeout}
    [408]} *)

val conflict_409 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-409-conflict}[409]} *)

val gone_410 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-410-gone}[410]} *)

val length_required_411 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-411-length-required}
    [411]} *)

val precondition_failed_412 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-412-precondition-failed}
    [412]} *)

val content_too_large_413 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-413-content-too-large}
    [413]} *)

val uri_too_long_414 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-414-uri-too-long}
    [414]} *)

val unsupported_media_type_415 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-415-unsupported-media-type}
    [415]} *)

val range_not_satisfiable_416 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-416-range-not-satisfiable}
    [416]} *)

val expectation_failed_417 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-417-expectation-failed}
    [417]} *)

val i'm_a_teapot_418 : t
(** {{:https://www.rfc-editor.org/rfc/rfc2324#section-2.3.2}[418]} *)

val upgrade_required_426 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-426-upgrade-required}
    [436]} *)

(** {2:server_error Server Error 5xx} *)

val server_error_500 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-500-internal-server-error}
    [500]} *)

val not_implemented_501 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-501-not-implemented}
    [501]} *)

val bad_gateway_502 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-502-bad-gateway}[502]} *)

val service_unavailable_503 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-503-service-unavailable}
    [503]} *)

val gateway_time_out_504 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-504-gateway-timeout}
    [504]} *)

val http_version_not_supported_505 : t
(** {{:https://www.rfc-editor.org/rfc/rfc9110#name-505-http-version-not-suppor}
    [505]} *)

(** {1:predicates Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal] tests statuses for equality. *)

val compare : t -> t -> int
(** [compare] is a total order on statuses compatible with {!equal}. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats status codes for inspection. *)
