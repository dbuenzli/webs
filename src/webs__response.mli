(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** HTTP responses.

    These values are:

    {ul
    {- Constructed by client connectors according to
       {{!page-connector_conventions.client_responses}these conventions} and
         handed over to client users for intepretation.}
    {- Constructed by service implementations and interpreted
       by service connectors according to
       {{!page-connector_conventions.service_responses}these conventions}.}}

    @canonical Webs.Http.Response *)

(** {1:responses Responses} *)

type t
(** The type for HTTP responses. An HTTP message head and a response body. *)

val make :
  ?headers:Webs__headers.t -> ?log:string -> ?reason:string ->
  ?version:Webs__version.t -> Webs__status.t -> Webs__body.t -> t
(** [make status body] is a response with given [status] and [body] and:
    {ul
    {- [headers], the response headers. Defaults to {!Http.Headers.empty}.
       In general it is better to let the {{!Http.Body.make}body} define the
       content type and content length headers, see the
       {{!page-connector_conventions.client_responses}client response} and
       {{!page-connector_conventions.service_responses}service response}
       conventions.}
    {- [log], the connector side log message, see {!log}. Defaults to [""].}
    {- [reason], the status reason phrase. Defaults to
       {!Http.Status.reason_phrase}[ status].}
    {- [version], the HTTP version, see {!version}. Defaults to
       {!Version.none}. Service connectors generally ignore this,
       they decide how they want to send the response to the client.}} *)

val empty :
  ?headers:Webs__headers.t -> ?log:string -> ?reason:string ->
  Webs__status.t -> t
(** [empty status] is [make status Http.Body.empty]. *)

val with_body : Webs__body.t -> t -> t
(** [with_body b response] is [response] with body [b]. The body
    of [response] is {!Http.Body.dismiss}ed. *)

val with_headers : Webs__headers.t -> t -> t
(** [with_headers headers response] is [response] with headers [headers]. *)

val override_headers : by:Webs__headers.t -> t -> t
(** [override_headers ~by response] is [response] with headers
    [Http.Headers.override (headers response) ~by]. *)

val with_log : string -> t -> t
(** [with_log log response] is [response] with log [log]. *)

val with_status : ?log:string -> ?reason:string -> Webs__status.t -> t -> t
(** [with_status status response] is [response] with status [status], reason
    phrase [reason] (defaults to {!Http.Status.reason_phrase}[
    status], use [reason response] to keep the previous reason) and
    log [log] (defaults to [log response]). *)

(** {1:properties Properties} *)

val body : t -> Webs__body.t
(** [body response] is the body of [response]. *)

val headers : t -> Webs__headers.t
(** [headers response] are the headers of [response]. *)

val log : t -> string
(** [log response] is the log of [response]. The log is a
    server-side {!reason} {b not meant to be sent to the
    client}. It can be used to log further details or explanations
    about the answer that one may not want to disclose to the
    client. *)

val reason : t -> string
(** [reason response] is the reason phrase of [response]. *)

val status : t -> Webs__status.t
(** [status response] is the
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-status-codes}status}
    of [response]. *)

val version : t -> Webs__version.t
(** [version response] is the version of [response].
    {ul
    {- For client connectors this should be the HTTP version of the
      response read by the connector.}
    {- For service connectors this is mostly irrelevant: the connector
       decides how it wants to send the response to the client. But if a
       connector supports multiple versions it can be used as a hint for
       which HTTP version to use.}} *)

(** {1:responding Responding}

    See also {{!Webs.Http.Request.deconstruct}request deconstruction}
    combinators. *)

(** {2:simple Simple content} *)

val content :
  ?content_type:Webs__media_type.t ->  ?headers:Webs__headers.t ->
  ?log:string -> ?reason:string -> Webs__status.t -> string -> t
(** [content status s] is
    [make status (Http.Body.of_string ?content_type s)]. *)

val text :
  ?headers:Webs__headers.t -> ?log:string -> ?reason:string ->
  Webs__status.t -> string -> t
(** [text] responds with UTF-8 encoded plain text.
    This is {!content} with {!Media_type.text_plain}. *)

val html :
  ?headers:Webs__headers.t -> ?log:string -> ?reason:string ->
  Webs__status.t -> string -> t
(** [html] responds with UTF-8 encoded HTML text. This
    is {!content} with {!Media_type.text_html}.  *)

val json :
  ?headers:Webs__headers.t -> ?log:string -> ?reason:string ->
  Webs__status.t -> string -> t
(** [json] responds with JSON text. This is {!content} with
    {!Media_type.application_json}. *)

(** {2:redirections Redirections} *)

val redirect :
  ?body:Webs__body.t -> ?headers:Webs__headers.t ->
  ?log:string -> ?reason:string -> Webs__status.t -> string -> t
(** [redirect status loc] is a response with status [status] and
    {!Http.Headers.location} set to [loc] on headers.  [body] defaults
    to {!Body.empty}.

    {b Warning.} It is your duty to properly percent-encode [loc]
    using for example {!Webs.Url.Percent} or {!Path.encode}.

    See also {!Request.redirect_to_path}. *)

(** {2:client_errors Client errors} *)

val bad_request_400 :
  ?body:Webs__body.t -> ?headers:Webs__headers.t -> ?log:string ->
  ?reason:string -> unit -> ('a, t) result
(** [bad_request_400 ()] is [Error r] with [r] a response with status
    {!Status.bad_request_400}. [body] defaults to {!Body.empty}. *)

val unauthorized_401 :
  ?body:Webs__body.t -> ?headers:Webs__headers.t -> ?log:string ->
  ?reason:string -> unit -> ('a, t) result
(** [unauthorized_401 ()] is [Error r] with [r] a response with status
    {!Status.unauthorized_401}. [body] defaults to {!Body.empty}. *)

val forbidden_403 :
  ?body:Webs__body.t -> ?headers:Webs__headers.t -> ?log:string ->
  ?reason:string -> unit -> ('a, t) result
(** [forbidden_403 ()] is [Error r] with [r] a response with status
    {!Status.forbidden_403}. [body] defaults to {!Body.empty}. *)

val not_found_404 :
  ?body:Webs__body.t -> ?headers:Webs__headers.t -> ?log:string ->
  ?reason:string -> unit -> ('a, t) result
(** [not_found_404 ()] is [Error r] with [r] a response with status
    {!Status.not_found_404}. [body] defaults to {!Body.empty}. *)

val method_not_allowed_405 :
  ?body:Webs__body.t -> ?headers:Webs__headers.t -> ?log:string ->
  ?reason:string -> allowed:Webs__method.t list -> unit -> ('a, t) result
(** [method_not_allowed_450 ~allowed ()] is [Error r] with [r] a
    response with status {!Status.method_not_allowed_405} and
    {!Http.Headers.allow} set on [headers] with the [allowed]
    methods (which can be empty). [body] defaults to
    {!Body.empty}. *)

val gone_410 :
  ?body:Webs__body.t -> ?headers:Webs__headers.t -> ?log:string ->
  ?reason:string -> unit -> ('a, t) result
(** [gone_410 ()] is [Error r] with [r] a response with
    status {!Status.gone_410}. [body] defaults to {!Body.empty}. *)

(** {2:server_errors Server errors} *)

val todo :
  ?body:Webs__body.t -> ?headers:Webs__headers.t -> ?log:string ->
  ?reason:string -> unit -> ('a, t) result
(** [todo] is {!not_implemented_501}. *)

val server_error_500 :
  ?body:Webs__body.t -> ?headers:Webs__headers.t -> ?log:string ->
  ?reason:string -> unit -> ('a, t) result
(** [server_error_500 ()] is [Error r] with [r] a response with status
    {!Status.server_error_500}. [body] defaults to {!Body.empty}. *)

val not_implemented_501 :
  ?body:Webs__body.t -> ?headers:Webs__headers.t -> ?log:string ->
  ?reason:string -> unit -> ('a, t) result
(** [not_implemented_501 ()] is [Error r] with [r] a response with
    status {!Status.not_implemented_501}. [body] defaults to
    {!Body.empty}. *)

val bad_gateway_502 :
  ?body:Webs__body.t -> ?headers:Webs__headers.t -> ?log:string ->
  ?reason:string -> unit -> ('a, t) result
(** [bad_gateway_502 ()] is [Error r] with [r] a response with
    status {!Status.bad_gateway_502}. [body] defaults to
    {!Body.empty}. *)

val service_unavailable_503 :
  ?body:Webs__body.t -> ?headers:Webs__headers.t -> ?log:string ->
  ?reason:string -> unit -> ('a, t) result
(** [service_unavailable_503 ()] is [Error r] with [r] a response with
    status {!Status.service_unavailable_503}. [body] defaults to
    {!Body.empty}. *)

(** {2:error_map Error handling} *)

val map_errors : only_on_empty_body:bool -> (t -> t) -> t -> t
(** [map_errors ~only_on_empty_body f response] maps reponse [response]
    with [f] if [r]'s status is a 4XX or 5XX. If [only_on_empty_body]
    is [true] it does so only whenever [is_body_empty response] is
    [true].

    The idea of {!map_errors} is that service and service building blocks
    define their errors as responses with empty bodies. This function
    can be called just before handing over the reponse to the
    connector to define a page content for [response] with
    {!with_body}. *)

(** {1:predicates Predicates and comparisons} *)

val is_body_empty : t -> bool
(** [is_body_empty response] is [Http.Body.is_empty (body response)]. *)

val equal : t -> t -> bool
(** [equal] tests responses for equality. This consumes the response body. *)

val compare : t -> t -> int
(** [compare] is a total order on responses compatible with
    {!equal}. This consumes the response body. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats responses for inspection. Guarantees not consume
    the response body. *)
