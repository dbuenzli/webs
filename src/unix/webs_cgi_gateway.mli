(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** CGI gateway connector.

    This connector serves one request via CGI by reading headers from
    the process environment, reading the body from {!Unix.stdin} and
    writing the response on {!Unix.stdout}.

    See {!serve} for making sure your gateway passes an environment
    suitable for deriving a {!Webs.Http.Response.t} value.

    {b Note.} This was reimplemented at some point. But that new
    implementation was little tested. Expect hiccups.

    {b References.}y
    {ul
    {- D. Robinson et al.
       {{:https://www.rfc-editor.org/rfc/rfc3875}
       {e The Common Gateway Interface (CGI) Version 1.1}. 2004.}}} *)

open Webs

(** {1:connector Connector} *)

type t
(** The type for CGI connectors.  *)

val make :
  ?extra_vars:string list -> ?log:(Http.Connector.Log.msg -> unit) ->
  ?max_request_body_byte_size:int -> ?service_path:Http.Path.t -> unit -> t
(** [make ()] is a new connector with:
    {ul
    {- [extra_vars c] the list of custom environment variables whose content
       is added to the request headers of requests handled by [c]. See
       {!serve}.}
    {- [log] the connector message logger. Defaults to
       {!Webs.Http.Connector.Log.default} with trace enabled.}
    {- [max_request_body_byte_size] the maximal request body size in bytes.
       FIXME not enforced.}
    {- [service_path] is the path on which the root of the service is
       served by the gateway. This value can be found in a request in the
       {!Webs.Http.Request.service_path} property. This path is
       stripped from the path found in the request's target to yield
       the {!Webs.Http.Request.path} property of the request.
       Defaults to {!Webs.Http.Path.root}.}} *)

val extra_vars : t -> string list
(** [extra_vars c] is the list of additional environment variables
    of [c]. See {!make}. *)

val log : t -> (Http.Connector.Log.msg -> unit)
(** [log c] is the log of [c]. See {!make}. *)

val max_request_body_byte_size : t -> int
(** [max_request_body_byte_size c] is the maximal request body size
    in bytes supported by [c]. See {!make}. *)

val service_path : t -> Http.Path.t
(** [service_path c] is service path of [c]. See {!make}.  *)

(** {1:serving Serving} *)

val serve : t -> (Http.Request.t -> Http.Response.t) -> (unit, string) result
(** [serve c service] runs service [service] with connector [c]. The
    connector supports {!Webs_unix.Fd.Writer} body writers.

    This blocks until the response for the request has been
    served. The error is returned in case of connector error, it's a
    good practice to write the message on stderr and exit with a
    non-zero exit code if that happens.

	  The {!Webs.Http.Request.t} value is constructed from the environment and
    {!Unix.stdin} as follows:
    {ul
    {- {!Webs.Http.Request.method'} is the value of the
       {{:http://tools.ietf.org/html/rfc3875#section-4.3}[REQUEST_METHOD]}
       variable.}
    {- {!Webs.Http.Request.raw_path} is the value of the
       {b non standard} [REQUEST_URI] which should have the raw,
       percent-encoded,
       {{:https://www.rfc-editor.org/rfc/rfc9112#name-request-target}request
       target} (HTTP/1.1) or
       {{:https://www.rfc-editor.org/rfc/rfc9113#section-8.3.1-2.4.1}
       [:path]} pseudo-header (HTTP/2, HTTP/3).}
    {- {!Webs.Http.Request.version} is the value of the
       {{:http://tools.ietf.org/html/rfc3875#section-4.1.16}[SERVER_PROTOCOL]}
       variable.}
    {- {!Webs.Http.Request.service_path} is the value of [service_path c]}
	  {- {!Webs.Http.Request.headers} has the following headers defined:
       {ul
       {- {!Webs.Http.Headers.content_type} if the variable
          {{:http://tools.ietf.org/html/rfc3875#section-4.1.3}[CONTENT_TYPE]}
          is defined and non empty.}
       {- {!Webs.Http.Headers.content_length} if the variable
          {{:http://tools.ietf.org/html/rfc3875#section-4.1.2}[CONTENT_LENGTH]}
          is defined and non empty.}
       {- For any other variable of the form [HTTP_$VAR] there is a
          corresponding [$var] header whose name is made by
          lowercasing [$VAR] and mapping ['_'] to ['-']. For
          example [HTTP_USER_AGENT] becomes {!Webs.Http.Headers.user_agent}.
          If defined the variables [HTTP_CONTENT_TYPE] and
          [HTTP_CONTENT_LENGTH] are respectively overriden by
          [CONTENT_TYPE] and [CONTENT_LENGTH] when present.}
       {- For any other [$VAR] in [extra_vars c] a header is added
          with the value of the variable. The header name of a
          variable is made by lowercasing it, mapping ['_'] to ['-']
          and prefixing the result with [x-cgi]. For example
          [SERVER_SOFTWARE] becomes [x-cgi-server-software].}}}
    {- {!Webs.Http.Request.val-body}, is the result of reading {!Unix.stdin}}}

    The connector otherwise adheres to the following conventions:

    {ul
    {- The
       {{!page-connector_conventions.service_requests}
       request values} conventions.}
    {- The
       {{!page-connector_conventions.service_responses}
       response value intepretation} conventions.}
    {- The {{!page-connector_conventions.service_connector_responses}
       connector responses} conventions.}}

    {b Note.} We require the non standard [URI_REQUEST] environment
    variable because recovering the value for
    {!Webs.Http.Request.raw_path} from the CGI [PATH_INFO] and
    [QUERY_STRING] variables is not really possible. Moreover
    [PATH_INFO] easily get confused by requests like
    ["/s1/s2%2Fha/s3"]. *)
