(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** CGI gateway connector.

    This connector serves one request via CGI.

    See the {{!page-web_service_howto}Web service howto} manual for
    instructions.

    {b Important.} Reconstructing the raw request target from the CGI
    [PATH_INFO] and [QUERY_STRING] variables is not really
    possible. Moreover [PATH_INFO] easily get confused by requests
    like ["/s1/s2%2Fha/s3"]. To side step this issue this connector
    relies on the non-standard [REQUEST_URI] variable in which the
    raw, url-encoded, request target should be passed. [nginx] passes
    that by default with the value of its [$request_uri] variable.

    {b References.}
    {ul
    {- D. Robinson et al.
       {{:http://tools.ietf.org/html/rfc3875}
       {e The Common Gateway Interface (CGI) Version 1.1}. 2004.}}} *)

open Webs

(** {1:connector Connector} *)

type t
(** The type for CGI connectors. Serves one request by reading headers
    from the process environment, reading the body from {!Unix.stdin} and
    writing the response on {!Unix.stdout}. *)

val create :
  ?extra_vars:string list -> ?log:(Connector.log_msg -> unit) ->
  ?max_req_body_byte_size:int -> ?service_path:Http.path -> unit -> t
(** [create ()] is a new CGI connector with the following parameters:
    {ul
    {- [extra_vars c] is the list of environment variables whose content
       is added to the request headers of requests handled by [c]. The header
       name corresponding to a variable is made by lowercasing it,
       mapping ['_'] to ['-'] and prefixing the result with [x-cgi].
       For example [SERVER_SOFTWARE] becomes [x-cgi-server-software]. Defaults
       to [[]]}
    {- [log] logs connector log messages. Defaults to
       {!Webs.Connector.default_log} with trace messages.}
    {- [max_req_body_byte_size] is the maximal request body size in bytes.
       FIXME not enforced.}
    {- [service_path] the path at which the root of the service of
       [c] is being served. This path is stripped from the path found in
       the request's target to yield the {!Webs.Req.path} of the request to
       serve. The connector responds with a {!Webs.Http.bad_request_400}
       if the strip fails. The value of the service path can also be
       found in the {!Webs.Req.service_path} of the request to serve.
       Defaults to [[""]].}} *)

val extra_vars : t -> string list
(** [extra_vars c] is the list of additional environment variables
    of [c]. See {!create}. *)

val log : t -> (Connector.log_msg -> unit)
(** [log c] is the log of [c]. See {!create}. *)

val max_req_body_byte_size : t -> int
(** [max_req_body_byte_size c] is the maximal request body size
    in bytes supported by [c]. See {!create}. *)

val service_path : t -> Http.path
(** [service_path c] is service path of [c]. See {!create}.  *)

(** {1:serving Serving} *)

val serve : t -> Webs.service -> (unit, string) result
(** [serve c s] runs service [s] with connector [c]. This blocks until
    the response of [s] for the request has been served. The error is
    returned in case of connector error, it's a good practice to write
    the message on stderr and exit with a non-zero exit code if that
    happens. See {{!req_derivation}here} to understand how the request
    value to serve is derived. *)

(** {1:req_derivation Request derivation}

	  The  {!Webs.Req.t} value is constructed from the environment and
    {!Unix.stdin} as follows:
    {ul
    {- {!Webs.Req.val-body}, is the result of reading {!Unix.stdin}}
    {- {!Webs.Req.body_length} is determined from the headers according to
       {!Webs.Http.Headers.request_body_length}.}
	  {- {!Webs.Req.headers} has the following headers defined:
       {ul
       {- {!Webs.Http.content_type} if the variable
          {{:http://tools.ietf.org/html/rfc3875#section-4.1.3}[CONTENT_TYPE]}
          is defined and non empty.}
       {- {!Webs.Http.content_length} if the variable
          {{:http://tools.ietf.org/html/rfc3875#section-4.1.2}[CONTENT_LENGTH]}
          is defined and non empty.}
       {- For any other variable of the form [HTTP_$VAR] there is a
          corresponding [$var] header whose name is made by
          lowercasing [$VAR] and mapping ['_'] to ['-']. For
          example [HTTP_USER_AGENT] becomes {!Webs.Http.user_agent}.
          If defined the variables [HTTP_CONTENT_TYPE] and
          [HTTP_CONTENT_LENGTH] are overriden by the previous two
          definitions.}
       {- For any other [$VAR] in [extra_vars] given when
          the connector is {!create}d, a header is added with the value
          of the variable. The header name
          of a variable is made by lowercasing it, mapping ['_'] to ['-']
          and prefixing the result with [x-cgi]. For example
          [SERVER_SOFTWARE] becomes [x-cgi-server-software].}}}
	  {- {!Webs.Req.meth} is the value of the
       {{:http://tools.ietf.org/html/rfc3875#section-4.3}[REQUEST_METHOD]}
       variable.}
    {- {!Webs.Req.path} is the path of the (non standard) [REQUEST_URI]
       variable, stripped by the connector's {!service_path}.}
    {- {!Webs.Req.query} is the query (if any) of the (non standard)
       [REQUEST_URI] variable.}
    {- {!Webs.Req.service_path} is the connectors's {!service_path}.}
	  {- {!Webs.Req.version} is the value of the
       {{:http://tools.ietf.org/html/rfc3875#section-4.1.16}[SERVER_PROTOCOL]}
       variable.}
    {- {!Webs.Req.request_target} is the value of the (non standard)
       [REQUEST_URI] variable.}}

    If the request derivation fails in some way an appropriate HTTP error
    response is written on {!Unix.stdout}.*)

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers

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
