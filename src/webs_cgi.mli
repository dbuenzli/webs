(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** CGI gateway connector.

    This connector serves one request via CGI.

    {b Important.} Reconstructing the raw request target from CGI's
    [PATH_INFO] and [QUERY_STRING] is not really possible and
    [PATH_INFO]s can easily get confused by requests like
    ["/s1/s2%2Fha/s3"]. To side step this issue this connector relies on the
    non-standard [REQUEST_URI] variable in which the raw url-encoded
    request target should be passed. [nginx] passes that by default
    with the value of [$request_uri], unfortunately it doesn't seem
    to be able to chop a prefix from that variable without getting
    url-decoded results. You can add the [REQUEST_TARGET_PREFIX] in
    the environment whose value will be chopped from [REQUEST_URI].

    See the {{!page-web_service_howto}Web service howto} manual for
    instructions.

    {b References.}
    {ul
    {- D. Robinson et al.
       {{:http://tools.ietf.org/html/rfc3875}
       {e The Common Gateway Interface (CGI) Version 1.1}. 2004.}}} *)

open Webs

(** {1:connector Connector} *)

type t
(** The type for CGI connectors. Serves one request by reading
    headers from the environment and the body from {!Unix.stdin}. *)

val create :
  ?log:(Connector.log_msg -> unit) -> ?max_req_body_byte_size:int ->
  ?extra_vars:string list -> unit -> t
(** [create ()] is a new CGI connector with parameters:
    {ul
    {- [extra_vars] is a list of environment variables whose content is
       added to the request headers (defaults to [[]]). The header name
       of a variable is made by lowercasing it, mapping ['_'] to ['-']
       and prefixing the result with [x-cgi]. For example
       [SERVER_SOFTWARE] becomes [x-cgi-server-software].}
    {- [max_req_body_byte_size] is the maximal request body size in bytes.
       FIXME not enforced, unclear where this is to put the limit on, for
       streaming bodies, if we cut the line the service might end up
       being confused (but then it should also cater for that possibility).}
    {- [log] logs connector log messages. It defaults
       {!Webs.Connector.default_log} with trace message disabled.}} *)

val serve : t -> Webs.service -> (unit, string) result
(** [serve c s] runs service [s] with connector [c]. This blocks until
    the response of [s] for the request has been served. The error is
    returned in case of connector error, it's a good practice to write
    the message on stderr and exit with a non-zero exit code if that
    happend. *)

(** {1:req_derivation Request derivation}

	  The  {!Webs.Req.t} value is constructed from the environment and
    {!Unix.stdin} as follows:
	  {ul
	  {- {!Webs.Req.version} is the value of the
       {{:http://tools.ietf.org/html/rfc3875#section-4.1.16}[SERVER_PROTOCOL]}
       variable.}
	  {- {!Webs.Req.meth} is the value of the
       {{:http://tools.ietf.org/html/rfc3875#section-4.3}[REQUEST_METHOD]}
       variable.}
    {- {!Webs.Req.request_target} is the value of the (non standard)
       [REQUEST_URI] variable, chopped from the value in the
       [REQUEST_TARGET_PREFIX] variable (if present).}
	  {- {!Webs.Req.headers} has the following headers defined:
    {ul
       {- {!Webs.Http.H.content_type} if the variable
          {{:http://tools.ietf.org/html/rfc3875#section-4.1.3}[CONTENT_TYPE]}
          is defined and non empty.}
       {- {!Webs.Http.H.content_length} if the variable
          {{:http://tools.ietf.org/html/rfc3875#section-4.1.2}[CONTENT_LENGTH]}
          is defined and non empty.}
       {- For any other variable of the form [HTTP_$VAR] there is a
          corresponding [$var] header whose name is made by
          lowercasing [$VAR] and mapping ['_'] to ['-']. For
          example [HTTP_USER_AGENT] becomes {!Webs.Http.H.user_agent}.
          If defined the variables [HTTP_CONTENT_TYPE] and
          [HTTP_CONTENT_LENGTH] are overriden by the previous two
          definitions.}
       {- For any other [$VAR] in [extra_vars] given when
          the connector is {!create}d, a header is added with the value
          of the variable. The header name
          of a variable is made by lowercasing it, mapping ['_'] to ['-']
          and prefixing the result with [x-cgi]. For example
          [SERVER_SOFTWARE] becomes [x-cgi-server-software].}}}
    {- {!Webs.Req.body_length} is determined from the headers according to
       {!Webs.Http.H.request_body_length}.}
    {- {!Webs.Req.body}, is the result of reading {!Unix.stdin}}}

    If the request derivation fails in some way an 500 is returned and
    an error should be printed on standard error. *)

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
