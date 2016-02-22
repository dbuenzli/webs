(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** CGI connector.

    [Webs_cgi] is a CGI connector. {{!req}See how} {!Webs.req} values
    are derived from the CGI environment.

    {b Important.} This connector uses one non-standard CGI variable.
    {ul
    {- [REQUEST_URI]. Your web server must provide it in the CGI environment
       with the requested
      {{:http://tools.ietf.org/html/rfc7230#section-5.3.1}origin-form} value.}}

    {b References.}
    {ul
    {- D. Robinson et al.
       {{:http://tools.ietf.org/html/rfc3875}
       {e The Common Gateway Interface (CGI) Version 1.1}. 2004.}}}

    {e Release %%VERSION%% - %%MAINTAINER%% } *)

open Rresult
open Webs

(** {1:conf Configuration keys}

    Default configuration keys.

	  {ul
	  {- {!Conf.sendfile_header}}} *)

val vars : string list Hmap.key
(** [vars] is a list of CGI environment variables that are added
    to the request's {!Webs.Req.headers} in the request if they are
    defined. See {{!req}here} for details about how the header is
    named.

    The variables must be valid HTTP
    {{:https://tools.ietf.org/html/rfc7230#section-3.2.6}token}s,
    otherwise the connector will error. *)

(** {1 Connector} *)

val connect : Webs.connector

(** {1:req Requests}

	  A  {!Webs.req} value is constructed from the CGI environment as follows.
	  {ul
	  {- {!Webs.Req.meth} is the value of the
       {{:http://tools.ietf.org/html/rfc3875#section-4.3}[REQUEST_METHOD]}
       variable decoded by {!Webs.HTTP.decode_meth}.}
	  {- {!Webs.Req.path} is determined by cutting the (non standard)
       [REQUEST_URI] variable at the first ['?'] and applying
       {!Webs.HTTP.decode_path} on the left hand side.}
    {- {!Webs.Req.query} is determined by spitting the (non standard)
       [REQUEST_URI] variable at the first ['?'] and take the right hand
       side.}
	  {- {!Webs.Req.version} is the value of the
       {{:http://tools.ietf.org/html/rfc3875#section-4.1.16}[SERVER_PROTOCOL]}
       variable decoded by {!Webs.HTTP.decode_version}.}
	  {- {!Webs.Req.headers} has the following headers defined:
    {ul
       {- {!Webs.HTTP.H.content_type} if the variable
          {{:http://tools.ietf.org/html/rfc3875#section-4.1.3}[CONTENT_TYPE]}
          is defined and non empty.}
       {- {!Webs.HTTP.H.content_length} header is defined if the variable
          {{:http://tools.ietf.org/html/rfc3875#section-4.1.2}[CONTENT_LENGTH]}
          is defined and non empty.}
       {- For any other variable of the form [HTTP_$VAR] there is a
          corresponding [$VAR] header whose name is made by
          lowercasing [$VAR] and mapping ['_'] to ['-']. Note
          that aboth the variables [HTTP_CONTENT_TYPE] and
          [HTTP_CONTENT_LENGTH] are ignored if defined.}
       {- For any defined CGI environment variable mentioned in the
          connector's configuration key {!vars} a header is added.
          The header name corresponding to a
          variable is derived by lowercasing the CGI variable, map any ['_']
          character to ['-'] and prefix it with [x-cgi-]. For example the
          header name for [GATEWAY_INTERFACE] is
          [x-cgi-gateway-interface].}}}
    {- {!Webs.Req.body_len} is the value of
       {{:http://tools.ietf.org/html/rfc3875#section-4.1.2}[CONTENT_LENGTH]}
       decoded by {!Webs.HTTP.decode_digits} if defined and non empty.}
    {- {!Webs.Req.body}, is the result of reading {!stdin}}} *)

(** {1:resp Responses}

    The response status and headers are written on [stdout]. The response
    body is handled as follows:
    {ul
    {- For a stream body, it is simply output on [stdout].}
    {- For a file body. First a full absolute file path is determined by
       appending the relative file path of the body to the absolute
       path given in the environment variable ["DOCUMENT_ROOT"]. If
       this variable is absent or empty or if the resulting
       path escapes the ["DOCUMENT_ROOT"] this results in response
       is ignore and transformed to a 502.

       Then if the file range is [None] (i.e. the whole file) and
       {!Connector.sendfile_header} is specified. The filepath
       is returned in the given header (FIXME should be the original
       relative file ?).}}
*)
(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
