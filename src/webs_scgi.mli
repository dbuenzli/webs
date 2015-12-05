(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {{:http://python.ca/scgi/protocol.txt}SCGI} connector.

    {b Important.} This connector uses two non-standard SCGI variables.
    {ul
     {- [REQUEST_URI], your web server must provide it in the SCGI variables.}
     {- [HTTPS], your web server must provide it in the SCGI variables
         with the value ["on"] if you want to work over HTTPS.}} *)

(** {1 Connector} *)

val connect : Webs.Connector.t

(** {1:conf Configuration keys}
	  {ul
	  {- {!Conf.sendfile_header}}
	  {- {!Conf.cgi_variables}}
	  {- {!Conf.listen}}}
	  {1 Web server interaction details}

	  {2:req Request}

        The connector listens for connections on the address specified
        by {!Conf.listen}.

	  The {!Service.request} is constructed as follows.
	  {ul
	  {- [request_protocol], the value of the [SERVER_PROTOCOL] variable.}
	  {- [request_method], the value of the [REQUEST_METHOD] variable.}
	  {- [request_uri], the value of the [REQUEST_URI] variable.}
	  {- [request_headers], the value of all the [HTTP_*] variables,
           are in a corresponding HTTP header, aswell as those mentionned
	  in {!Conf.cgi_variables}.}
	  {- [request_remote_addr], the value of the [REMOTE_ADDR] variable.}
	  {- [request_is_https] is [true] iff there is a [HTTPS] variable
	  with a value of ["on"] or ["true"].}}

	  The request body, if any, is read from the connection.

	  {2:resp Response}

	  The response writes the status and headers on the connection, followed
	  by the response body. *)

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
