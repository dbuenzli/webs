(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {{:http://python.ca/scgi/protocol.txt}SCGI} connector.

    {b Important.} This connector uses two non-standard SCGI variables.
    {ul
     {- [REQUEST_URI], your web server must provide it in the SCGI variables.}
     {- [HTTPS], your web server must provide it in the SCGI variables
         with the value ["on"] if you want to work over HTTPS.}}

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

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
   Copyright (c) 2015 Daniel C. Bünzli

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
