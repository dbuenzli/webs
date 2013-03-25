(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Web server interface. 

    [Webs] defines a generic and {e low-level} interface between 
    web servers and services implemented in OCaml.

    The interaction between the {{!Service}service} and the web server
    is mediated by a {{!Connector}connector}.  [Webs] provides CGI and
    SCGI connectors. New connectors can be defined by implementing
    this {{!Connector.T}signature}.

    Consult the {{!basics}basics} and {{!examples}examples} of use.
    Open the module to use it, it defines only three modules in your
    scope.

    {e Release %%VERSION%% - %%AUTHORS%% } *)


(** {1:top Interface} *)

(** HTTP datatypes and constants. 

    {b Note.} Pretty printers are for debugging purpose. *)
module HTTP : sig

(*
  (** {1 Versions} *)

  type version = int * int 
  (** The type for HTTP versions. *)

  val pp_version : Format.formatter -> version -> unit
  (** [pp_version ppf v] prints [v] on [ppf]. *)

  (** {2:vcst Constants} *)

  val v0_9 : version 
  val v1_0 : version 
  val v1_1 : version 

  (** {1 Methods} *)

  type method_ = 
      [ `GET | `POST | `HEAD | `PUT | `DELETE | `TRACE | `CONNECT | `OPTIONS ]
  (** The type for standard HTTP methods. *)

  val method_of_string : string -> [ `Method of method_ | `Extension of string ]
  (** [method_of_string s] is [(`Ok m)] if [s] is the HTTP method [m] or 
      [(`Unknown s)] otherwise. *)

  val method_to_string : method_ -> string 
  (** [method_to_string m] is the HTTP method [m] as a string. *)

  val pp_method : Format.formatter -> method_ -> unit
  (** [pp_method ppf m] prints [m] on [ppf]. *)
*)
  (** {1 Headers} *)

  type header_name = string
  (** The type for (lowercase) HTTP header names.

      TODO.
      Non empty string with characters in 0x21, 0x23-0x27, 0x2a-0x2b, 
      0x2d-0x2e, 0x30-0x39, 0x41-0x5a, 0x5e-0x7a, 0x7c, 0x7e, 0x7f *)

  type header = header_name * string 
  (** The type for HTTP headers. *)

  val h : header_name -> string -> header
  (** [h n v] is the header [(n, v)]. *)

  val pp_header : Format.formatter -> header -> unit 
  (** [pp_header ppf h] prints [h] on [ppf]. *)

  (** {2:hcst Constants} *)

  val accept : header_name
  val accept_charset : header_name
  val accept_encoding : header_name
  val accept_language : header_name
  val accept_ranges : header_name
  val age : header_name
  val allow : header_name
  val authorization : header_name
  val cache_control : header_name
  val connection : header_name
  val content_encoding : header_name
  val content_language : header_name
  val content_length : header_name
  val content_location : header_name
  val content_md5 : header_name
  val content_range : header_name
  val content_type : header_name
  val content_version : header_name
  val cookie : header_name
  val date : header_name
  val etag : header_name
  val expect : header_name
  val expires : header_name
  val from : header_name
  val host : header_name
  val if_match : header_name
  val if_modified_since : header_name
  val if_none_match : header_name
  val if_range : header_name
  val if_unmodified_since : header_name
  val last_modified : header_name
  val location : header_name
  val max_forwards : header_name
  val pragma : header_name
  val proxy_authenticate : header_name
  val proxy_authorization : header_name
  val range : header_name
  val referer : header_name
  val retry_after : header_name
  val set_cookie : header_name
  val trailer : header_name
  val transfer_encoding : header_name
  val upgrade : header_name
  val user_agent : header_name
  val vary : header_name
  val via : header_name
  val warning : header_name
  val www_authenticate : header_name
  
  (** {1 Status codes} *)

  type status = int
  (** The type for HTTP status codes. *)

  val status_reason_phrase : status -> string 
  (** [status_reason_phrase s] is [s]'s reason phrase. *)

  val pp_status : Format.formatter -> status -> unit
  (** [pp_status reason ppf s] prints [s] and its reason phrase on [ppf]. *)

  (** {2:scst Constants} 

      Each status is followed by an equivalent status 
      named by the reason phrase. *)

  val s_100 : status val s_continue : status  
  val s_101 : status val s_switching_protocols : status 
  val s_200 : status val s_ok : status
  val s_201 : status val s_created : status
  val s_202 : status val s_accepted : status 
  val s_203 : status val s_non_authoritative_information : status 
  val s_204 : status val s_no_content : status
  val s_205 : status val s_reset_content : status 
  val s_206 : status val s_partial_content : status
  val s_300 : status val s_multiple_choices : status 
  val s_301 : status val s_moved_permanently : status
  val s_302 : status val s_found : status
  val s_303 : status val s_see_other : status
  val s_304 : status val s_not_modified : status
  val s_305 : status val s_use_proxy : status
  val s_307 : status val s_temporary_redirect : status
  val s_400 : status val s_bad_request : status    
  val s_401 : status val s_unauthorized : status    
  val s_402 : status val s_payement_required : status
  val s_403 : status val s_forbidden : status    
  val s_404 : status val s_not_found : status    
  val s_405 : status val s_not_allowed : status
  val s_406 : status val s_not_acceptable : status 
  val s_407 : status val s_proxy_authentication_required : status
  val s_408 : status val s_request_time_out : status 
  val s_409 : status val s_conflict : status 
  val s_410 : status val s_gone : status 
  val s_411 : status val s_length_required : status 
  val s_412 : status val s_precondition_failed : status    
  val s_413 : status val s_request_entity_too_large : status 
  val s_414 : status val s_request_uri_too_large  : status  
  val s_415 : status val s_unsupported_media_type : status 
  val s_416 : status val s_requested_range_not_satisfiable : status    
  val s_417 : status val s_expectation_failed : status 
  val s_500 : status val s_server_error : status
  val s_501 : status val s_not_implemented : status 
  val s_502 : status val s_bad_gateway : status   
  val s_503 : status val s_service_unavailable : status 
  val s_504 : status val s_gateway_time_out : status   
  val s_505 : status val s_http_version_not_supported : status 
end

(** Services.  

    A service maps a {{!request}request} from the web server
    to a {{!response}response}.

    {b TODO} integration with asynchronous libraries. 
*)
module Service : sig

(** {1 Requests}  *)

type request = {
      request_remote_addr : string; 
      (** The network address of the client sending the request. *)
      request_protocol : string;
      (** The {{:http://tools.ietf.org/html/rfc2616#section-3.1}
	  HTTP version} of the protocol used by the client. *) 
      request_is_https : bool; 
      (** [true] if the request is made over HTTPS. *)
      request_method : string;
      (** The 
	  {{:http://tools.ietf.org/html/rfc2616#section-5.1.1}HTTP
	  method}. *) 
      request_uri : string; 
      (** The {{:http://tools.ietf.org/html/rfc2616#section-5.1.2}HTTP request
	  URI} with this syntax: [abs_path \[ '?' query\]]. *)
      request_headers : HTTP.header list;
      (** The HTTP headers of the request. Includes at least 
	  the {!HTTP.host} header. *)
  }
(** The type for requests. *)

  type request_body = (string * int * Unix.file_descr) option
  (** The type for request bodies. Content type, content length, 
    fd to read the body from. TODO asynchronous libraries *)

  (** {1 Responses} *)

  type response_body = [
    | `R_enum of Unix.file_descr -> unit
    | `R_builder of string
    | `R_file of string * (int * int) option ]
  (** The type for response bodies. 
      TODO enumerator. *)
	
  type response = HTTP.status * HTTP.header list * response_body
    
  (** {1 Services} *)

  type t = request -> request_body -> response
  (** The type for services. 

      Note that services must be thread-safe. 
      {b TODO} there's a lot to say about asynchrony etc. *)

  val echo : t
  (** [echo] is a service that returns the request as a [text/plain] document. 
   *)

  (** {1 Higher-order services} *)

(* 
  val dump_response t -> t 
  val dump_service_exn t -> t 
*)
end

(** Web server connectors. *)
module Connector : sig

  (** {1 Connectors}  *)

  (** Connector configuration. 

      A connector configuration is a set of {{!keys}keys} mapping to
      typed values. Connectors can define their own keys but
      should favor {{!std}standard} ones if appropriate.
   *)
  module Conf : sig 

    (** {1:keys Keys} *)

    type 'a key 
    (** The type for configuration keys whose lookup value is ['a]. *)

    val key : unit -> 'a key
    (** [key ()] is a new configuration key. *)
	  
    (** {1:conf Configurations} *)

    type t 
    (** The type for configurations. *)

    val empty : t
    (** [empty] is the empty configuration. *)

    val is_empty : t -> bool 
    (** [is_empty c] is [true] iff [c] is empty. *) 
	    
    val mem : t -> 'a key -> bool
    (** [mem c k] is [true] iff [k] has a mapping in [c]. *)

    val add : t -> 'a key -> 'a -> t
    (** [add c k v] is [c] with [k] mapping to [v]. *)
	
    val rem : t -> 'a key -> t
    (** [rem c k] is [c] with [k] unbound. *)
	
    val find : t -> 'a key -> 'a option
    (** [find c k] is [k]'s mapping in [c], if any. *)
	
    val get : t -> 'a key -> 'a
    (** [get c k] is [k]'s mapping in [c]. 
       
	{b Raises.} [Invalid_argument] if [k] is not bound in [d]. *)

    (** {1:std Standard keys} *)

    val listen : [ `Addr of Unix.sockaddr | `Fd of Unix.file_descr ] key 
    (** [listen] is a key defining where the connector should 
	listen for connections from the web server. 
	
	{b Purpose.} If a connector supports this key it will listen for 
	requests on the provided connection specification. *)

    val sendfile_header : string key
    (** [sendfile_header] is key defining a header name. 

	{b Purpose.} If a connector supports this key, file response bodies are
	not handled by the connector. The filename is returned to the
	web server in this header. Use for example ["x-accel-redirect"] for 
	{{:nginx.org}nginx} or ["x-sendfile"] for Apache with 
	{{:https://tn123.org/mod_xsendfile/}mod_xsendfile}
	and {{:http://www.lighttpd.net/}Lightppd}. *)

    val service_exn_log : Format.formatter key 
    (** [error_log] is a key defining a formatter to log service errors. 

        {b Purpose.} If a connector supports this key, it must log
	uncaught service exceptions to the given formatter. *)

    val cgi_variables : [ `All | `Vars of string list ] key
    (** [cgi_variables] is a key to ask to include specific CGI variables
        in the request headers.

	{b Important.} Use of this key may hinder compatibility of your
	service with other connectors.

	{b Purpose.} If a connector supports this key, it must include
	the value of all ([`All]) or some ([`Vars]) CGI variables in
	the request headers if present.  Each variable is in a header
	named as follows: lowercase the variable, 
	map any ['_']
	character to ['-'] and prefix it with
	[x-cgi-]. For example the header name for 
	[GATEWAY_INTERFACE] is [x-cgi-gateway-interface]. *)

    (** {1:dconfig Default configuration} *)

    val default : t
    (** [default] is a configuration that has the following
	keys :
	{ul 
	{- {!listen} with [`Addr ("localhost", 9298)]}} TODO. *)

  end

  type error = [ 
  | `Connector of string | `Webserver of string | `Service of exn ]
  (** The type for connector errors. *)

  (** Signature implemented by all connectors. *)
  module type T = sig
    
    (** {1 Interface} *)

    val connect : Conf.t -> Service.t -> [ `Error of error | `Ok]
    (** [connect c s] configures the connector with [c]
	and installs the service [s].

	{b Implementation duties.} The implementation 
	of [connect] should satisfy the following constraints.
	{ul 
	{- If the connector uses {{!Conf.std}standard} configuration
	   keys it must respect their semantics.}
	{- The [connect] function must never raise an exception.}
	{- If a service raises an exception it must be caught. The 
	   connector should then try to respond to the web server 
	   with a {!HTTP.s_server_error}. The connector should 
	   try to log the uncaught exception, for example by using
	   the {!Conf.service_exn_log} key.}
	{- If a configuration, connector or web server connection 
	   error the function should return with [`Error] with an 
	   exception describing the error condition.}
	{- If the connector has an implementation dependent way of 
	   closing the connection with the web server 
	   it should return from the function and return [`Ok].}
	{- A connector should not return [`Ok] unless it has no 
	   initiated service requests.}} *)
  end


  (** {{:http://tools.ietf.org/html/rfc3875}CGI} connector.
	
      {b Important.} This connector uses two non-standard CGI variables.
      {ul
      {- [REQUEST_URI], your web server must provide it in the CGI environment.}
      {- [HTTPS], your web server must provide it in the CGI environment 
         with the value ["on"] if you want to work over HTTPS.}} *)
  module CGI : sig 
   
    (** {1 Connector} *)

    val connect : Conf.t -> Service.t -> [ `Error of error | `Ok ] 
    
    (** {1:conf Configuration keys} 

	{ul
	{- {!Conf.sendfile_header}}
	{- {!Conf.cgi_variables}}}
	{1:req Web server interaction details}
	
	{2:req Request}

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

	The request body, if any, is read from [stdin].
	

	{2:resp Response} 
      
	The response writes the status and headers on [stdout], followed
	by the response body. *)
  end

  (** {{:http://python.ca/scgi/protocol.txt}SCGI} connector.

      {b Important.} This connector uses two non-standard SCGI variables.
      {ul
      {- [REQUEST_URI], your web server must provide it in the SCGI variables.}
      {- [HTTPS], your web server must provide it in the SCGI variables
         with the value ["on"] if you want to work over HTTPS.}}
  *)
  module SCGI : sig 

    (** {1 Connector} *)

    val connect : Conf.t -> Service.t -> [ `Error of error | `Ok ] 
    
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
  end


  (** {1 Implementings new connectors.} *)

  (** {1 Private functions for implementing connectors.} *)
  module Private : sig end
end

(** {1:basics Basics} *)

(** {2 TODO} 

    Content-length Content-type in request do we put them in the headers ? 
    Why replicate the info, we have a body type with it already parsed.
*)


(** {2:misc Important points}
  {ul 
    {- [Wsi] assumes that HTTP header names are lowercase.
        No checks are performed but services and connectors {b must} provide 
        lower cased header names to ensure interoperability.}
    {- [Wsi] assumes strings are immutable.}} *)

(** {1:examples Examples} 

    {2:scgi Simple service via SCGI}

    In this example we make our service available to the web
    server via SCGI with the {{!Connector.Conf.default}default} configuration. 
{[
open Wsi;;

let handler _ = HTTP.(s_OK, [h content_type "text/plain"], build "Revolt!")

let () = match Connector.(SCGI.run Conf.default handler) with 
  | `Ok -> exit 0
  | `Error exn -> ...
]}
   

   {2:timing Timing the response.}
   
   Forward req and analyse return headers, if 
   content-type = text/html, add comment with time. 
*)


(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli
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

   3. Neither the name of the Daniel C. Bünzli nor the names of
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






    
(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli
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
