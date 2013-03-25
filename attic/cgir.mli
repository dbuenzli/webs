(*----------------------------------------------------------------------------
   Copyright (c) 2007 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   Cgir version %%VERSION%%
  ----------------------------------------------------------------------------*)

(** Uniform CGI and SCGI interface.

    Cgir provides a uniform interface to CGI and SCGI requests. It is
    intentionally low level, clients are expected to extract the
    information they need into more structured types and quickly get
    rid of this representation.

    Consult the {{:#fl}features and limitations} and an {{:#ex}example}
    of use.

    {e Version %%VERSION%% - %%EMAIL%% }

    {b Note.} This document uses the term {e header} for what the CGI
    specification calls an environment variable.

    {b References.}

    NCSA Software Development Group.
    {e {{:http://hoohoo.ncsa.uiuc.edu/cgi/interface.html}
       The CGI specification}.}
  
    Neil Schemenauer. 
    {e {{:http://python.ca/scgi/protocol.txt}
       SCGI: A Simple Common Gateway Interface alternative}}. 2008
*)


(** {1 Basic types and values} *)

val cgi_headers : string list
(** [cgi_headers] is the list of default
    {{:http://hoohoo.ncsa.uiuc.edu/cgi/env.html}CGI header names}. *)

val http_headers : string list 
(** [http_headers] is the list of 
    {{:http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.3}HTTP 
    request header names}. *)

type t
(** The type for (S)CGI requests. *)

type body = string option * int * in_channel
(** The type for request bodies. The content type,
    the content length ([> 0]), and the channel to read from. *)

val header : t -> string -> string option
(** [header r h] is the value of the header [h] in the request [r]
    or [None] if the header is absent. *)

val headers : t -> (string * string) list 
(** [headers r] is, for SCGI requests, the list of headers transmitted by 
    the server and, for CGI requests, the headers of {!cgi_headers} and 
    {!http_headers} that are present. *)

val body : t -> body option
(** [body r] is the request's body if present. More details 
    about {{:#body}request bodies}. *)

val input_cgi : unit -> [ `Ok of t | `Error of string ]
(** [input_cgi ()] returns a request if the program was invoked 
    as a CGI script. 
    More details about {{:#errors} errors}. *)

val input_scgi : in_channel -> [ `Ok of t | `Error of string ]
(** [input_scgi ic] parses an SCGI request from [ic]. 
    More details about {{:#errors} errors}.*)

val print : ?body:bool -> ?headers:string list -> 
  Format.formatter -> t -> unit
(** [print body headers fmt r] prints the request [r] on [fmt].
    If [body] is [false] doesn't
    print the body (defaults to [true]).

    If [headers] specifies a list of header names, only those are
    printed.  If [headers] is unspecified, the {!cgi_headers} and
    {!http_headers} are printed for CGI requests, and the headers
    transmitted by the server for SCGI ones.

    {b Raises.} [End_of_file] if the body of the request is not
    long enough. *)

val to_string : ?body:bool -> ?headers:string list -> t -> string
(** See {!print}. *)

(** The following function can be used to simulate (possibly broken) 
    SCGI clients. *)

val output_scgi : out_channel -> (string * string) list -> unit
(** [scgi_output headers oc], outputs a SCGI request with headers
    [headers] on [oc]. 

    To generate a valid request. The first header {e must} be
    [("CONTENT_LENGTH", size)] where [size] is the length of the body ([0] 
    for empty bodies), the header [("SCGI", "1")] must appear in [headers],
    and after the call to this function at least [size] bytes must be
    written on [oc] to terminate the request. *)

(** {1:fl Features and limitations} 

    {2:errors Request errors}

    Cgir is liberal in determining what constitutes a valid request.
    Errors are returned only in the following cases.
    {ul
    {- In all requests if the [CONTENT_LENGTH] header value does not parse 
       to an [int].}
    {- In SCGI requests, if the input channel does not follow the SCGI
       protocol or if the 
       {{:http://cr.yp.to/proto/netstrings.txt}netstring}'s size 
       containing the headers exceeds 
       [Sys.max_string_length].}}
    {2:body Request body}

    Cgir doesn't look at the [REQUEST_METHOD] header to determine the 
    presence of a request body. The request has a body iff 
    there is a header [CONTENT_LENGTH] whose value is strictly greater than 
    [0].

    The content type of the body is determined by the [CONTENT_TYPE] header
    if present.
    {2 Channels}
    
    The module only reads and writes from/to channels, it doesn't close 
    or flush them.

    {1:ex Example}

    The following program can be used both as a CGI script and a 
    SCGI server. In practice you certainly don't want to use 
    [Unix.establish_server] for SCGI since it forks all requests,
    use a thread or process pool.
{[
let service req oc = match req with
  | `Ok req -> ...
  | `Error e -> ...

let serve = function
  | `CGI -> service (Cgir.input_cgi ()) stdout 
  | `SCGI (host, port) -> 
    let addr = 
      try (Unix.gethostbyname host).Unix.h_addr_list.(0) with 
      | Not_found -> failwith (Printf.sprintf "%s: unknown host." host)
    in 
    let saddr = Unix.ADDR_INET (addr, port) in
    let service ic oc = service (Cgir.input_scgi ic) oc in 
    Unix.establish_server service saddr
      
let main () = 
  let exec = Filename.basename Sys.executable_name in
  let pr_err fmt = Printf.eprintf ("%s:" ^^ fmt ^^ "%!") exec in
  let usage = Printf.sprintf "Usage: %s <option>\nOptions:" exec in
  let protocol = ref `CGI in
  let host = ref "localhost" in
  let port = ref 9999 in
  let options = [
    "-cgi", Arg.Unit (fun () -> protocol := `CGI),
    "act as a CGI script (default).";
    "-scgi", Arg.Unit (fun () -> protocol := `SCGI),
    "act as a SCGI server on <host>:<port>.";
    "-h", Arg.Set_string host,
    "<host>, host for SCGI server (defaults to localhost).";
    "-p", Arg.Set_int port, 
    "<int>, port for SCGI server (defaults to 9999)."; ]
  in
  try 
    Arg.parse options (fun _ -> ()) usage;
    match !protocol with 
    | `CGI -> serve `CGI
    | `SCGI -> serve (`SCGI (!host, !port))
  with 
  | Failure e -> (pr_err " %s\n" e; exit 1)
  | Unix.Unix_error (e, f, v) -> 
      let v = if v = "" then "" else Printf.sprintf " on %s" v in 
      pr_err " %s%s: %s\n" f v (Unix.error_message e); exit 1

let () = main ()
]}
*)

(*----------------------------------------------------------------------------
  Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:
        
  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the
     distribution.

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
  ----------------------------------------------------------------------------*)
