(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let str = Printf.sprintf 

let err_conf_key_unbound = "key unbound in configuration"
let err_cgi_undefined_var v = str "CGI variable %s undefined" v
let err_cgi_content_length v = str "invalid CONTENT_LENGTH: %s" v 

module HTTP = struct
  type version = int * int 
  let pp_version ppf (b, s) = Format.fprintf ppf "%d.%d" b s 
  let v0_9 = (0, 9) 
  let v1_0 = (1, 0)
  let v1_1 = (1, 1)
   
  type method_ = 
      [ `GET | `POST | `HEAD | `PUT | `DELETE | `TRACE | `CONNECT | `OPTIONS ]

  let method_of_string = function 
    | "GET" ->  `Method `GET  | "POST" ->  `Method `POST
    | "HEAD" -> `Method `HEAD | "PUT" -> `Method `PUT
    | "DELETE" -> `Method `DELETE | "CONNECT" -> `Method `CONNECT
    | "TRACE" -> `Method `TRACE | "OPTIONS" -> `Method `OPTIONS
    | s -> `Extension s
   
  let method_to_string = function 
    | `GET -> "GET" | `POST -> "POST" | `HEAD -> "HEAD" | `PUT -> "PUT" 
    | `DELETE -> "DELETE" | `TRACE -> "TRACE" | `CONNECT -> "CONNECT" 
    | `OPTIONS -> "OPTION"
  
  let pp_method ppf m = Format.pp_print_string ppf (method_to_string m)

  type header_name = string 
  type header = header_name * string
  let pp_header ppf (n, v) = Format.fprintf ppf "@[%s = @ \"%s\"@]" n v
  let h n v = (n, v)
  let accept = "accept"
  let accept_charset = "accept-charset"
  let accept_encoding = "accept-encoding"
  let accept_language = "accept-language"
  let accept_ranges = "accept-ranges"
  let age = "age"
  let allow = "allow"
  let authorization = "authorization"
  let cache_control = "cache-control"
  let connection = "connection"
  let content_encoding = "content-encoding"
  let content_language = "content-language"
  let content_length = "content-length"
  let content_location = "content-location"
  let content_md5 = "content-md5"
  let content_range = "content-range"
  let content_type = "content-type"
  let content_version = "content-version"
  let cookie = "cookie"
  let date = "date"
  let etag = "etag"
  let expect = "expect"
  let expires = "expires"
  let from = "from"
  let host = "host"
  let if_match = "if-match"
  let if_modified_since = "if-modified-since"
  let if_none_match = "if-none-match"
  let if_range = "if-range"
  let if_unmodified_since = "if-unmodified-since"
  let last_modified = "last-modified"
  let location = "location"
  let max_forwards = "max-forwards"
  let pragma = "pragma"
  let proxy_authenticate = "proxy-authenticate"
  let proxy_authorization = "proxy-authorization"
  let range = "range"
  let referer = "referer"
  let retry_after = "retry-after"
  let set_cookie = "set-cookie"
  let trailer = "trailer"
  let transfer_encoding = "transfer-encoding"
  let upgrade = "upgrade"
  let user_agent = "user-agent"
  let vary = "vary"
  let via = "via"
  let warning = "warning"
  let www_authenticate = "www-authenticate"
      
  type status = int
  let status_reason_phrase = function 
    | 100 -> "Continue" | 101 -> "Switching Protocols"
    (* 2XX *)
    | 200 -> "OK" | 201 -> "Created" | 202 -> "Accepted"
    | 203 -> "Non-Authoritative Information" | 204 -> "No Content"
    | 205 -> "Reset Content" | 206 -> "Partial Content"
    (* 3XX *)
    | 300 -> "Multiple Choices" | 301 -> "Moved Permanently" | 302 -> "Found"
    | 303 -> "See Other" | 304 -> "Not Modified" | 305 -> "Use Proxy"
    | 307 -> "Temporary Redirect" 
    (* 4XX *)
    | 400 -> "Bad Request" | 401 -> "Unauthorized" | 402 -> "Payment Required" 
    | 403 -> "Forbidden" | 404 -> "Not Found" | 405 -> "Method Not Allowed" 
    | 406 -> "Not Acceptable" | 407 -> "Proxy Authentication Required" 
    | 408 -> "Request Time-out" | 409 -> "Conflict" | 410 -> "Gone"
    | 411 -> "Length Required" | 412 -> "Precondition Failed"
    | 413 -> "Request Entity Too Large" | 414 -> "Request-URI Too Large"
    | 415 -> "Unsupported Media Type" | 416 -> "Requested range not satisfiable"
    | 417 -> "Expectation Failed"
    (* 5XX *)
    | 500 -> "Internal Server Error" | 501 -> "Not Implemented"
    | 502 -> "Bad Gateway" | 503 -> "Service Unavailable"
    | 504 -> "Gateway Time-out" | 505 -> "HTTP Version not supported"
    (* XXX *)
    | s -> "Unknown extension"

  let pp_status ppf s = Format.fprintf ppf "%d %s" s (status_reason_phrase s)
  let s_100 = 100 let s_continue = s_100
  let s_101 = 101 let s_switching_protocols = s_101
  let s_200 = 200 let s_ok = s_200
  let s_201 = 201 let s_created = s_201
  let s_202 = 202 let s_accepted = s_202
  let s_203 = 203 let s_non_authoritative_information = s_203
  let s_204 = 204 let s_no_content = s_204
  let s_205 = 205 let s_reset_content = s_205
  let s_206 = 206 let s_partial_content = s_206
  let s_300 = 300 let s_multiple_choices = s_300
  let s_301 = 301 let s_moved_permanently = s_301
  let s_302 = 302 let s_found = s_302
  let s_303 = 303 let s_see_other = s_303
  let s_304 = 304 let s_not_modified = s_304
  let s_305 = 305 let s_use_proxy = s_305
  let s_307 = 307 let s_temporary_redirect = s_307
  let s_400 = 400 let s_bad_request = s_400
  let s_401 = 401 let s_unauthorized = s_401
  let s_402 = 402 let s_payement_required = s_402
  let s_403 = 403 let s_forbidden = s_403
  let s_404 = 404 let s_not_found = s_404
  let s_405 = 405 let s_not_allowed = s_405
  let s_406 = 406 let s_not_acceptable = s_406
  let s_407 = 407 let s_proxy_authentication_required = s_407
  let s_408 = 408 let s_request_time_out = s_408
  let s_409 = 409 let s_conflict = s_409
  let s_410 = 410 let s_gone = s_410
  let s_411 = 411 let s_length_required = s_411
  let s_412 = 412 let s_precondition_failed = s_412  
  let s_413 = 413 let s_request_entity_too_large = s_413
  let s_414 = 414 let s_request_uri_too_large = s_414
  let s_415 = 415 let s_unsupported_media_type = s_415
  let s_416 = 416 let s_requested_range_not_satisfiable = s_416
  let s_417 = 417 let s_expectation_failed = s_417
  let s_500 = 500 let s_server_error = s_500
  let s_501 = 501 let s_not_implemented = s_501
  let s_502 = 502 let s_bad_gateway = s_502
  let s_503 = 503 let s_service_unavailable = s_503
  let s_504 = 504 let s_gateway_time_out = s_504
  let s_505 = 505 let s_http_version_not_supported = s_505
end

module Service = struct
  type request = 
    { request_remote_addr : string; 
      request_protocol : string;
      request_is_https : bool; 
      request_method : string;
      request_uri : string; 
      request_headers : HTTP.header list; }
      
  type request_body = (string * int * Unix.file_descr) option
    
  type response_body = [
  | `R_enum of Unix.file_descr -> unit
  | `R_builder of string
  | `R_file of string * (int * int) option ]
	
  type response = HTTP.status * HTTP.header list * response_body    
  type t = request -> request_body -> response

  let echo r body = 
    let b = Buffer.create 512 in 
    let add = Buffer.add_string in
    let add_header (n, v) = 
      add b "\n "; add b n; add b " = \""; add b v; add b "\"";
    in
    add b "request_remote_addr = \""; add b r.request_remote_addr;
    add b "\"\nrequest_protocol = \""; add b r.request_protocol;
    add b "\"\nrequest_is_https = "; add b (string_of_bool r.request_is_https);
    add b "\nrequest_method = \""; add b r.request_method;
    add b "\"\nrequest_uri = \""; add b r.request_uri;
    add b "\"\nrequest_headers = ";
    List.iter add_header (List.sort compare r.request_headers);
    add b "\nrequest_body = ";
    begin match body with
    | None -> add b "None"
    | Some (t, l, _) -> 
	add b "\""; add b t; add b "\", "; 
	add b (string_of_int l); add b " bytes\n";
	add b " TODO print body\n";
    end;
    HTTP.(s_ok, [h content_type "text/plain"], `R_builder (Buffer.contents b))
    

  let debug fmt r = failwith "TODO"
end

(* Webserver connectors. *)

module Connector = struct

  (* Configurations, see http://mlton.org/PropertyList *)

  module Conf = struct    
    module Id = struct
      type t = int
      let create () = Oo.id (object end)                 (* thread-safe UID. *)
      let compare : int -> int -> int = compare
    end
	
    module M = (Map.Make (Id) : Map.S with type key = Id.t)
    type t = exn M.t
    type 'a key = Id.t * ('a -> exn) * (exn -> 'a option)
          
    let key () (type v) =
      let module M = struct exception E of v end in
      Id.create (), (fun x -> M.E x), (function M.E x -> Some x | _ -> None)
	
    let compare = M.compare compare 
    let equal = M.equal ( = )      
    let empty = M.empty 
    let is_empty = M.is_empty
    let mem d (id, _, _ ) = M.mem id d
    let add d (id, inj, _) v = M.add id (inj v) d
    let rem d (id, _, _) = M.remove id d
    let find d (id, _, proj) = try proj (M.find id d) with Not_found -> None
    let get d (id, _, proj) =
      try match proj (M.find id d) with Some v -> v | None -> raise Not_found 
      with Not_found -> invalid_arg err_conf_key_unbound
	  
    let listen = key () 
    let sendfile_header = key () 
    let service_exn_log = key () 
    let cgi_variables = key () 

    let default_listen = `Addr (Unix.ADDR_INET (Unix.inet6_addr_loopback, 9298))
    let default = add empty listen default_listen
  end

  (* Connectors *)

  type error = [ `Connector of string | `Webserver of string | `Service of exn ]

  module type T = sig 
    val connect : Conf.t -> Service.t -> [ `Error of error | `Ok ] 
  end
  
  module Private = struct 
    let sendfile f range = failwith "TODO"
  end

  module CGI = struct 
    let split_right s pos =         (* substring after pos (pos valid in s). *) 
      let start = pos + 1 in String.sub s start (String.length s - start)
	
    let normalize_name n =                  (* lowercase and map '_' to '-'. *) 
      for i = 0 to String.length n - 1 do 
	if n.[i] = '_' then n.[i] <- '-' else n.[i] <- Char.lowercase n.[i]
      done
	
    let get_http_header n =        (* if possible strip HTTP_ and normalize. *)
      let l = String.length n in 
      if l < 6 then None else
      if n.[0] = 'H' && n.[1] = 'T' && n.[2] = 'T' && n.[3] = 'P' && n.[4] = '_'
      then let s = String.sub n 5 (l - 5) in (normalize_name s; Some s) 
      else None
	  
    let parse_request cgi_vars = 
      let get_required_var r = match !r with `Value v -> v 
      | `Undefined var -> failwith (err_cgi_undefined_var var) 
      in
      let server_protocol = ref (`Undefined "SERVER_PROTOCOL") in 
      let request_method = ref (`Undefined "REQUEST_METHOD") in 
      let request_uri = ref (`Undefined "REQUEST_URI") in 
      let remote_addr = ref (`Undefined "REMOTE_ADDR") in 
      let content_length = ref None in 
      let content_type = ref None in
      let https = ref None in
      let headers = ref [] in
      let parse_vars s = 
	let pos = String.index s '=' in match String.sub s 0 pos with 
	| "SERVER_PROTOCOL" -> server_protocol := `Value (split_right s pos)
	| "REQUEST_METHOD" -> request_method := `Value (split_right s pos)
	| "REQUEST_URI" -> request_uri := `Value (split_right s pos) 
	| "REMOTE_ADDR" -> remote_addr := `Value (split_right s pos) 
	| "CONTENT_LENGTH" -> content_length := Some (split_right s pos) 
	| "CONTENT_TYPE" -> content_type := Some (split_right s pos) 
	| "HTTPS" -> https := Some (split_right s pos)
	| name -> match get_http_header name with
	  | Some n -> headers := (n, split_right s pos) :: !headers
	  | None ->
	      let drop_it = match cgi_vars with `All -> false
	      | `Vars l when List.mem name l -> false | `Vars _ -> true
	      in
	      if drop_it then () else 
	      let n = "x-cgi-" ^ (normalize_name name; name) in
	      headers := (n, split_right s pos) :: !headers
      in
      Array.iter parse_vars (Unix.environment ());
      let body = match !content_length with None -> None | Some len ->
	let length = try int_of_string len with 
	| Failure _ -> failwith (err_cgi_content_length len)
	in
	if length = 0 then None else
	let content_type = match !content_type with Some c -> c 
	| None -> "application/octet-stream"
	in
	Some (content_type, length, Unix.stdin)
      in
      { Service.request_protocol = get_required_var server_protocol;
	request_method = get_required_var request_method;
	request_uri = get_required_var request_uri;
	request_headers = !headers; 
	request_remote_addr = get_required_var remote_addr; 
	request_is_https = match !https with 
	| Some ("on" | "true") -> true
	| None | Some _ -> false }, body
      
    let pr_header (n, v) = Printf.printf "%s: %s\x0D\x0A" n v
	
    let connect c service =
      try
	let cgi_vars = match Conf.find c Conf.cgi_variables with 
	| None -> `Vars [] | Some vars -> vars
	in
	let r, body = parse_request cgi_vars in
	let status, headers, body = service r body in
	Printf.printf "Status: %d\x0D\x0A" status;
	List.iter pr_header headers; 
	Printf.printf "\x0D\x0A";
	begin match body with
	| `R_builder s -> Printf.printf "%s" s
	| `R_enum f -> f Unix.stdout 
	| `R_file (fn, range) -> 
	    match Conf.find c Conf.sendfile_header with
	    | None -> Private.sendfile fn range 
	    | Some h -> (* TODO range *) pr_header (h, fn)
	end; 
	`Ok 
      with Failure f -> `Error (`Webserver f)
          
  end

  module SCGI = struct 

    (* Netstring parsing *)

    let really_input fd b start len = 
      if len <= 0 then () else
      try match read fd b start len with 
      | 0 -> failwith err_ns_malformed
      | r -> really_read fd b (start + r) (len - r)
      with Unix_error (Unix.EINTR, _, _) -> really_input fd b start len

    let input_char fd b = really_input fd b 0 1 (* TODO very ineffecient. *)
      
      
    let input_ns_len fd = 
      let b = "0" in
      let c = input_char fd b in 
      if c = '0' then (if input_char ic <> ':'

    let input_netstring ic =                         
      let length =
        let c = input_char ic in
        if c = '0' then 
          (if input_char ic <> ':' then failwith err_ns_malformed else 0)
        else
          let i = ref ((Char.code c) - 0x30) in
          try 
            while true do 
              let c = input_char ic in 
              if c = ':' then raise Exit else
              if c < '0' || c > '9' then failwith err_ns_malformed;
              i := !i * 10 + ((Char.code c) - 0x30);
            done;
            assert false
          with Exit -> 
            if !i > Sys.max_string_length then failwith err_ns_size else
            !i
      in
      let s = 
        if length = 0 then "" else 
        let buf = String.create length in
        really_input ic buf 0 length;
        buf
      in
      if (input_char ic) <> ',' then failwith err_ns_malformed else
      s
        
    let get_fd c =
      let listen = match Conf.find Conf.listen with 
      | None -> Conf.default_listen | Some l -> l 
      in
      match listen with 
      | `Fd fd -> fd
      | `Addr saddr -> 
          let dom = Unix.domain_of_sockaddr saddr in 
          let fd = Unix.socket dom Unix.SOCK_STREAM 0 in 
          Unix.setsockopt fd Unix.SO_REUSEADDR true;
          Unix.bind fd saddr

    (** TODO set signals to avoid sigint on shutdown *)            

    let connect c service =
      let continue = ref true in
      let fd = get_fd c in 
      let rec accept fd = try Unix.accept fd with 
      | Unix_error (Unix.EINTR, _, _) -> accept fd
      in
      try
        Unix.listen fd (-1); (* TODO *)
        while !continue do 
          let fd, _ = accept fd in 
          ignore (fd);
        done;
	`Ok
      with
      | Failure f -> `Error (`Webserver f)
      | Unix.Unix_error (e, f, v) -> 
	  let v = if v = "" then "" else str " on %s" v in 
	  `Error (`Connector (str "%s%s: %s" f v (Unix.error_message e)))
*)
  end
end

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
