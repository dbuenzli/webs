(*----------------------------------------------------------------------------
   Copyright (c) 2007 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   Cgir version %%VERSION%%
  ----------------------------------------------------------------------------*)

let str = Printf.sprintf 
let err_clen s = str "header CONTENT_LENGTH: not an integer (%S)" s 
let err_scgi s = str "SCGI request: %s" s
let err_miss_clen = "missing header: CONTENT_LENGTH"
let err_miss_scgi = "missing header: SCGI with value 1"
let err_miss_nul = "malformed headers: missing a NUL separator"
let err_no_name = "malformed headers: empty header name"
let err_eos = "unexpected end of stream"
let err_ns_size = "netstring too large" 
let err_ns_malformed = "malformed netstring"

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
    
let cgi_headers = [ 
  "SERVER_SOFTWARE"; "SERVER_NAME"; "GATEWAY_INTERFACE"; "SERVER_PROTOCOL";
  "SERVER_PORT"; "REQUEST_METHOD"; "PATH_INFO"; "PATH_TRANSLATED"; 
  "SCRIPT_NAME"; "QUERY_STRING"; "REMOTE_HOST"; "REMOTE_ADDR"; "AUTH_TYPE";
  "REMOTE_USER"; "REMOTE_IDENT"; "CONTENT_TYPE"; "CONTENT_LENGTH"; ]
    
let http_headers = [ 
  "HTTP_ACCEPT"; "HTTP_ACCEPT_CHARSET"; "HTTP_ACCEPT_ENCODING"; 
  "HTTP_ACCEPT_LANGUAGE"; "HTTP_AUTHORIZATION"; "HTTP_EXPECT"; 
  "HTTP_FROM"; "HTTP_HOST"; "HTTP_IF_MATCH"; "HTTP_IF_MODIFIED_SINCE"; 
  "HTTP_IF_NONE_MATCH"; "HTTP_IF_RANGE"; "HTTP_IF_UNMODIFIED_SINCE"; 
  "HTTP_MAX_FORWARDS"; "HTTP_PROXY_AUTHORIZATION"; "HTTP_RANGE"; 
  "HTTP_REFERER"; "HTTP_TE"; "HTTP_USER_AGENT"; ]
    
type headers = 
  | Fun of (string -> string option) 
  | Assoc of (string * string) list

type body = string option * int * in_channel
type t = { headers : headers; body : body option; }

let header r h = match r.headers with
| Fun f -> f h 
| Assoc l -> try Some (List.assoc h l) with Not_found -> None 

let headers r = match r.headers with
| Assoc l -> l
| Fun f -> 
    let add acc h = match f h with None -> acc | Some v -> (h,v) :: acc in
    let cgi = List.fold_left add [] cgi_headers in 
    List.rev (List.fold_left add cgi http_headers)

let body r = r.body 
  
let input_cgi () =
  let f s = try Some (Sys.getenv s) with Not_found -> None in
  try 
    let body = match f "CONTENT_LENGTH" with
    | Some l -> 
	let l = try int_of_string l with Failure _ -> failwith (err_clen l) in
	if l > 0 then Some (f "CONTENT_TYPE", l, stdin) else None
    | None -> None
    in
    `Ok { headers = Fun f; body = body }
  with Failure e -> `Error e

let input_scgi ic =
  let parse_req ns =
    let ns_len = String.length ns in 
    let rec aux ns last_nul acc scgi ctype =
      if last_nul < 0 then 
	begin 
	  if not scgi then failwith err_miss_scgi else
	  match acc with 
	  | (("CONTENT_LENGTH", l) :: hl) as headers ->
	      let l = 
		try int_of_string l with Failure _ -> failwith (err_clen l)
	      in 
	      let body = if l > 0 then Some (ctype, l, ic) else None in
	      `Ok { headers = Assoc headers; body = body }
	  | _ -> failwith err_miss_clen
	end
      else
      let v_end = last_nul - 1 in
      let v_nul = String.rindex_from ns v_end '\x00' in
      let h_end = v_nul - 1 in
      let h_nul = try String.rindex_from ns h_end '\x00' with Not_found -> -1 in
      let h = String.sub ns (h_nul + 1) (h_end - h_nul) in
      let v = String.sub ns (v_nul + 1) (v_end - v_nul) in 
      match h, v with
      | "", _ -> failwith err_no_name
      | ("SCGI", "1") as h_v -> aux ns h_nul (h_v :: acc) true ctype
      | ("CONTENT_TYPE", ct) as h_v -> aux ns h_nul (h_v :: acc) scgi (Some ct)
      | h_v -> aux ns h_nul (h_v :: acc) scgi ctype
    in
    try
      if ns_len = 0 then failwith err_miss_clen else
      if ns.[ns_len - 1] <> '\x00' then raise Not_found else
      aux ns (ns_len - 1) [] false None 
    with
    | Not_found -> failwith err_miss_nul
  in
  try parse_req (input_netstring ic) with 
  | End_of_file -> `Error (err_scgi err_eos)
  | Failure e -> `Error (err_scgi e)	

let print ?(body = true) ?headers fmt r = 
  let pr = Format.fprintf in
  let hv r h = h, header r h in 
  let hv_list = match headers with 
  | Some hl -> List.map (hv r) hl 
  | None -> match r.headers with
    | Fun f -> List.map (hv r) (cgi_headers @ http_headers)
    | Assoc l -> List.map (fun (h, v) -> h, Some v) l 
  in
  let pr_hv fmt (h,v) = match v with 
  | None -> pr fmt "@[<h>%s@ absent@]@\n" h
  | Some v -> pr fmt "@[<h>%s@ =@ %S@]@\n" h v
  in
  List.iter (pr_hv fmt) hv_list;
  if body then begin match r.body with
  | None -> pr fmt "@\nRequest body empty.@\n"
  | Some (t, len, ic) -> 
      pr fmt "@\nRequest body:@\n";
      for i = 1 to len do pr fmt "%c" (input_char ic); done
  end

let to_string ?body ?headers r = 
  let b = Buffer.create 1024 in 
  Format.bprintf b "%a@?" (print ?body ?headers) r;
  Buffer.contents b

let output_scgi oc hl =
  let len_h acc (h,v) = acc + 2 + String.length h + String.length v in
  let out_h oc (h,v) = 
    output_string oc h; output_char oc '\x00';
    output_string oc v; output_char oc '\x00';
  in
  let s_len = List.fold_left len_h 0 hl in
  output_string oc (string_of_int s_len);
  output_char oc ':';
  List.iter (out_h oc) hl;
  output_char oc ','
  
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
