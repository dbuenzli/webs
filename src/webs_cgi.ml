(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult
open Astring
open Webs

(* Errors *)

let err_var_undef var =
  R.error_msgf "variable %S: undefined in environment" var

let err_var_dec var v kind =
  R.error_msgf "variable %S: cannot decode %s from value %S" var kind v

let err_var_header_name var =
  R.error_msgf "variable %S: cannot be represented by a header name" var

let err_malformed_env b =
  R.error_msgf "malformed environment binding: %S" b

(* Configuration keys *)

let vars = Hmap.Key.create ()

(* Environment variable decoding *)

let wrap_decoder var v k dec = match dec v with
| None -> err_var_dec var v k
| Some v -> Ok v

let decode_var var k dec =
  try wrap_decoder var (Sys.getenv var) k dec with
  | Not_found -> err_var_undef var

let decode_var_opt var k dec =
  try wrap_decoder var (Sys.getenv var) k dec with
  | Not_found -> Ok None

(* Request components *)

let get_version () =
  decode_var "SERVER_PROTOCOL" "HTTP version" HTTP.decode_version

let get_meth () =
  decode_var "REQUEST_METHOD" "HTTP method" HTTP.decode_meth

let get_path_and_query () =
  let decode_uri_request = function
  | "" -> None
  | v ->
      let p, q = match String.cut ~sep:"?" v with
      | None -> v, None
      | Some (p, q) -> p, Some q
      in
      match HTTP.decode_path p with
      | None -> None
      | Some segs -> Some (segs, q)
  in
  decode_var "REQUEST_URI" "path and query" decode_uri_request

(* Request headers *)

let normalize_name n =                          (* lowercase map '_' to '-'. *)
  for i = 0 to Bytes.length n - 1 do
    let c = Bytes.get n i in
    if c = '_' then Bytes.set n i '-' else
    Bytes.set n i (Char.Ascii.lowercase c)
  done

let http_vars_defs acc =
  let rec add acc d = acc >>= fun acc ->
    let len = String.length d in
    if len < 5
       || not (d.[0] = 'H' && d.[1] = 'T' && d.[2] = 'T' && d.[3] = 'P' &&
               d.[4] = '_')
    then Ok acc else
    match String.find (Char.equal '=') d with
    | None -> err_malformed_env d
    | Some eq_pos ->
        let name_len = eq_pos - 5 in
        let name = Bytes.create name_len in
        Bytes.blit_string d 5 name 0 name_len;
        normalize_name name;
        match HTTP.H.decode_name (Bytes.unsafe_to_string name) with
        | None ->
            err_var_header_name (String.with_range d ~len:eq_pos)
        | Some name ->
            if name = HTTP.H.content_type || name = HTTP.H.content_length
            then (* skip *) Ok acc else
            let v_first = eq_pos + 1 in
            let v_len = String.length d - v_first in
            let v = String.with_range d ~first:v_first ~len:v_len in
            Ok (HTTP.H.def name v acc)
  in
  Array.fold_left add (Ok acc) (Unix.environment ())

let cgi_var_header_name var =
  let prefix = "x-cgi-" in
  let pre_len = String.length prefix in
  let var_len = String.length var in
  let name = Bytes.create (pre_len + var_len) in
  Bytes.blit_string prefix 0 name 0 pre_len;
  Bytes.blit_string var 0 name pre_len var_len;
  normalize_name name;
  HTTP.H.decode_name (Bytes.unsafe_to_string name)

let cgi_vars_defs cgi_vars acc =
  let rec loop acc = function
  | [] -> Ok acc
  | var :: vars ->
      begin match cgi_var_header_name var with
      | None -> err_var_header_name var
      | Some n ->
          let acc' = try HTTP.H.def n (Sys.getenv var) acc with
          | Not_found -> acc
          in
          loop acc' vars
      end
  in
  loop acc cgi_vars

let get_headers cgi_vars =
  let var_def var n hs =
    try (match Sys.getenv var with "" -> hs | v -> HTTP.H.def n v hs)
    with Not_found -> hs
  in
  HTTP.H.(empty
          |> var_def "CONTENT_LENGTH" content_length
          |> var_def "CONTENT_TYPE" content_type
          |> cgi_vars_defs cgi_vars
          >>= http_vars_defs)

(* Request body *)

let get_body_len () =
  let decode_content_len = function
  | "" -> Some None
  | v ->
      match HTTP.decode_digits v with
      | None -> None
      | Some v -> Some (Some v)
  in
  decode_var_opt "CONTENT_LENGTH" "content length" decode_content_len

let get_body ic body_len = match body_len with
| None -> fun () -> None
| Some len ->
    let todo = ref len in
    let buf = ref (Bytes.create 65535) in
    fun () ->
      if !todo = 0 then (buf := Bytes.empty; None) else
      let len = min !todo (Bytes.length !buf) in
      let rc = input ic !buf 0 len in
      todo := !todo - rc;
      Some (!buf, 0, rc)

(* Request *)

let req_of_cgi_env cgi_vars =
  get_version ()
  >>= fun version -> get_meth ()
  >>= fun meth -> get_path_and_query ()
  >>= fun (path, query) -> get_headers cgi_vars
  >>= fun headers -> get_body_len ()
  >>= fun body_len ->
  let body = get_body Pervasives.stdin body_len in
  Ok (Req.v version meth ~path ?query headers ?body_len body)

(* Response *)

let stream_of_body = function
| Resp.Stream s -> Ok s
| Resp.File (range, file) -> failwith "TODO"


let write_status_line oc resp =
  output_string oc (HTTP.encode_version (Resp.version resp));
  output_char oc ' ';
  output_string oc (HTTP.encode_digits (Resp.status resp));
  output_char oc ' ';
  output_string oc (HTTP.status_reason_phrase (Resp.status resp));
  output_string oc "\r\n";
  ()

let write_headers oc resp = ()
(*
  let write_header n v =
    output_string oc (HTTP.H.encode_name n);
    output_char oc ':';
    output_string oc v;
    output_string oc "\r\n";
  in
(*
  let write_header_multi n vs =
    if HTTP.H.(name_equal n set_cookie)
    then List.iter (fun v -> write_header n v) vs
    else write_header n (HTTP.H.encode_multi_value vs)
  in
*)
*)

let write_resp oc resp =
  try
    let write_body = function
    | None -> close_out oc
    | Some (buf, pos, len) -> output oc buf pos len
    in
    stream_of_body (Resp.body resp) >>= fun stream ->
    write_status_line oc resp;
    write_headers oc resp;
    stream write_body;
    Ok ()
  with exn -> failwith "TODO"


(* Connector *)

let connect conf service =
  let cgi_vars = match Hmap.find vars conf with
  | None -> [] | Some vars -> vars
  in
  match req_of_cgi_env cgi_vars with
  | Error (`Msg _ as e) -> Error (`Webserver e)
  | Ok req ->
      match R.trap_exn service req with
      | Error trap -> Error (`Service trap)
      | Ok resp -> write_resp stdout resp


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
