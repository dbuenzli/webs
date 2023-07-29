(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs

let strf = Printf.sprintf
let ( let* ) = Result.bind

module Smap = Map.Make (String)
let chop_prefix ~prefix s =
  if not (Http.string_starts_with ~prefix s) then s else
  Http.string_subrange ~first:(String.length prefix) s

let io_buffer_size = 65536 (* IO_BUFFER_SIZE 4.0.0 *)

(* Convert CGI environment variables to header names *)

let var_to_header_name ?(start = 0) ?(prefix = "") s =
  let b = Bytes.create (String.length s - start) in
  for i = 0 to String.length s - 1 - start do
    let c = String.get s (i + start) in
    if c = '_' then Bytes.set b i '-' else
    Bytes.set b i (Char.lowercase_ascii c)
  done;
  let name = Bytes.unsafe_to_string b in
  Http.Name.decode (if prefix <> "" then prefix ^ name else name)

let http_var_to_header_name var = var_to_header_name ~start:5 var
let extra_var_to_header_name var =
  match var_to_header_name ~prefix:"x-cgi-" var with
  | Error e -> invalid_arg e | Ok n -> n

(* Connectors *)

type t =
  { extra_vars : (string * Http.Name.t) list;
    log : Webs_connector.log_msg -> unit;
    max_req_body_byte_size : int;
    service_path : Http.Path.t; }

let create
    ?(extra_vars = []) ?(log = Webs_connector.default_log ~trace:false ())
    ?(max_req_body_byte_size = 10 * 1024 * 1024) ?(service_path = [""]) ()
  =
  let with_header_name v = v, extra_var_to_header_name v in
  let extra_vars = List.map with_header_name extra_vars in
  { extra_vars; log; max_req_body_byte_size; service_path; }

let extra_vars c = List.map fst c.extra_vars
let log c = c.log
let max_req_body_byte_size c = c.max_req_body_byte_size
let service_path c = c.service_path

(* Request *)

let err_malformed_env = "malformed environment"
let err_var_miss var = strf "variable %S undefined in environment" var
let err_var_decode var e = strf "error decoding %S: %s" var e

let[@inline] is_http_var s =
  String.length s > 5 &&
  s.[0] = 'H' && s.[1] = 'T' && s.[2] = 'T' && s.[3] = 'P' && s.[4] = '_'

let content_vars =
  [ "CONTENT_TYPE", Http.content_type;
    "CONTENT_LENGTH", Http.content_length ]

let headers_of_env ~extra_vars env =
  let add_var ~add_empty env hs (var, name) = match Smap.find_opt var env with
  | Some v when add_empty || v <> "" ->
      Http.Headers.def name (Http.Private.trim_ows v) hs
  | _ -> hs
  in
  let rec loop i max env hs others =
    if i > max then hs, others else
    let b = env.(i) in
    match String.index_opt b '=' with
    | None -> failwith err_malformed_env
    | Some eq ->
        let var = Http.string_subrange ~last:(eq - 1) b in
        let value = Http.string_subrange ~first:(eq + 1) b in
        match is_http_var var with
        | false -> loop (i + 1) max env hs (Smap.add var value others)
        | true ->
            match http_var_to_header_name var with
            | Error e -> failwith e
            | Ok v ->
                let hs = Http.Headers.def v (Http.Private.trim_ows value) hs in
                loop (i + 1) max env hs others
  in
  let max = Array.length env - 1 in
  let hs, env = loop 0 max env Http.Headers.empty Smap.empty in
  let hs = List.fold_left (add_var ~add_empty:true env) hs extra_vars in
  let hs = List.fold_left (add_var ~add_empty:false env) hs content_vars in
  hs, env

let find_var var decode env = match Smap.find_opt var env with
| None -> None
| Some value ->
    match decode value with
    | Ok value -> Some value | Error e -> failwith (err_var_decode e var)

let get_var var decode env = match find_var var decode env with
| None -> failwith (err_var_miss var) | Some v -> v

let header_section_of_env ~extra_vars env =
  let hs, env = headers_of_env ~extra_vars env in
  let version = get_var "SERVER_PROTOCOL" Http.Version.decode env in
  let meth = get_var "REQUEST_METHOD" Http.Meth.decode env in
  let request_target = get_var "REQUEST_URI" Result.ok env in
  version, meth, request_target, hs

let body_length hs = match Http.Headers.request_body_length hs with
| Error e -> Error (`Malformed e)
| Ok (`Length l) -> Ok (Some l)
| Ok `Chunked -> Error (`Not_implemented "chunked bodies") (* TODO *)

let read_req c env fd_in =
  try
    let version, meth, request_target, headers =
      header_section_of_env ~extra_vars:c.extra_vars env
    in
    let path, query =
      match Http.Path.and_query_string_of_request_target request_target with
      | Ok v -> v | Error e -> failwith e
    in
    let service_path, path =
      if path = [] (* "*" request line FIXME not sure it's a good idea,
                      maybe we coud fail *)
      then [], [] else
      match Http.Path.strip_prefix ~prefix:c.service_path path with
      | None -> failwith "Cannot strip service path from requested URI"
      | Some path -> c.service_path, path
    in
    let buf = Bytes.create io_buffer_size in
    let* body_length = body_length headers in
    let body =
      let first_start = 0 and first_len = 0 in
      let max_req_body_byte_size = c.max_req_body_byte_size in
      Webs_unix.Connector.req_body_reader
        ~max_req_body_byte_size ~body_length fd_in buf ~first_start ~first_len
    in
    Ok (Http.Req.v ~body ~body_length ~headers ~meth ~path ~query
          ~request_target ~service_path ~version ())
  with
  | Failure e -> Error (`Malformed e)
  (* FIXME maybe for error from header_section we should rather throw
     unexpected connector *)

(* Responses *)

let encode_resp_header_section st reason hs =
  let crlf = "\r\n" in
  let enc_header n v acc =
    let encode n acc v = Http.Name.encode n :: ": " :: v :: crlf :: acc in
    if not (Http.Name.equal n Http.set_cookie) then encode n acc v else
    let cookies = Http.Headers.values_of_set_cookie_value v in
    List.fold_left (encode Http.set_cookie) acc cookies
  in
  String.concat "" @@
  "Status:" :: string_of_int st :: " " :: reason :: crlf ::
  Http.Headers.fold enc_header hs [crlf]

let write_resp c fd resp =
  let resp, write_body = Webs_unix.Connector.resp_body_writer resp in
  (* TODO check what to do with the connection in case of upgrade *)
  let hs =
    Http.Headers.(Http.Resp.headers resp |>
                  def_if_undef Http.connection "close")
  in
  let st = Http.Resp.status resp and reason = Http.Resp.reason resp in
  let sec = encode_resp_header_section st reason hs in
  let sec = Bytes.unsafe_of_string sec in
  Webs_unix.Connector.write fd sec ~start:0 ~len:(Bytes.length sec);
  write_body fd

(* Serving *)

let resp_of_error e =
  let reason e = if e = "" then None else Some e in
  match e with
  | `Service -> Http.Resp.v Http.Status.server_error_500
  | `Too_large -> Http.Resp.v Http.Status.payload_too_large_413 (* FIXME *)
  | `Malformed e -> Http.Resp.v Http.Status.bad_request_400 ?reason:(reason e)
  | `Not_implemented e ->
      Http.Resp.v Http.Status.not_implemented_501 ?reason:(reason e)

let apply_service c service req =
  try
    let resp = service req in
    Ok resp
  with
  | e ->
      let bt = Printexc.get_raw_backtrace () in
      match e with
      | Out_of_memory as e -> Printexc.raise_with_backtrace e bt
      | Sys.Break as e -> Printexc.raise_with_backtrace e bt
      | Stack_overflow as e | e -> c.log (`Service_exn (e, bt)); Error `Service

let serve_req c env fd_in fd_out service =
  try
    let dur = Webs_unix.Time.counter () in
    let req = read_req c env fd_in in
    let resp = Result.bind req (apply_service c service) in
    let resp = Result.fold ~ok:Fun.id ~error:resp_of_error resp in
    let () = write_resp c fd_out resp in
    let dur = Webs_unix.(Time.Span.to_uint64_ns @@ Time.count dur) in
    c.log (`Trace (dur, Result.to_option req, Some resp));
    Ok ()
  with
  | e ->
      (* apply_service catches some of exns and turns them into 500.
         If we are here we started writing the response and are beyond
         being able to respond with a 500. *)
      let bt = Printexc.get_raw_backtrace () in
      match e with
      | Out_of_memory as e -> Printexc.raise_with_backtrace e bt
      | Sys.Break as e -> Printexc.raise_with_backtrace e bt
      | Stack_overflow as _e | _e ->
          c.log (`Connector_exn (e, bt)); Error "Connector error"

let serve c service =
  serve_req c (Unix.environment ()) Unix.stdin Unix.stdout service

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
