(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs

let ( let* ) = Result.bind
let strf = Printf.sprintf

module String_map = Map.Make (String)

let chop_prefix ~prefix s =
  if not (String.starts_with ~prefix s) then s else
  Http.Private.string_subrange ~first:(String.length prefix) s

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
  Http.Headers.Name.decode (if prefix <> "" then prefix ^ name else name)

let http_var_to_header_name var = var_to_header_name ~start:5 var
let extra_var_to_header_name var =
  match var_to_header_name ~prefix:"x-cgi-" var with
  | Error e -> invalid_arg e | Ok n -> n

(* Connectors *)

type t =
  { extra_vars : (string * Http.Headers.Name.t) list;
    log : Http.Connector.Log.msg -> unit;
    max_request_body_byte_size : int;
    service_path : Http.Path.t; }

let make
    ?(extra_vars = []) ?(log = Http.Connector.Log.default ~trace:false ())
    ?(max_request_body_byte_size =
      Http.Connector.Default.max_request_body_byte_size)
    ?(service_path = Http.Path.root) ()
  =
  let with_header_name v = v, extra_var_to_header_name v in
  let extra_vars = List.map with_header_name extra_vars in
  { extra_vars; log; max_request_body_byte_size; service_path; }

let extra_vars c = List.map fst c.extra_vars
let log c = c.log
let max_request_body_byte_size c = c.max_request_body_byte_size
let service_path c = c.service_path

(* Responses *)

let encode_cgi_response_head st reason headers =
  let crlf = "\r\n" in
  let enc_header n v acc =
    let encode n acc v =
      Http.Headers.Name.encode n :: ": " :: v :: crlf :: acc
    in
    if not (Http.Headers.Name.equal n Http.Headers.set_cookie)
    then encode n acc v else
    let cookies = Http.Headers.values_of_set_cookie_value v in
    List.fold_left (encode Http.Headers.set_cookie) acc cookies
  in
  String.concat "" @@
  "Status:" :: string_of_int st :: " " :: reason :: crlf ::
  Http.Headers.fold enc_header headers [crlf]

let write_cgi_response fd response =
  let write_body = Webs_unix.Fd.body_writer (Http.Response.body response) in
  let status = Http.Response.status response in
  let reason = Http.Response.reason response in
  let hs = Http.Response.headers response in
  let hs = Http.Headers.for_connector hs (Http.Response.body response) in
  let hs = Http.Headers.(def_if_undef connection "close") hs in
  let head = encode_cgi_response_head status reason hs in
  let head = Bytes.unsafe_of_string head and length = String.length head in
  Webs_unix.Fd.write fd head ~start:0 ~length;
  write_body fd

let send_100_continue fd =
  let r = Http.Response.make Http.Status.continue_100 Http.Body.empty in
  write_cgi_response fd r (* FIXME error handling *)

(* Request *)

let err_malformed_env = "malformed environment"
let err_var_miss var = strf "variable %S undefined in environment" var
let err_var_decode var e = strf "error decoding %S: %s" var e

let[@inline] is_http_var s =
  String.length s > 5 &&
  s.[0] = 'H' && s.[1] = 'T' && s.[2] = 'T' && s.[3] = 'P' && s.[4] = '_'

let content_vars =
  [ "CONTENT_TYPE", Http.Headers.content_type;
    "CONTENT_LENGTH", Http.Headers.content_length ]

let headers_of_env ~extra_vars env =
  let add_var ~add_empty env hs (var, name) =
    match String_map.find_opt var env with
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
        let var = Http.Private.string_subrange ~last:(eq - 1) b in
        let value = Http.Private.string_subrange ~first:(eq + 1) b in
        match is_http_var var with
        | false -> loop (i + 1) max env hs (String_map.add var value others)
        | true ->
            match http_var_to_header_name var with
            | Error e -> failwith e
            | Ok v ->
                let hs = Http.Headers.def v (Http.Private.trim_ows value) hs in
                loop (i + 1) max env hs others
  in
  let max = Array.length env - 1 in
  let hs, env = loop 0 max env Http.Headers.empty String_map.empty in
  let hs = List.fold_left (add_var ~add_empty:true env) hs extra_vars in
  let hs = List.fold_left (add_var ~add_empty:false env) hs content_vars in
  hs, env

let find_var var decode env = match String_map.find_opt var env with
| None -> None
| Some value ->
    match decode value with
    | Ok value -> Some value | Error e -> failwith (err_var_decode e var)

let get_var var decode env = match find_var var decode env with
| None -> failwith (err_var_miss var) | Some v -> v

let head_of_env ~extra_vars env =
  let hs, env = headers_of_env ~extra_vars env in
  let version = get_var "SERVER_PROTOCOL" Http.Version.decode env in
  let meth = get_var "REQUEST_METHOD" Http.Method.decode env in
  let raw_path = get_var "REQUEST_URI" Result.ok env in
  version, meth, raw_path, hs

let handle_expect_header headers =
  (* See connector_conventions.mld for more information. *)
  match Http.Headers.find ~lowervalue:true Http.Headers.expect headers with
  | Some "100-continue" -> Error (`Expect_100_continue)
  | None | Some _ -> Ok ()

let read_request c env fd_in fd_out =
  let content_length hs = match Http.Headers.request_body_length hs with
  | Error e -> Error (`Malformed e)
  | Ok (`Length l) -> Ok (Some l)
  | Ok `Chunked -> Error (`Not_implemented "chunked bodies") (* TODO *)
  in
  try
    let version, method', raw_path, headers =
      head_of_env ~extra_vars:c.extra_vars env
    in
    let path, query =
      match Http.Path.and_query_string_of_request_target raw_path with
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
    let* content_length = content_length headers in
    let content_type =
      Http.Headers.(find ~lowervalue:true content_length) headers
    in
    let* content = handle_expect_header headers in
    let content =
      let first_start = 0 and first_len = 0 in
      let max_request_body_byte_size = c.max_request_body_byte_size in
      Webs_unix.Fd.body_byte_reader
        ~max_request_body_byte_size ~content_length fd_in buf ~first_start
        ~first_len
    in
    let body = Http.Body.of_byte_reader ?content_length ?content_type content in
    Ok (Http.Request.make ~headers ~path ~query ~service_path ~version
          method' ~raw_path body)
  with
  | Failure e -> Error (`Malformed e)
  (* FIXME maybe for error from header_section we should rather throw
     unexpected connector *)

(* Serving *)

let response_of_error e =
  let reason e = if e = "" then None else Some e in
  match e with
  | `Service ->
      Http.Response.empty Http.Status.server_error_500
  | `Too_large ->
      Http.Response.empty Http.Status.content_too_large_413 (* FIXME *)
  | `Malformed e ->
      Http.Response.empty Http.Status.bad_request_400 ?reason:(reason e)
  | `Not_implemented e ->
      Http.Response.empty Http.Status.not_implemented_501 ?reason:(reason e)
  | `Expect_100_continue ->
      Http.Response.empty Http.Status.expectation_failed_417

let apply_service c service req = try Ok (service req) with
| e ->
    let bt = Printexc.get_raw_backtrace () in
    match e with
    | Out_of_memory as e -> Printexc.raise_with_backtrace e bt
    | Sys.Break as e -> Printexc.raise_with_backtrace e bt
    | Stack_overflow as e | e -> c.log (`Service_exn (e, bt)); Error `Service

let serve_request c env fd_in fd_out service =
  try
    let dur = Webs_unix.Time.counter () in
    let request = read_request c env fd_in fd_out in
    let response = Result.bind request (apply_service c service) in
    let response = Result.fold ~ok:Fun.id ~error:response_of_error response in
    let () = write_cgi_response fd_out response in
    let dur = Webs_unix.(Time.Span.to_uint64_ns @@ Time.count dur) in
    c.log (`Trace (dur, Result.to_option request, Some response));
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
  serve_request c (Unix.environment ()) Unix.stdin Unix.stdout service
