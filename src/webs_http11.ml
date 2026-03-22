(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Result.Syntax
open Webs
open Bytesrw

let decode_header_list buf ~crlfs =
  let rec loop acc buf last_crlf = function
  | [] -> acc
  | crlf :: crlfs ->
      let first = last_crlf + 2 in
      let name, value = Webs__base.decode_header_field buf ~first ~crlf in
      let name = Http.Headers.Name.unsafe_of_string name in
      let acc =
        (* This looks ok according to RFC 7230 3.2.2 *)
        if Http.Headers.Name.equal name Http.Headers.set_cookie
        then Http.Headers.append_set_cookie value acc
        else Http.Headers.append_value name value acc
        in
        loop acc buf crlf crlfs
  in
  loop Http.Headers.empty buf (List.hd crlfs) (List.tl crlfs)

(* Read an HTTP head from a bytes reader *)

let err_miss_crlf = "Missing CRLF in HTTP head"
let err_eod_in_head = "Unexpected end of stream in HTTP head"
let err_max_head d =
  Printf.sprintf "maximal HTTP head size (%d bytes) exceeded" d

let default_head_buffer () =
  Bytes.create Http.Connector.Default.max_http_head_byte_size

type head_state = Eps | Cr | Crlf | Crlf_cr | Crlf_crlf

let read_head_bytes r head = (* write in [head] returns the last written byte *)
  let rec loop r head next state = match Bytes.Reader.read r with
  | slice when Bytes.Slice.is_eod slice -> failwith err_eod_in_head
  | slice ->
      let first = Bytes.Slice.first slice and last = Bytes.Slice.last slice in
      let bytes = Bytes.Slice.bytes slice in
      let state = ref state in
      let i = ref first in
      while !i <= last && !state != Crlf_crlf do
        state := begin match Bytes.get bytes !i with
        | '\r' ->
            (match !state with Crlf -> Crlf_cr | _ -> Cr)
        | '\n' ->
            (match !state with Cr -> Crlf | Crlf_cr -> Crlf_crlf | _ -> Eps)
        | _ -> Eps
        end;
        incr i;
      done;
      let last = if !state = Crlf_crlf then !i - 1 else last in
      let len = last - first + 1 in
      let next' = next + len in
      if next' >= Bytes.length head
      then failwith (err_max_head (Bytes.length head)) else
      begin
        Bytes.blit bytes first head next len;
        if !state <> Crlf_crlf then loop r head (next + len) !state else
        let () = match Bytes.Slice.drop len slice with
        | None -> ()
        | Some rem -> Bytes.Reader.push_back r rem
        in
        next + len - 1 - 2 (* eschew last crlf *)
      end
  in
  loop r head 0 Eps

let get_next_crlf bytes ~first ~last =
  let cr_max = last - 1 in
  let i = ref first and found = ref false in
  while !i <= cr_max && not !found do
    found := (Bytes.get bytes !i = '\r' && Bytes.get bytes (!i + 1) = '\n');
    incr i;
  done;
  if not !found then raise_notrace (Failure err_miss_crlf) else (!i - 1)

let decode_headers r ~head ~first ~max =
  let rec loop headers head ~first ~max =
    if first > max then headers else
    let crlf = get_next_crlf head ~first ~last:max in
    let name, value = Webs__base.decode_header_field head ~first ~crlf in
    let name = Http.Headers.Name.unsafe_of_string name in
    let first = crlf + 2 in
    let headers =
      (* This looks ok according to RFC 7230 3.2.2 *)
      if Http.Headers.Name.equal name Http.Headers.set_cookie
      then Http.Headers.append_set_cookie value headers
      else Http.Headers.append_value name value headers
    in
    loop headers head ~first ~max
  in
  let headers = loop Http.Headers.empty head ~first ~max in
  let content_length =
    (* FIXME we are using that both for reponse and request
       FIXME we need to sort out the conditions in which the body
       is empty and make sure that Http.Body.of_bytes_reader
       uses an empty body in that case. E.g. it would be nice
       on a websocket upgrade that the body is empty. *)
    match Http.Headers.request_body_length headers with
    | Error e -> failwith e
    | Ok (`Length l) -> Some l
    | Ok `Chunked ->
        (* TODO I guess we can do that with a read filter on [r] *)
        failwith "TODO not implemented: chunked bodies"
  in
  let content_type =
    Http.Headers.(find ~lowervalue:true content_type) headers
  in
  headers, Http.Body.of_bytes_reader ?content_length ?content_type r

let encode_header n v acc =
  let encode n acc v =
    Http.Headers.Name.encode n :: ": " :: v :: Webs__base.crlf :: acc
  in
  if not (Http.Headers.Name.equal n Http.Headers.set_cookie)
  then encode n acc v else
  let vs = Http.Headers.values_of_set_cookie_value v in
  List.fold_left (encode Http.Headers.set_cookie) acc vs

module Version = struct
  let err_version = "not an HTTP version"

  let decode_of_bytes b ~first ~max =
    let len = max - first + 1 in
    if len < 6 then failwith err_version else
    let[@inline] c b i = Bytes.get b (first + i) in
    if c b 0 = 'H' && c b 1 = 'T' && c b 2 = 'T' && c b 3 = 'P' &&
       c b 4 = '/' && Webs__base.is_digit (c b 5)
    then begin
      if len = 6 then (first + 6, (Webs__base.digit_to_int (c b 5), 0)) else
      let sep = c b 6 in
      if sep = ' ' then (first + 6, (Webs__base.digit_to_int (c b 5), 0)) else
      if len >= 8 && sep = '.' && Webs__base.is_digit (c b 7)
      then first + 8, (Webs__base.digit_to_int (c b 5),
                       Webs__base.digit_to_int (c b 7))
      else failwith err_version
    end
    else failwith err_version

  let decode s =
    let len = String.length s in
    if not (len = 8 || len = 6) then Error err_version else
    let max = len - 1 in
    match decode_of_bytes (Bytes.unsafe_of_string s) ~first:0 ~max with
    | exception Failure e -> Error e | (_, v) -> Ok v

  let encode (maj, min) =
    let b = Bytes.create 8 and s = Bytes.set in
    Bytes.blit_string "HTTP/" 0 b 0 5;
    s b 5 (Webs__base.digit_of_int maj); s b 6 '.';
    s b 7 (Webs__base.digit_of_int min);
    Bytes.unsafe_to_string b
end

module Status = struct
  let err_status = "not an HTTP status code"
  let decode b ~first ~max = (* TODO rewrite *)
    if max - first + 1 < 3 then failwith err_status else
    let[@inline] c b i = Bytes.get b (first + i) in
    if Webs__base.is_digit (c b 0) &&
       Webs__base.is_digit (c b 1) &&
       Webs__base.is_digit (c b 2)
    then first + 3,
         (Webs__base.digit_to_int (c b 0) * 100 +
          Webs__base.digit_to_int (c b 1) * 10 +
          Webs__base.digit_to_int (c b 2))
    else failwith err_status
end

module Response = struct

  (* Decode *)

  let err_status_line_garbage = "remaining garbage on the HTTP status line"

  let decode_status_line b ~first ~last =
    (* HTTP https://www.rfc-editor.org/rfc/rfc9112#name-status-line *)
    let first, version = Version.decode_of_bytes b ~first ~max:last in
    let first = Webs__base.decode_sp b ~first ~max:last in
    let first, status = Status.decode b ~first ~max:last in
    let first = Webs__base.decode_sp b ~first ~max:last in
    let reason = Bytes.sub_string b first (last - first + 1) in
    version, status, reason

  let read ?head_buffer:(head = default_head_buffer ()) ?log r =
    try
      let first = 0 and last = read_head_bytes r head in
      let l = get_next_crlf head ~first ~last - 1 in
      let version, status, reason = decode_status_line head ~first ~last:l in
      let headers, body = decode_headers r ~head ~first:(l + 3) ~max:last in
      Ok (Http.Response.make ~headers ?log ~reason ~version status body)
    with
    | Failure e -> Error e

  (* Encode *)

  let encode_head response =
    let status = string_of_int (Http.Response.status response) in
    let reason = Http.Response.reason response in
    let hs = Http.Response.headers response in
    let hs = Http.Headers.for_connector hs (Http.Response.body response) in
    let hs = Http.Headers.fold encode_header hs [Webs__base.crlf] in
    String.concat "" @@
    "HTTP/1.1 " :: status :: " " :: reason :: Webs__base.crlf :: hs

  let encode response =
    encode_head response ^ Http.Body.to_string (Http.Response.body response)

  let write_head w response =
    Bytes.Writer.write_string w (encode_head response)

  let write ~eod w response =
    write_head w response; Http.Body.write ~eod w (Http.Response.body response)
end

module Request = struct

  (* Decode *)

  let err_request_line_garbage = "remaining garbage on the HTTP request line"

  let decode_method b ~first ~max = (* FIXME redo *)
    let first, token = Webs__base.decode_token b ~first ~max in
    first, Http.Method.decode token |> Result.get_ok

  let decode_line b ~first ~last:max =
    (* https://www.rfc-editor.org/rfc/rfc9112#name-request-line *)
    let decode_request_target b ~first ~max =
      let rec loop b i =
        if i <= max && Bytes.get b i <> ' ' then loop b (i + 1) else i - 1
      in
      match loop b first with
      | last when last < first -> failwith Webs__base.err_empty_string
    | last -> last + 1, Bytes.sub_string b first (last - first + 1)
    in
    let first, meth = decode_method b ~first ~max in
    let first = Webs__base.decode_sp b ~first ~max in
    let first, target = decode_request_target b ~first ~max in
    let first = Webs__base.decode_sp b ~first ~max in
    let first, version = Version.decode_of_bytes b ~first ~max in
    if first = max + 1
    then meth, target, version
    else failwith err_request_line_garbage

  (* Encode *)

  let encode_head request =
    let method' = Http.Method.encode (Http.Request.method' request) in
    let target = Http.Request.raw_path request in
    let hs = Http.Request.headers request in
    let hs = Http.Headers.for_connector hs (Http.Request.body request) in
    let hs = Http.Headers.fold encode_header hs [Webs__base.crlf] in
    String.concat "" @@
    method' :: " " :: target :: " HTTP/1.1" :: Webs__base.crlf :: hs

  let encode request =
    encode_head request ^ Http.Body.to_string (Http.Request.body request)

  let read ?head_buffer:(head = default_head_buffer ()) ?log  ~service_path r =
    try
      let first = 0 and last = read_head_bytes r head in
      let l = get_next_crlf head ~first ~last - 1 in
      let method', raw_path, version = decode_line head ~first ~last:l in
      let headers, body = decode_headers r ~head ~first:(l + 3) ~max:last in
      Http.Request.for_service_connector
        ~headers ?log ~service_path ~version method' ~raw_path body
    with
    | Failure e ->
        (* FIXME abide by connector responses conventions *)
        Http.Response.bad_request_400 ~log:e ()

  let write_head w request =
    Bytes.Writer.write_string w (encode_head request)

  let write ~eod w request =
    write_head w request; Http.Body.write ~eod w (Http.Request.body request)
end
