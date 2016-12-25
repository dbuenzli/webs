(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult
open Astring

(* Errors *)

let err_neg_digits d = strf  "negative number (%d)" d
let err_path_no_segs = strf "path without any segments"
let err_header_undef n = strf "header %s undefined in map" n
let err_empty_multi_value = "multi value cannot be the empty list"
let err_not_token t = strf "%S is not an HTTP token" t
let err_version_digit num d =
  strf "%s version number %d not in [0;9] interval" num d

(* HTTP tokens, see https://tools.ietf.org/html/rfc7230#section-3.2.6 *)

let is_upper = Char.Ascii.is_upper
let is_lower_tchar = function
| 'a' .. 'z'
| '0' .. '9'
| '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
| '`' | '|' | '~' -> true
| _ -> false

let is_tchar c = is_upper c || is_lower_tchar c

let is_token s =
  let max = String.length s - 1 in
  if max < 0 then false else
  let rec loop i =
    if i > max then true else
    if is_tchar s.[i] then loop (i + 1) else
    false
  in
  loop 0

let err_token_empty = R.error_msg "HTTP token: cannot be empty"
let err_token_char c = R.error_msgf "HTTP token: illegal character (%C)" c

let decode_token_to_lower s =                  (* new string only if needed. *)
  let upper_to_lower c (* 'A'..'Z' *) = Char.(unsafe_of_byte (to_int c + 32)) in
  let max = String.length s - 1 in
  if max < 0 then None else
  let rec to_lower b i =
    if i > max then Some (Bytes.unsafe_to_string b) else
    let c = Bytes.get b i in
    if is_lower_tchar c then to_lower b (i + 1) else
    if is_upper c then (Bytes.set b i (upper_to_lower c); to_lower b (i+1)) else
    None
  in
  let rec loop i =
    if i > max then Some s else
    if is_lower_tchar s.[i] then loop (i + 1) else
    to_lower (Bytes.of_string s) i
  in
  loop 0

(* HTTP whitespace, see https://tools.ietf.org/html/rfc7230#section-3.2.3 *)

let is_ows c = c = ' ' || c = '\t'

let trim_ows s =
  let max = String.length s - 1 in
  if max < 0 then s else
  if not (is_ows s.[0] || is_ows s.[max]) then s else
  let i = ref 0 and j = ref max in
  while !i <= max && is_ows s.[!i] do incr i done;
  while !j >= !i && is_ows s.[!j] do decr j done;
  if !j >= !i then String.with_range s ~first:!i ~len:(!j - !i + 1) else ""

(* HTTP DIGIT https://tools.ietf.org/html/rfc5234#appendix-B.1 *)

let is_digit = Char.Ascii.is_digit
let digit_to_int c = Char.to_int c - 0x30
let digit_of_int i =
  if i < 0 || i > 9 then assert false else
  Char.unsafe_of_byte (i + 0x30)

(* Versions *)

type version = int * int

let decode_version s =
  let parse_version s =
    if (String.length s = 3 && is_digit s.[0] && s.[1] = '.' && is_digit s.[2])
    then Some (digit_to_int s.[0], digit_to_int s.[2])
    else None
  in
  match String.cut ~sep:"/" s with
  | Some ("HTTP", version) -> parse_version version
  | Some _ -> None
  | None -> None

let encode_version (maj, min) =
  if maj < 0 || maj > 9 then invalid_arg (err_version_digit "major" maj);
  if min < 0 || min > 9 then invalid_arg (err_version_digit "minor" min);
  let b = Bytes.create 8 in
  Bytes.set b 0 'H'; Bytes.set b 1 'T'; Bytes.set b 2 'T'; Bytes.set b 3 'P';
  Bytes.set b 4 '/';
  Bytes.set b 5 (Char.unsafe_of_byte (maj + 0x30));
  Bytes.set b 6 '.';
  Bytes.set b 7 (Char.unsafe_of_byte (min + 0x30));
  Bytes.unsafe_to_string b

let pp_version ppf v = Format.pp_print_string ppf (encode_version v)

(* Methods *)

type meth =
  [ `GET | `HEAD | `POST | `PUT | `DELETE | `CONNECT | `OPTIONS | `TRACE
  | `PATCH
  | `Other of string ]

let decode_meth = function
| "GET" -> Some `GET
| "HEAD" -> Some `HEAD
| "POST" -> Some `POST
| "PUT" -> Some `PUT
| "DELETE" -> Some `DELETE
| "CONNECT" -> Some `CONNECT
| "OPTIONS" -> Some `OPTIONS
| "TRACE" -> Some `TRACE
| "PATCH" -> Some `PATCH
| s -> if is_token s then Some (`Other s) else None

let encode_meth = function
| `GET -> "GET"
| `HEAD -> "HEAD"
| `POST -> "POST"
| `PUT -> "PUT"
| `DELETE -> "DELETE"
| `CONNECT -> "CONNECT"
| `OPTIONS -> "OPTIONS"
| `TRACE -> "TRACE"
| `PATCH -> "PATCH"
| `Other s -> if is_token s then s else invalid_arg (err_not_token s)

let pp_meth ppf m = Format.pp_print_string ppf (encode_meth m)

(* Headers *)

module H = struct

  (* Header names *)

  type name = string
  let name s = match decode_token_to_lower s with
  | None -> invalid_arg (err_not_token s)
  | Some n -> n

  let name_equal n n' = n = n'

  let decode_name s = decode_token_to_lower s
  let encode_name n = n
  let pp_name = Format.pp_print_string

  (* Header values *)

  let decode_multi_value s =
    let vs = String.cuts ~sep:"," s in
    List.rev (List.rev_map trim_ows vs)

  let encode_multi_value = function
  | [] -> invalid_arg err_empty_multi_value
  | vs -> String.concat ~sep:"," vs

  (* Header maps *)

  module M = Map.Make (String)
  type headers = string list M.t

  let empty = M.empty
  let is_empty = M.is_empty
  let is_def = M.mem
  let undef = M.remove

  let pp_map ppf hs =
    let pp_header ppf (n, vs) = match vs with
    | [v] -> Format.fprintf ppf "@[<1>(%s@ %S)@]" n v
    | vs ->
        Format.fprintf ppf "@[<1>(%s" n;
        List.iter (fun v -> Format.fprintf ppf "@ %S" v) vs;
        Format.fprintf ppf ")@]"
    in
    Format.fprintf ppf "@[<1>(headers";
    M.iter (fun n vs -> Format.fprintf ppf "@ %a" pp_header (n, vs)) hs;
    Format.fprintf ppf ")@]";
    ()

  (* Single-valued header definition *)

  let find n hs =
    try Some (encode_multi_value (M.find n hs)) with Not_found -> None

  let def n v hs = M.add n [v] hs

  let redef n f hs = match f (find n hs) with
  | None -> undef n hs
  | Some v -> M.add n [v] hs

  let def_if_undef n v hs = if is_def n hs then hs else def n v hs

  let get n hs = match find n hs with
  | None -> invalid_arg (err_header_undef n)
  | Some v -> v

  (* Multi-valued header definition *)

  let find_multi n hs = try Some (M.find n hs) with Not_found -> None

  let def_multi n v hs = M.add n v hs

  let redef_multi n f hs = match f (find_multi n hs) with
  | None -> undef n hs
  | Some vs -> M.add n vs hs

  let def_if_undef_multi n vs hs = if is_def n hs then hs else def_multi n vs hs

  let get_multi n hs = match find_multi n hs with
  | None -> invalid_arg (err_header_undef n)
  | Some vs -> vs

  (* Map folding, predicates and conversion *)

  let fold = M.fold
  let iter = M.iter
  let for_all = M.for_all
  let exists = M.exists
  let keep_if = M.filter
  let partition = M.partition
  let bindings = M.bindings
  let cardinal = M.cardinal

  (* Header name constants *)

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
  let content_range = "content-range"
  let content_type = "content-type"
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
  let server = "server"
  let set_cookie = "set-cookie"
  let te = "te"
  let trailer = "trailer"
  let transfer_encoding = "transfer-encoding"
  let upgrade = "upgrade"
  let user_agent = "user-agent"
  let vary = "vary"
  let via = "via"
  let warning = "warning"
  let www_authenticate = "www-authenticate"
end

type headers = H.headers
let pp_headers = H.pp_map

(* Status *)

type status = int

let status_reason_phrase = function
(* 1XX *)
| 100 -> "Continue"
| 101 -> "Switching Protocols"
(* 2XX *)
| 200 -> "OK"
| 201 -> "Created"
| 202 -> "Accepted"
| 203 -> "Non-Authoritative Information"
| 204 -> "No Content"
| 205 -> "Reset Content"
| 206 -> "Partial Content"
(* 3XX *)
| 300 -> "Multiple Choices"
| 301 -> "Moved Permanently"
| 302 -> "Found"
| 303 -> "See Other"
| 304 -> "Not Modified"
| 305 -> "Use Proxy"
| 307 -> "Temporary Redirect"
(* 4XX *)
| 400 -> "Bad Request"
| 401 -> "Unauthorized"
| 402 -> "Payment Required"
| 403 -> "Forbidden"
| 404 -> "Not Found"
| 405 -> "Method Not Allowed"
| 406 -> "Not Acceptable"
| 407 -> "Proxy Authentication Required"
| 408 -> "Request Time-out"
| 409 -> "Conflict"
| 410 -> "Gone"
| 411 -> "Length Required"
| 412 -> "Precondition Failed"
| 413 -> "Payload Too Large"
| 414 -> "URI Too Long"
| 415 -> "Unsupported Media Type"
| 416 -> "Range Not Satisfiable"
| 417 -> "Expectation Failed"
| 426 -> "Upgrade Required"
(* 5XX *)
| 500 -> "Internal Server Error"
| 501 -> "Not Implemented"
| 502 -> "Bad Gateway"
| 503 -> "Service Unavailable"
| 504 -> "Gateway Time-out"
| 505 -> "HTTP Version Not Supported"
(* XXX *)
| s -> "Unknown Extension"

let pp_status ppf s = Format.fprintf ppf "@[%d %s@]" s (status_reason_phrase s)

let s100_continue = 100
let s101_switching_protocols = 101
let s200_ok = 200
let s201_created = 201
let s202_accepted = 202
let s203_non_authoritative_information = 203
let s204_no_content = 204
let s205_reset_content = 205
let s206_partial_content = 206
let s300_multiple_choices = 300
let s301_moved_permanently = 301
let s302_found = 302
let s303_see_other = 303
let s304_not_modified = 304
let s305_use_proxy = 305
let s307_temporary_redirect = 307
let s400_bad_request = 400
let s401_unauthorized = 401
let s402_payement_required = 402
let s403_forbidden = 403
let s404_not_found = 404
let s405_not_allowed = 405
let s406_not_acceptable = 406
let s407_proxy_authentication_required = 407
let s408_request_time_out = 408
let s409_conflict = 409
let s410_gone = 410
let s411_length_required = 411
let s412_precondition_failed = 412
let s413_payload_too_large = 413
let s414_uri_too_long = 414
let s415_unsupported_media_type = 415
let s416_range_not_satisfiable = 416
let s417_expectation_failed = 417
let s426_upgrade_required = 426
let s500_server_error = 500
let s501_not_implemented = 501
let s502_bad_gateway = 502
let s503_service_unavailable = 503
let s504_gateway_time_out = 504
let s505_http_version_not_supported = 505

(* Path *)

type path = string list

let is_non_pct_pchar = function
  (* see http://tools.ietf.org/html/rfc3986#section-3.3 *)
  (* unreserved *)
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~'
  (* sub-delims *)
  | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '='
  (* *)
  | ':' | '@' -> true
  | _ -> false

let is_hexdig = function
| '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
| _ -> false

let hexdig_to_int = function
| '0' .. '9' as c -> Char.to_int c - 0x30
| 'A' .. 'F' as c -> Char.to_int c - 0x37
| 'a' .. 'f' as c -> Char.to_int c - 0x57
| _ -> assert false

let hexdig_of_int i =
  if i < 0 || i > 15 then assert false else
  if i < 10 then Char.unsafe_of_byte (i + 0x30) else
  Char.unsafe_of_byte (i + 0x37)

let decode_path s =
  let max = String.length s - 1 in
  if max < 0 then None else
  if s.[0] <> '/' then None else
  let buf = lazy (Buffer.create 255) in
  let segs = ref [] in
  let rec seg_buf buf start k = (* decode segment using buf (pct dec needed) *)
    if k > max then Some (List.rev (Buffer.contents buf :: !segs)) else
    match s.[k] with
    | '/' -> (* start new segment *)
        segs := (Buffer.contents buf) :: !segs;
        seg (k + 1) (k + 1)
    | '%' ->
        if k + 2 > max then None else
        let hi = s.[k + 1] in
        let lo = s.[k + 2] in
        if not (is_hexdig hi && is_hexdig lo) then None else
        begin
          let c = (hexdig_to_int hi lsl 4) lor (hexdig_to_int lo) in
          Buffer.add_char buf (Char.unsafe_of_byte c);
          seg_buf buf start (k + 3)
        end
    | c when is_non_pct_pchar c ->
        Buffer.add_char buf c;
        seg_buf buf start (k + 1)
    | _ -> None
  and seg first k =
    if k > max then
      begin
        if first > max
        then (* end with '/' *) Some (List.rev ("" :: (!segs)))
        else
        Some (List.rev (String.with_range s ~first ~len:(max - first + 1) ::
                        !segs))
      end
    else match s.[k] with
    | '/' -> (* start new segment *)
        segs := (String.with_range s ~first ~len:(k - first)) :: !segs;
        seg (k + 1) (k + 1)
    | '%' -> (* percent decode segment via buffer *)
        let buf = Lazy.force buf in
        Buffer.clear buf; Buffer.add_substring buf s first (k - first);
        seg_buf buf first k
    | c when is_non_pct_pchar c -> seg first (k + 1)
    | _ -> None
  in
  seg 1 1

let buffer_encode_path b segs =
  let enc_seg seg =
    for k = 0 to String.length seg - 1 do
      match seg.[k] with
      | c when is_non_pct_pchar c -> Buffer.add_char b c
      | c ->
          let hi = (Char.to_int c lsr 4) land 0xF in
          let lo = (Char.to_int c) land 0xF in
          Buffer.add_char b '%';
          Buffer.add_char b (hexdig_of_int hi);
          Buffer.add_char b (hexdig_of_int lo)
    done
  in
  let add_seg seg = Buffer.add_char b '/'; enc_seg seg in
  if segs = [] then invalid_arg err_path_no_segs else
  List.iter add_seg segs

let encode_path segs =
  let b = Buffer.create 255 in
  buffer_encode_path b segs;
  Buffer.contents b

let pp_path ?(human = false) () ppf p =
  let pp_sep ppf () = Format.fprintf ppf "/" in
  let pp_seg_human = Format.pp_print_string in
  let pp_seg_precise ppf seg = Format.fprintf ppf "%S" seg in
  let pp_seg = if human then pp_seg_human else pp_seg_precise in
  Format.fprintf ppf "/%a" (Format.pp_print_list ~pp_sep pp_seg) p

(* Misc *)

let decode_digits s =
  let max = String.length s - 1 in
  if max < 0 then None else
  let acc = ref 0 in
  let rec loop k =
    if k > max then Some !acc else
    match s.[k] with
    | c when is_digit c ->
        acc := !acc * 10 + digit_to_int c;
        if !acc < 0 then None (* overflow *) else
        loop (k + 1)
    | _ -> None
  in
  loop 0

let encode_digits n =
  if n < 0 then invalid_arg (err_neg_digits n) else
  string_of_int n

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
