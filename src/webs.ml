(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Preliminaries *)

let error_to_failure = function Ok v -> v | Error e -> failwith e

let string_subrange ?(first = 0) ?(last = max_int) s =
  let max_idx = String.length s - 1 in
  let first = max 0 first in
  let last = min max_idx last in
  if first > last then "" else String.sub s first (last - first + 1)

let string_chop_known_prefix ~prefix s =
  let len = String.length prefix in
  String.sub s len (String.length s - len)

let[@inline] upper_to_lower c (* assert is_upper *) =
  Char.(unsafe_chr (code c + 32))

let string_lowercase s =
  let doit s i max =
    let b = Bytes.of_string s in
    for i = i to max do match Bytes.unsafe_get b i with
    | 'A' .. 'Z' as c -> Bytes.unsafe_set b i (upper_to_lower c)
    | _ -> ()
    done;
    Bytes.unsafe_to_string b
  in
  let rec check s i max =
    if i > max then s else match String.unsafe_get s i with
    | 'A' .. 'Z' -> doit s i max
    | c -> check s (i + 1) max
  in
  check s 0 (String.length s - 1)

module String_map = Map.Make (String)
module Fmt = struct
  let str = Format.asprintf
  let pf = Format.fprintf
  let invalid_arg fmt = Format.kasprintf invalid_arg fmt
  let failwith fmt = Format.kasprintf failwith fmt
  let error fmt = Format.kasprintf Result.error fmt
  let cut = Format.pp_print_cut
  let nl = Format.pp_print_newline
  let string = Format.pp_print_string
  let qstring ppf s = pf ppf "%S" s
  let field f pp_v ppf v = pf ppf "@[<h>(%s %a)@]" f pp_v v
  let list = Format.pp_print_list
  let exn_backtrace ~kind ppf (exn, bt) =
    let pp_exn ppf e = string ppf (Printexc.to_string e) in
    let pp_bt ppf bt =
      let bt = Printexc.raw_backtrace_to_string bt in
      if bt = "" then string ppf "No backtrace available." else
      list string ppf (String.split_on_char '\n' bt)
    in
    pf ppf "@[<v>Unexpected %s exception: %a@,%a@]" kind pp_exn exn pp_bt bt
end

module Fpath = struct
  type t = string
  type file_ext = string
  let get_ext p = match String.rindex_opt p '.' with
  | None -> ""
  | Some dpos ->
      let max = String.length p - 1 in
      let seg_start, final_sep_len = match String.rindex_opt p '/' with
      | None -> 0, 0
      | Some i when i <> max -> i + 1, 0
      | Some i when i = max && i = 0 -> assert false
      | Some i ->
          match String.rindex_from_opt p (i - 1) '/' with
          | None -> 0, 1
          | Some i -> i + 1, 1
      in
      if dpos <= seg_start then "" else
      String.sub p dpos (String.length p - dpos - final_sep_len)
end

(* Basic HTTP codecing *)

let crlf = "\r\n"
let err_miss_eq = "missing '='"
let err_miss_dash = "missing '-'"
let err_space_miss = "missing space"
let err_empty_string = "empty string"
let err_digits_neg d = Fmt.str "negative number (%d)" d
let err_digits_char c = Fmt.str "%C is not a digit" c
let err_digits_overflow = "sequence of digits overflows"
let err_token_miss = "missing token"
let err_token t = Fmt.str "%S is not an HTTP token" t
let err_token_char c = Fmt.str "%C not a token character" c
let err_version = "not an HTTP version"
let err_status = "not an HTTP status code"
let err_path_start_slash = "no starting '/'"
let err_path_char c = Fmt.str "%C not a path character" c
let err_path_seg_stray_dir_sep = "stray directory separator in path segment"
let err_path_empty = "empty list of segments"
let err_rl_garbage = "remaining garbage on the request line"
let err_st_garbage = "remaining garbage on the status line"
let err_header_undefined n = Fmt.str "header %s undefined" n
let err_header_miss_delim = "missing ':' delimiter in header"
let err_header_value_char c = "%C not a header value character"
let err_headers_length_conflicts = "conflicting body length specification"
let err_headers_length = "cannot determine body length"
let err_empty_multi_value = "multi value cannot be the empty list"
let err_etag = "not an entity-tag"

let[@inline] is_digit = function '0' .. '9' -> true | _ -> false
let[@inline] digit_to_int c = Char.code c - 0x30 (* assert (is_digit c) *)
let[@inline] digit_of_int i = Char.chr (i + 0x30) (* assert (0 <= i <= 9 *)
let[@inline] str_digit_of_int i = String.make 1 (digit_of_int i)
let[@inline] is_vchar = function '\x21' .. '\x7E' -> true | _ -> false
let[@inline] is_ows c = c = ' ' || c = '\t'

(* HTTP whitespace https://www.rfc-editor.org/rfc/rfc9110#name-whitespace *)

let skip_ows b ~start ~max =
  let rec loop b i max =
    if i > max then i else
    if is_ows (Bytes.get b i) then loop b (i + 1) max else i
  in
  loop b start max

let rskip_ows b ~min ~start =
  let rec loop b i min =
    if i < min then i else
    if is_ows (Bytes.get b i) then loop b (i - 1) min else i
  in
  loop b start min

let trim_ows s =
  if s = "" then "" else
  let max = String.length s - 1 in
  if not (is_ows s.[0] || is_ows s.[max]) then s else
  let i = ref 0 and j = ref max in
  while !i <= max && is_ows s.[!i] do incr i done;
  while !j >= !i && is_ows s.[!j] do decr j done;
  if !j >= !i then String.sub s !i (!j - !i + 1) else ""

let[@inline] decode_sp b ~first ~max =
  if first > max || Bytes.get b first <> ' '
  then failwith err_space_miss else first + 1

(* HTTP token https://www.rfc-editor.org/rfc/rfc9110#name-tokens *)

let[@inline] is_upper = function 'A' .. 'Z' -> true | _ -> false
let[@inline] is_lower_tchar = function
| 'a' .. 'z'
| '0' .. '9'
| '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
| '`' | '|' | '~' -> true
| _ -> false

let is_tchar c = (is_upper[@inlined]) c || (is_lower_tchar[@inlined]) c
let is_token s =
  if s = "" then false else
  let max = String.length s - 1 in
  let rec loop s i =
    if i > max then true else
    if is_tchar s.[i] then loop s (i + 1) else false
  in
  loop s 0

let decode_token b ~first ~max =
  let rec loop b i max =
    if i <= max && is_tchar (Bytes.get b i) then loop b (i + 1) max else i - 1
  in
  match loop b first max with
  | last when last < first -> failwith err_token_miss
  | last -> last + 1, Bytes.sub_string b first (last - first + 1)

let rec token_to_lower ?(first = 0) b =
  let rec loop b i max =
    if i > max then Bytes.unsafe_to_string b else
    let c = Bytes.get b i in
    if is_lower_tchar c then loop b (i + 1) max else
    if is_upper c then (Bytes.set b i (upper_to_lower c); loop b (i + 1) max)
    else failwith (err_token_char c)
  in
  loop b first (Bytes.length b - 1)

let lower_token_of_string s = (* new string only if needed. *)
  if s = "" then failwith err_empty_string else
  let max = String.length s - 1 in
  let rec loop s i max =
    if i > max then s else
    if is_lower_tchar s.[i] then loop s (i + 1) max else
    token_to_lower (Bytes.of_string s) ~first:i
  in
  loop s 0 max

(* HTTP header-field *)

let decode_field_value b ~first ~last =
  let rec loop b i last =
    if i > last then i else
    let c = Bytes.get b i in
    if is_vchar c || is_ows c then loop b (i + 1) last else i
  in
  let i = loop b first last in
  if i = last + 1 then Bytes.sub_string b first (last - first + 1) else
  failwith (err_header_value_char (Bytes.get b i))

let decode_header_field b ~first ~crlf =
  match Bytes.index_from_opt b first ':' with
  | None -> failwith err_header_miss_delim
  | Some i ->
      let name = token_to_lower (Bytes.sub b first (i - first)) in
      let j = skip_ows b ~start:(i + 1) ~max:(crlf - 1) in
      let k = rskip_ows b ~min:(j + 1) ~start:(crlf - 1) in
      let value = decode_field_value b ~first:j ~last:k in
      name, value

module Media_type = struct
  type t = string
  let none = ""
  let application_json = "application/json"
  let application_octet_stream = "application/octet-stream"
  let application_x_www_form_urlencoded = "application/x-www-form-urlencoded"
  let text_css = "text/css"
  let text_html = "text/html;charset=utf-8"
  let text_javascript = "text/javascript"
  let text_plain = "text/plain;charset=utf-8"
  let multipart_byteranges = "multipart/byteranges"
  let multipart_form_data = "multipart/form-data"

  let get_type s =
    let rec after_token s i max =
      if i <= max && is_tchar s.[i] then after_token s (i + 1) max else i
    in
    let max = String.length s - 1 in
    let after = after_token s 0 max in
    if after > max then s else
    if s.[after] <> '/' then String.sub s 0 after else
    let after = after_token s (after + 1) max in
    if after > max then s else String.sub s 0 after

  type fpath = Fpath.t
  type file_ext = Fpath.file_ext
  type file_ext_map = t String_map.t

  let default_file_ext_map =
    String_map.empty
    |> String_map.add ".aac"  "audio/aac"
    |> String_map.add ".avi"  "video/x-msvideo"
    |> String_map.add ".bin"  "application/octet-stream"
    |> String_map.add ".bmp"  "image/bmp"
    |> String_map.add ".bz"   "application/x-bzip"
    |> String_map.add ".bz2"  "application/x-bzip2"
    |> String_map.add ".css"  "text/css"
    |> String_map.add ".gz"   "application/gzip"
    |> String_map.add ".gif"  "image/gif"
    |> String_map.add ".htm"  "text/html"
    |> String_map.add ".html" "text/html"
    |> String_map.add ".ics"  "text/calendar"
    |> String_map.add ".jpeg" "image/jpeg"
    |> String_map.add ".jpg"  "image/jpeg"
    |> String_map.add ".js"   "text/javascript"
    |> String_map.add ".json" "application/json"
    |> String_map.add ".jsonldx" "application/ld+json"
    |> String_map.add ".md"   "text/markdown;charset=utf-8"
    |> String_map.add ".midi" "audio/midi audio/x-midi"
    |> String_map.add ".mjs"  "text/javascript"
    |> String_map.add ".mp3"  "audio/mpeg"
    |> String_map.add ".mpeg" "video/mpeg"
    |> String_map.add ".oga"  "audio/ogg"
    |> String_map.add ".ogv"  "video/ogg"
    |> String_map.add ".ogx"  "application/ogg"
    |> String_map.add ".opus" "audio/opus"
    |> String_map.add ".otf"	"font/otf"
    |> String_map.add ".png"	"image/png"
    |> String_map.add ".pdf"	"application/pdf"
    |> String_map.add ".rar"	"application/vnd.rar"
    |> String_map.add ".rtf"	"application/rtf"
    |> String_map.add ".svg"	"image/svg+xml"
    |> String_map.add ".tar"	"application/x-tar"
    |> String_map.add ".tif"  "image/tiff"
    |> String_map.add ".tiff" "image/tiff"
    |> String_map.add ".ts"   "video/mp2t"
    |> String_map.add ".ttf"	"font/ttf"
    |> String_map.add ".txt"	"text/plain;charset=utf-8"
    |> String_map.add ".wav"	"audio/wav"
    |> String_map.add ".weba"	"audio/webm"
    |> String_map.add ".webm"	"video/webm"
    |> String_map.add ".webp"	"image/webp"
    |> String_map.add ".woff"	"font/woff"
    |> String_map.add ".woff2" "font/woff2"
    |> String_map.add ".xhtml" "application/xhtml+xml"
    |> String_map.add ".xml"  "application/xml"
    |> String_map.add ".zip"  "application/zip"
    |> String_map.add ".7z"   "application/x-7z-compressed"

  let of_file_ext ?(map = default_file_ext_map) ext =
    let default = application_octet_stream in
    Option.value (String_map.find_opt ext map) ~default

  let of_filepath ?map file = of_file_ext ?map (Fpath.get_ext file)
end

module Base64 = struct (* See https://www.rfc-editor.org/rfc/rfc4648 *)
  type error =
  | Invalid_letter of (bool * int * char)
  | Unexpected_eoi of bool

  let url u = if u then "url" else ""
  let error_message = function
  | Unexpected_eoi u -> Fmt.str "unexpected end of base64%s input" (url u)
  | Invalid_letter (u, i, c) ->
      Fmt.str "%d: invalid base64%s alphabet character %C" i (url u) c

  let error_string r = Result.map_error error_message r

  let alpha =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  let alpha_url =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

  let _encode ~url s =
    let rec loop alpha len e ei s i  =
      if i >= len then Bytes.unsafe_to_string e else
      let i0 = i and i1 = i + 1 and i2 = i + 2 in
      let b0 = Char.code s.[i0] in
      let b1 = if i1 >= len then 0 else (Char.code s.[i1]) in
      let b2 = if i2 >= len then 0 else (Char.code s.[i2]) in
      let u = (b0 lsl 16) lor (b1 lsl 8) lor b2 in
      let c0 = alpha.[u lsr 18] in
      let c1 = alpha.[(u lsr 12) land 63] in
      let c2 = if i1 >= len then '=' else alpha.[(u lsr 6) land 63] in
      let c3 = if i2 >= len then '=' else alpha.[u land 63] in
      Bytes.set e (ei    ) c0;
      Bytes.set e (ei + 1) c1;
      Bytes.set e (ei + 2) c2;
      Bytes.set e (ei + 3) c3;
      loop alpha len e (ei + 4) s (i2 + 1)
    in
    match String.length s with
    | 0 -> ""
    | len ->
        let alpha = if url then alpha_url else alpha in
        loop alpha len (Bytes.create (((len + 2) / 3) * 4)) 0 s 0

  let _decode ~url s =
    let exception Alpha_error of int in
    let decode_alpha len s i = match s.[i] with
    | 'A' .. 'Z' as c -> Char.code c - 0x41
    | 'a' .. 'z' as c -> Char.code c - 0x61 + 26
    | '0' .. '9' as c -> Char.code c - 0x30 + 52
    | '+' when not url -> 62 | '/' when not url -> 63
    | '-' when url -> 62 | '_' when url -> 63
    | '=' when i = len - 1 || i = len - 2 -> -1
      | _ -> raise_notrace (Alpha_error i)
    in
    let rec loop len d di s i =
      if i >= len then Bytes.unsafe_to_string d else
      let a0 = decode_alpha len s (i    ) in
      let a1 = decode_alpha len s (i + 1) in
      let a2 = decode_alpha len s (i + 2) in
      let a3 = decode_alpha len s (i + 3) in
      let b0 = Char.unsafe_chr ((a0 lsl 2) lor (a1 lsr 4)) in
      Bytes.set d di b0;
      if a2 = -1 then begin
        if a3 = -1 then Bytes.unsafe_to_string d else
        raise_notrace (Alpha_error (i + 2))
      end else begin
        let b1 = Char.unsafe_chr (((a1 land 0xF) lsl 4) lor (a2 lsr 2)) in
        Bytes.set d (di + 1) b1;
        if a3 = -1 then Bytes.unsafe_to_string d else
        let b2 = Char.unsafe_chr (((a2 land 0x3) lsl 6) lor a3) in
        Bytes.set d (di + 2) b2;
        loop len d (di + 3) s (i + 4)
      end
    in
    let len = String.length s in
    if len = 0 then Ok "" else
    if len mod 4 <> 0 then Error (Unexpected_eoi url) else
    let dlen = (len / 4) * 3 in
    let dlen = if s.[len - 1] = '=' then dlen - 1 else dlen in
    let dlen = if s.[len - 2] = '=' then dlen - 1 else dlen in
    try Ok (loop len (Bytes.create dlen) 0 s 0) with
    | Alpha_error i -> Error (Invalid_letter (url, i, s.[i]))

  let encode s = _encode ~url:false s
  let decode s = _decode ~url:false s
  let url_encode s = _encode ~url:true s
  let url_decode s = _decode ~url:true s
end

module Pct = struct (* See https://tools.ietf.org/html/rfc3986 *)
  type kind = [ `Uri_component | `Uri ]

  let char_is_uri_component_verbatim = function
  (* unreserved *)
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~'
  (* sub-delims *)
  | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' -> true
  | _ -> false

  let char_is_uri_verbatim = function
  (* unreserved *)
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~'
  (* sub-delims *)
  | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '='
  (* gen-delims *)
  | ':' | '/' | '?' | '#' | '[' | ']' | '@' -> true
  | _ -> false

  let is_hexdig = function
  | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true | _ -> false

  let hexdig_to_int = function
  | '0' .. '9' as c -> Char.code c - 0x30
  | 'A' .. 'F' as c -> Char.code c - 0x37
  | 'a' .. 'f' as c -> Char.code c - 0x57
  | _ -> assert false

  let unsafe_hexdig_of_int i =
    if i < 10 then Char.unsafe_chr (i + 0x30) else Char.unsafe_chr (i + 0x37)

  let encode_to_buffer is_verbatim b s =
    for k = 0 to String.length s - 1 do match s.[k] with
    | c when is_verbatim c -> Buffer.add_char b c
    | c ->
        let hi = (Char.code c lsr 4) land 0xF in
        let lo = (Char.code c) land 0xF in
        Buffer.add_char b '%';
        Buffer.add_char b (unsafe_hexdig_of_int hi);
        Buffer.add_char b (unsafe_hexdig_of_int lo)
    done

  let decode_to_buffer b s ~first ~last =
    let i = ref first in
    while (!i <= last) do match s.[!i] with
    | '+' -> Buffer.add_char b ' '; incr i;
    | '%' when !i + 2 <= last ->
        let hi = s.[!i + 1] and lo = s.[!i + 2] in
        begin match is_hexdig hi && is_hexdig lo with
        | false -> Buffer.add_char b '%'; incr i
        | true ->
            let c = (hexdig_to_int hi lsl 4) lor (hexdig_to_int lo) in
            Buffer.add_char b (Char.unsafe_chr c);
            i := !i + 3
        end
    | c -> Buffer.add_char b c; incr i
    done

  let encode kind s =
    (* One day we should benchmark whether the scan first to determine
       length and then use Bytes directly is faster – see
       Query.pct_encode_space_as_plus – one day. *)
    let is_verbatim = match kind with
    | `Uri_component -> char_is_uri_component_verbatim
    | `Uri -> char_is_uri_verbatim
    in
    let b = Buffer.create (String.length s * 2) in
    encode_to_buffer is_verbatim b s;
    Buffer.contents b

  let decode s =
    let b = Buffer.create (String.length s) in
    decode_to_buffer b s ~first:0 ~last:(String.length s - 1);
    Buffer.contents b
end

module Digits = struct
  let decode s =
    (* https://www.rfc-editor.org/rfc/rfc5234#appendix-B.1 *)
    if s = "" then Error err_empty_string else
    let rec loop k acc max =
      if k > max then Ok acc else
      let c = s.[k] in
      if not (is_digit c) then Error (err_digits_char c) else
      let acc = acc * 10 + digit_to_int c in
      if acc < 0 then Error err_digits_overflow else
      loop (k + 1) acc max
    in
    loop 0 0 (String.length s - 1)

  let encode n =
    if n < 0 then invalid_arg (err_digits_neg n) else string_of_int n
end

module Version = struct
  type t = int * int
  let v11 = (1, 1)
  let v20 = (2, 0)
  let v30 = (3, 0)

  let decode_of_bytes b ~first ~max =
    if max - first + 1 < 8 then failwith err_version else
    let[@inline] c b i = Bytes.get b (first + i) in
    if c b 0 = 'H' && c b 1 = 'T' && c b 2 = 'T' && c b 3 = 'P' &&
       c b 4 = '/' && is_digit (c b 5) && c b 6 = '.' && is_digit (c b 7)
    then first + 8, (digit_to_int (c b 5), digit_to_int (c b 7))
    else failwith err_version

  let decode s =
    if String.length s <> 8 then Error err_version else
    match decode_of_bytes (Bytes.unsafe_of_string s) ~first:0 ~max:7 with
    | exception Failure e -> Error e | (_, v) -> Ok v

  let encode (maj, min) =
    let b = Bytes.create 8 and s = Bytes.set in
    Bytes.blit_string "HTTP/" 0 b 0 5;
    s b 5 (digit_of_int maj); s b 6 '.'; s b 7 (digit_of_int min);
      Bytes.unsafe_to_string b

  let pp ppf v = Fmt.string ppf (encode v)
end

module Method = struct
  type t =
  [ `GET | `HEAD | `POST | `PUT | `DELETE | `CONNECT | `OPTIONS | `TRACE
  | `PATCH | `Other of string ]

  let of_token = function
  | "GET" -> `GET | "HEAD" -> `HEAD | "POST" -> `POST | "PUT" -> `PUT
  | "DELETE" -> `DELETE | "CONNECT" -> `CONNECT | "OPTIONS" -> `OPTIONS
  | "TRACE" -> `TRACE | "PATCH" -> `PATCH | s -> `Other s

  let decode' b ~first ~max =
    let first, token = decode_token b ~first ~max in
    first, of_token token

  let decode s = match of_token s with
  | `Other s as o -> if is_token s then Ok o else Error (err_token s)
  | m -> Ok m

  let encode = function
  | `GET -> "GET" | `HEAD -> "HEAD" | `POST -> "POST" | `PUT -> "PUT"
  | `DELETE -> "DELETE" | `CONNECT -> "CONNECT" | `OPTIONS -> "OPTIONS"
  | `TRACE -> "TRACE" | `PATCH -> "PATCH"
  | `Other s -> if is_token s then s else invalid_arg (err_token s)

  let pp ppf m = Fmt.string ppf (encode m)

  (* Constraints *)

  type 'a constraint' = t * 'a

  let constrain ~allowed m =
    let rec loop mr = function
    | m :: ms -> if (fst m) = mr then Ok (snd m) else loop mr ms
    | [] -> Error allowed
    in
    loop m allowed

  let connect = `CONNECT, `CONNECT
  let delete = `DELETE, `DELETE
  let get = `GET, `GET
  let head = `HEAD, `HEAD
  let options = `OPTIONS, `OPTIONS
  let other s o = `Other s, o
  let patch = `PATCH, `PATCH
  let post = `POST, `POST
  let put = `PUT, `PUT
  let trace = `TRACE, `TRACE
end

module Path = struct
  type t = string list
  let none = []
  let root = [""]
  let _undot_and_compress ~check (* also applied on discarded segs *) p =
    let rec loop acc = function
    | "." :: [] -> loop ("" :: acc) []
    | "." :: ps -> loop acc ps
    | ".." :: ps when acc = [] -> loop acc ps
    | ".." :: ps -> loop (List.tl acc) ps
    | "" :: [] -> loop ("" :: acc) []
    | "" :: ps -> loop acc ps
    | seg :: ps -> if check seg then loop (seg :: acc) ps else failwith ""
    | [] -> if acc = [] then [""] else List.rev acc
    in
    loop [] p

  let undot_and_compress p = _undot_and_compress ~check:(Fun.const true) p

  let strip_prefix ~prefix p =
    if prefix = [] || p = [] then [] else
    if prefix = [""] then p else
    let rec loop pre p = match pre, p with
    | preseg :: pre, pseg :: p when String.equal preseg pseg -> loop pre p
    | ([] | [""]), (_ :: _ as p) -> p
    | [], [] -> [""]
    | _ -> []
    in
    loop prefix p

  let concat p0 p1 = match p0, p1 with
  | [], p1 -> p1
  | p0, [] -> p0
  | p0, p1 ->
      match List.rev p0 with
      | "" :: r -> List.rev_append r p1
      | r -> List.rev_append r p1

  let relative ~src ~dst =
    let rec dotdots segs ~on:acc = match segs with
    | _ :: segs -> dotdots segs ~on:(".." :: acc) | [] -> acc
    in
    match src, dst with (* Simpler if root paths are handled separately *)
    | [_], [""] -> ["."]
    | src, [""] -> dotdots (List.tl src) ~on:[]
    | [_], dst -> dst
    | src, dst ->
        let rec loop last src dst = match src, dst with
        | r :: src, p :: dst when String.equal r p -> loop r src dst
        | [], [] -> [if last = "" then "." else last] (* root = path *)
        | [], q -> last :: q (* root = r and path = r/q *)
        | p, [] -> dotdots p ~on:[last] (* root = r/q and path = r *)
        | p, [""] -> dotdots p ~on:[last; ""] (* root = r/q and path = r/ *)
        | p, q -> dotdots (List.tl p) ~on:q (* root = r/p  path = r/q *)
        in
        loop "" src dst

  (* File paths *)

  type fpath = Fpath.t

  let has_no_dir_seps s = (* String.forall :-( *)
    try
      for i = 0 to String.length s - 1
      do if s.[i] = '/' || s.[i] = '\\' then raise Exit else () done;
      true
    with Exit -> false

  let has_dir_seps s = not (has_no_dir_seps s)

  let to_absolute_filepath p =
    match _undot_and_compress ~check:has_no_dir_seps p with
    | [] -> Error err_path_empty
    | [""] -> Ok "/"
    | ps -> Ok (String.concat "/" ("" :: ps))
    | exception Failure _ -> Error err_path_seg_stray_dir_sep

  let prefix_filepath ~prefix:p0 p1 =
    let l0 = String.length p0 and l1 = String.length p1 in
    if l0 = 0 then p1 else
    if l1 = 0 then p0 else
    match p0.[l0 - 1], p1.[0] with
    | '/', '/' ->  String.sub p0 0 (l0 - 1) ^ p1
    | '/', _ | _, '/' -> p0 ^ p1
    | _, _ -> String.concat "/" [p0; p1]

  let filepath_ext = Fpath.get_ext

  (* Converting *)

  let decode_segment b ~first ~last s =
    Buffer.clear b; Pct.decode_to_buffer b ~first ~last s; Buffer.contents b

  (* The following decode allows percents not necessarily followed
     by two hex-digits, RFC 3986 wouldn't allow that, in the whatwg
     we get a validation error but the parsing continues. In
     practice curling URLs with such paths works. *)

  let decode s =
    if s = "" then Error err_empty_string else
    let max = String.length s - 1 in
    if s.[0] <> '/' then Error err_path_start_slash else
    let rec loop acc b s ~first i = match i > max with
    | true -> Ok (List.rev (decode_segment b ~first ~last:max s :: acc))
    | false ->
        match s.[i] with
        | '/' ->
            let seg = decode_segment b ~first ~last:(i - 1) s in
            let i = i + 1 in
            loop (seg :: acc) b s ~first:i i
        | c when c = '%' || Pct.char_is_uri_component_verbatim c ->
            loop acc b s ~first (i + 1)
        | c -> Error (err_path_char c)
    in
    loop [] (Buffer.create 255) s ~first:1 1

  let buffer_encode_path b segs =
    let add_seg seg =
      Buffer.add_char b '/';
      Pct.encode_to_buffer Pct.char_is_uri_component_verbatim b seg
    in
    List.iter add_seg segs

  let encode segs =
    let b = Buffer.create 255 in buffer_encode_path b segs; Buffer.contents b

  let pp ppf p = Fmt.string ppf (String.concat "/" ("" :: p))
  let pp_dump ppf p =
    let pp_sep ppf () = Fmt.pf ppf "@ " in
    let pp_seg ppf s = Fmt.pf ppf "%S" s in
    Fmt.list ~pp_sep pp_seg ppf p

  let and_query_string_of_request_target s =
    let subrange ?first ?last s = Some (string_subrange ?first ?last s) in
    let find_query ~first s = String.index_from_opt s first '?' in
    let none = None, None in
    let p, q = match s with
    | "" (* just in case *) | "*" -> none
    | s when s.[0] = '/' -> (* origin-form *)
        begin match find_query ~first:0 s with
        | None -> Some s, None
        | Some i -> subrange ~last:(i - 1) s, subrange ~first:(i + 1) s
        end
    | s ->
        (* Extract a path and/or query from absolute-form and handles
           authority-form (by doing nothing). Look for // then the first /
           (if any) and/or the first ? *)
        Option.fold ~none ~some:Fun.id @@
        let ( let* ) = Option.bind in
        let* i = String.index_from_opt s 0 '/' in
        let* j = String.index_from_opt s (i + 1) '/' in
        if j <> i + 1 then (* no // *) None else
        match String.index_from_opt s (j + 1) '/' with
        | None -> (* no path, we can still have a query *)
            (match find_query ~first:(j + 1) s with
            | None -> None
            | Some k -> Some (None, subrange ~first:(k + 1) s))
        | Some k ->
            match find_query ~first:(k + 1) s with
            | None -> Some (subrange ~first:k s, None)
            | Some l ->
                Some
                  (subrange ~first:k ~last:(l - 1) s,
                   subrange ~first:(l + 1) s)
    in
    match p with
    | None -> Ok ([], q)
    | Some p -> match decode p with Error _ as e -> e | Ok segs -> Ok (segs, q)
end

module Query = struct
  type t = string list (* The list is never empty *) String_map.t
  let empty = String_map.empty
  let def k v q = String_map.add k [v] q
  let undef = String_map.remove
  let add_value k v q =
    let vs = match String_map.find_opt k q with
    | None -> [v] | Some vs -> vs @ [v]
    in
    String_map.add k vs q

  (* Lookup *)

  let find_first k q = match String_map.find_opt k q with
  | None -> None | Some vs -> Some (List.hd vs)

  let find_all k q = Option.value ~default:[] (String_map.find_opt k q)
  let fold f q acc =
    let bindings k vs acc = List.fold_left (fun acc v -> f k v acc) acc vs in
    String_map.fold bindings q acc

  (* Predicates *)

  let is_empty = String_map.is_empty
  let mem = String_map.mem

  (* Converting *)

  (* https://url.spec.whatwg.org/\
       #application-x-www-form-urlencoded-percent-encode-set *)
  let[@inline] needs_encoding = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '*' | '-' | '.' | '_' -> false
  | _ -> true

  let pct_decode_space_as_plus b s ~first ~last =
    Buffer.clear b;
    let i = ref first in
    while (!i <= last) do match String.get s !i with
    | '+' -> Buffer.add_char b ' '; incr i;
    | '%' when !i + 2 <= last ->
        let hi = s.[!i + 1] and lo = s.[!i + 2] in
        begin match Pct.is_hexdig hi && Pct.is_hexdig lo with
        | false -> Buffer.add_char b '%'; incr i
        | true ->
            let c = (Pct.hexdig_to_int hi lsl 4) lor (Pct.hexdig_to_int lo) in
            Buffer.add_char b (Char.unsafe_chr c);
            i := !i + 3
        end
    | c -> Buffer.add_char b c; incr i
    done;
    Buffer.contents b

  let pct_encode_space_as_plus s =
    let space = ref false in
    let len = ref 0 in
    for i = 0 to String.length s - 1 do match String.unsafe_get s i with
    | ' ' -> space := true; incr len
    | c when needs_encoding c -> len := !len + 3
    | c -> incr len
    done;
    if not (!space) && !len = String.length s then s else
    let out = ref 0 in
    let b = Bytes.create !len in
    for i = 0 to String.length s - 1 do match String.unsafe_get s i with
    | ' ' -> Bytes.set b !out '+'; incr out
    | c when not (needs_encoding c) -> Bytes.set b !out c; incr out
    | c ->
        let hi = (Char.code c lsr 4) land 0xF in
        let lo = (Char.code c) land 0xF in
        Bytes.set b !out '%'; incr out;
        Bytes.set b !out (Pct.unsafe_hexdig_of_int hi); incr out;
        Bytes.set b !out (Pct.unsafe_hexdig_of_int lo); incr out;
    done;
    Bytes.unsafe_to_string b

  let decode s =
    (* See https://url.spec.whatwg.org/#urlencoded-parsing, note that we
       do not check UTF-8 validty. *)
    let rec loop b acc = function
    | "" :: kvs -> loop b acc kvs
    | kv :: kvs ->
        let max = String.length kv - 1 in
        let k, v = match String.index_opt kv '=' with
        | None ->
            pct_decode_space_as_plus b kv ~first:0 ~last:max, ""
        | Some i ->
            pct_decode_space_as_plus b kv ~first:0 ~last:(i - 1),
            pct_decode_space_as_plus b kv ~first:(i + 1) ~last:max
        in
        loop b (add_value k v acc) kvs
    | [] -> acc
    in
    loop (Buffer.create 255) empty (String.split_on_char '&' s)

  let encode q =
    (* See https://url.spec.whatwg.org/#urlencoded-serializing *)
    let first = ref true in
    let add k v b =
      (if !first then first := false else (Buffer.add_char b '&'));
      Buffer.add_string b (pct_encode_space_as_plus k);
      Buffer.add_char b '=';
      Buffer.add_string b (pct_encode_space_as_plus v);
      b
    in
    Buffer.contents (fold add q (Buffer.create 255))

  let pp ppf q =
    let pp_sep ppf () = Fmt.pf ppf "@ " in
    let pp_v ppf v = Fmt.pf ppf "\"%s\"" v in
    let pp_vs = Fmt.list ~pp_sep pp_v in
    let pp_binding ppf (k, vs) = Fmt.field k pp_vs ppf vs in
    Fmt.pf ppf "@[<v>%a@]" (Fmt.list pp_binding) (String_map.bindings q)
end

module Body = struct

  (* Byte readers and writers *)

  type byte_reader = unit -> (bytes * int * int) option
  type byte_writer = (bytes * int * int) option -> unit

  let string_to_byte_reader s =
    (* N.B. unsafe is ok: the consumer is not supposed to mutate the bytes. *)
    let s = ref (Some (Bytes.unsafe_of_string s, 0, String.length s))  in
    fun () -> let v = !s in s := None; v

  let string_to_byte_writer s = fun write ->
    (* N.B. unsafe is ok: the consumer is not supposed to mutate the bytes. *)
    write (Some (Bytes.unsafe_of_string s, 0, String.length s));
    write None

  let byte_reader_to_string r = match r () with
  | None -> ""
  | Some (bytes, start, len) ->
      let b = Buffer.create 1024 in
      Buffer.add_subbytes b bytes start len;
      let rec loop b = match r () with
      | None -> Buffer.contents b
      | Some (bytes, start, l) -> Buffer.add_subbytes b bytes start l; loop b
      in
      loop b

  let byte_writer_to_string w =
    let b = Buffer.create 1024 in
    let write = function
    | None -> ()
    | Some (bytes, start, len) -> Buffer.add_subbytes b bytes start len
    in
    let () = w write in
    Buffer.contents b

  (* Body contents *)

  type 'a writer = 'a -> unit
  type custom_content = ..
  type content =
  | Empty
  | Byte_reader of byte_reader
  | Byte_writer of byte_writer writer
  | Custom of custom_content

  (* Bodies *)

  type t =
    { content : content;
      content_type : Media_type.t;
      content_length : int option; }

  let make
      ?content_length ?(content_type = Media_type.application_octet_stream)
      content
    =
    let () = match content_length with
    | Some l when l < 0 -> Fmt.invalid_arg "negative content_length (%d)" l
    | _ -> ()
    in
    { content; content_type; content_length }

  let empty = make ~content_length:0 ~content_type:Media_type.none Empty

  let of_custom_content ?content_length ?content_type c =
    make ?content_length ?content_type (Custom c)

  let of_byte_writer ?content_length ?content_type w =
    make ?content_length ?content_type (Byte_writer w)

  let of_byte_reader ?content_length ?content_type r =
    make ?content_length ?content_type (Byte_reader r)

  let of_string ?content_type s =
    let content_length = String.length s in
    make ~content_length ?content_type (Byte_writer (string_to_byte_writer s))

  let to_byte_reader b = match b.content with
  | Empty -> Ok (fun () -> None)
  | Byte_reader r -> Ok r
  | Byte_writer w -> Ok (string_to_byte_reader (byte_writer_to_string w))
  | Custom _ -> Error "Don't know how to read custom body content"

  let to_string b = match b.content with
  | Empty -> Ok ""
  | Byte_reader r -> Ok (byte_reader_to_string r)
  | Byte_writer w -> Ok (byte_writer_to_string w)
  | Custom _ -> Error "Don't know how to read custom body content"

  let is_empty b = match b.content with Empty -> true | _ -> false

  let pp ppf b =
    let pp_type ppf b =
      if b.content_type = Media_type.none then () else
      Fmt.pf ppf " type:%s" b.content_type
    in
    let pp_length ppf b = match b.content_length with
    | None -> () | Some l -> Fmt.pf ppf " length:%d" l
    in
    begin match b.content with
    | Empty -> Fmt.string ppf "<empty"
    | Byte_reader _ -> Fmt.string ppf "<byte_reader"
    | Byte_writer _ -> Fmt.string ppf "<byte_writer"
    | Custom _ -> Fmt.string ppf "<custom"
    end;
    pp_type ppf b; pp_length ppf b; Fmt.string ppf ">"

  let content b = b.content
  let content_type b = b.content_type
  let content_length b = b.content_length
end

module Scheme = struct
  type t = [ `Http | `Https ]

  let tcp_port = function `Http -> 80 | `Https -> 443
  let split ~url =
    let https = "https://" and http = "http://" in
    if String.starts_with ~prefix:https url
    then (`Https, string_chop_known_prefix ~prefix:https url) else
    if String.starts_with ~prefix:http url
    then (`Http, string_chop_known_prefix ~prefix:http url)
    else Fmt.failwith "URL '%s': not an HTTP URL" url
end

module Status = struct
  type t = int
  let reason_phrase = function
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
  | 308 -> "Permanent Redirect"
  (* 4XX *)
  | 400 -> "Bad Request"
  | 401 -> "Unauthorized"
  | 402 -> "Payment Required"
  | 403 -> "Forbidden"
  | 404 -> "Not Found"
  | 405 -> "Method Not Allowed"
  | 406 -> "Not Acceptable"
  | 407 -> "Proxy Authentication Required"
  | 408 -> "Request Timeout"
  | 409 -> "Conflict"
  | 410 -> "Gone"
  | 411 -> "Length Required"
  | 412 -> "Precondition Failed"
  | 413 -> "Content Too Large"
  | 414 -> "URI Too Long"
  | 415 -> "Unsupported Media Type"
  | 416 -> "Range Not Satisfiable"
  | 417 -> "Expectation Failed"
  | 418 -> "I'm a teapot"
  | 426 -> "Upgrade Required"
  (* 5XX *)
  | 500 -> "Internal Server Error"
  | 501 -> "Not Implemented"
  | 502 -> "Bad Gateway"
  | 503 -> "Service Unavailable"
  | 504 -> "Gateway Time-out"
  | 505 -> "HTTP Version Not Supported"
  (* XXX *)
  | _ -> "Unknown"

  let pp ppf status = Fmt.pf ppf "@[%d – %s@]" status (reason_phrase status)

  let continue_100 = 100
  let switching_protocols_101 = 101
  let ok_200 = 200
  let created_201 = 201
  let accepted_202 = 202
  let non_authoritative_information_203 = 203
  let no_content_204 = 204
  let reset_content_205 = 205
  let partial_content_206 = 206
  let multiple_choices_300 = 300
  let moved_permanently_301 = 301
  let found_302 = 302
  let see_other_303 = 303
  let not_modified_304 = 304
  let use_proxy_305 = 305
  let temporary_redirect_307 = 307
  let permanent_redirect_308 = 308
  let bad_request_400 = 400
  let unauthorized_401 = 401
  let payement_required_402 = 402
  let forbidden_403 = 403
  let not_found_404 = 404
  let method_not_allowed_405 = 405
  let not_acceptable_406 = 406
  let proxy_authentication_required_407 = 407
  let request_time_out_408 = 408
  let conflict_409 = 409
  let gone_410 = 410
  let length_required_411 = 411
  let precondition_failed_412 = 412
  let content_too_large_413 = 413
  let uri_too_long_414 = 414
  let unsupported_media_type_415 = 415
  let range_not_satisfiable_416 = 416
  let expectation_failed_417 = 417
  let i'm_a_teapot_418 = 418
  let upgrade_required_426 = 426
  let server_error_500 = 500
  let not_implemented_501 = 501
  let bad_gateway_502 = 502
  let service_unavailable_503 = 503
  let gateway_time_out_504 = 504
  let http_version_not_supported_505 = 505

  let decode_of_bytes b ~first ~max =
    if max - first + 1 < 3 then failwith err_status else
    let[@inline] c b i = Bytes.get b (first + i) in
    if is_digit (c b 0) && is_digit (c b 1) && is_digit (c b 2)
    then first + 3,
         (digit_to_int (c b 0) * 100 + digit_to_int (c b 1) * 10 +
          digit_to_int (c b 2))
    else failwith err_status
end

module Headers = struct
  module Name = struct
    type t = string
    let v s = try lower_token_of_string s with Failure e -> invalid_arg e
    let equal = String.equal
    let compare = String.compare
    let pp = Fmt.string
    let encode n = n
    let decode s = try Ok (lower_token_of_string s) with Failure e -> Error e
  end
  let name = Name.v

  (* Standard header names *)

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
  let content_language = "content-languyage"
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

  (* Headers *)

  type t = string String_map.t (* always lowercased by Name.v *)
  let empty = String_map.empty
  let def = String_map.add
  let def_if_some n o hs = match o with None -> hs | Some v -> def n v hs
  let def_if_undef n v hs =
    let def = function None -> Some v | def -> def in
    String_map.update n def hs

  let undef = String_map.remove
  let _append_value sep n v hs = match String_map.find_opt n hs with
  | None -> String_map.add n v hs
  | Some v' -> String_map.add n (String.concat sep [v; v']) hs

  let add_value n v hs = _append_value "," n v hs
  let add_set_cookie v hs = _append_value "\x00" "set-cookie" v hs
  let override hs ~by =
    let merge_right _ _ v = Some v in
    String_map.union merge_right hs by

  (* Lookups *)

  let find ?(lowervalue = false) n hs = match lowervalue with
  | true -> Option.map string_lowercase (String_map.find_opt n hs)
  | false -> String_map.find_opt n hs

  let get ?lowervalue n hs = match find ?lowervalue n hs with
  | None -> invalid_arg (err_header_undefined n)
  | Some v -> v

  let fold = String_map.fold

  (* Header values *)

  let values_of_set_cookie_value s = String.split_on_char '\x00' s
  let values_of_string ?(sep = ',') s =
    List.rev @@ List.rev_map trim_ows (String.split_on_char sep s)

  let values_to_string ?(sep = ',') = function
  | [] -> invalid_arg err_empty_multi_value
  | vs -> String.concat (String.make 1 sep) vs

  let value_is_token = is_token

  let pp ppf hs =
    let pp_header ppf (n, v)  =
      if not (n = "set-cookie") then Fmt.field n Fmt.qstring ppf v else
      let cs = values_of_set_cookie_value v in
      List.iter (Fmt.field "set-cookie" Fmt.qstring ppf) cs
    in
    Fmt.list pp_header ppf (String_map.bindings hs)

  (* Predicates *)

  let is_empty = String_map.is_empty
  let mem = String_map.mem

  (* Header specific *)

  let request_body_length hs =
    let len = find content_length hs in
    let tr = find transfer_encoding ~lowervalue:true hs in
    match len, tr with
    | Some _, Some _ -> Error err_headers_length_conflicts (* §3.3.3 3. *)
    | None, None -> Ok (`Length 0) (* §3.3.3 6. *)
    | Some l, None -> Result.map (fun l -> `Length l) (Digits.decode l)
    | None, Some tes ->
        (* §3.3.3 3. *)
        let tes = values_of_string tes in
        let chunked = String.equal "chunked" (List.hd (List.rev tes)) in
        if chunked then Ok `Chunked else Error err_headers_length

  let get_host scheme hs =
    let find_hostname_port scheme host = match String.rindex_opt host ':' with
    | None -> Ok (host, Scheme.tcp_port scheme)
    | Some j ->
        let port = String.sub host (j + 1) (String.length host - j - 1) in
        if port = ""
        then Ok (String.sub host 0 j, (Scheme.tcp_port scheme)) else
        match int_of_string_opt port with
        | Some port -> Ok (String.sub host 0 j, port)
        | None -> (* IPv6 without port gets here *)
            Ok (host, (Scheme.tcp_port scheme))
    in
    match find host hs with
    | None -> Fmt.error "Missing host header"
    | Some host -> find_hostname_port scheme host

  let for_connector headers body =
    let def_content_type c =
      let c = match c with
      | None -> Body.content_type body
      | Some c -> c
      in
      if c = Media_type.none then None else Some c
    in
    let def_content_length = function
    | Some "" -> None
    | Some _ as v -> v
    | None ->
        match Body.content_length body with
        | None -> None
        | Some l -> Some (Int.to_string l)
    in
    headers
    |> String_map.update content_type def_content_type
    |> String_map.update content_length def_content_length
end

module Cookie = struct
  type name = string
  type attributes =
    { domain : string option;
      http_only : bool;
      max_age : int option;
      path : Path.t;
      same_site : string;
      secure : bool; }

  let default_attributes =
      { domain = None; http_only = true; max_age = None; path = [];
        same_site = "strict"; secure = true; }

  let attributes
      ?init:(a = default_attributes) ?(domain = a.domain)
      ?(http_only = a.http_only) ?(max_age = a.max_age)
      ?(path = a.path) ?(same_site = a.same_site) ?(secure = a.secure) ()
    =
    { domain; http_only; max_age; path; same_site; secure }

  let encode_attributes a =
    let max_age = match a.max_age with
    | None -> "" | Some a -> ";max-age=" ^ string_of_int a
    in
    let domain = match a.domain with None -> "" | Some d -> ";domain=" ^ d in
    let path = if a.path = [] then "" else ";path=" ^ (Path.encode a.path) in
    let secure = if a.secure then ";Secure" else "" in
    let http_only = if a.http_only then ";httponly" else "" in
    let same_site = ";samesite=" ^ a.same_site in
    String.concat "" [max_age; domain; path; secure; http_only; same_site]

  let encode ?(attributes = default_attributes) ~name value =
    String.concat "" [name; "="; value; encode_attributes attributes]

  let decode_list s =
    (* Very lax parsing, pretty sure someone will complain at some point.
       https://www.rfc-editor.org/rfc/rfc6265#section-4.2 *)
    let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | c :: cs ->
        match String.index_opt c '=' with
        | None -> Fmt.error "%S: illegal cookie pair" c
        | Some i ->
            let n = string_subrange ~last:(i - 1) c in
            let v = string_subrange ~first:(i + 1) c in
            let v =
              if v = "" then "" else
              let len = String.length v - 1 in
              if v.[0] = '\"' && v.[len - 1] = '\"' && len > 1
              then string_subrange ~first:1 ~last:(len - 2) v else
              v
            in
            loop ((n, v) :: acc) cs
    in
    loop [] (Headers.values_of_string ~sep:';' s)
end

module Etag = struct
  type t = { weak : bool; tag : string }
  let make ~weak tag = { weak; tag }
  let is_weak s = s.weak
  let tag s = s.tag
  let weak_match e0 e1 = String.equal e0.tag e1.tag
  let strong_match e0 e1 =
    not e0.weak && not e1.weak && String.equal e0.tag e1.tag

  let is_etagc = function
  | '\x21' | '\x23' .. '\x7E' | '\x80' .. '\xFF' -> true | _ -> false

  let decode s =
    try
      let max = String.length s - 1 in
      if max < 1 then failwith err_etag else
      let weak = s.[0] = 'W' && s.[1] = '/' in
      let start = if weak then 2 else 0 in
      if not (s.[start] = '\"' && s.[max] = '\"') then failwith err_etag else
      let first = start + 1 and last = max - 1 in
      for i = first to last
      do if not (is_etagc s.[i]) then failwith err_etag done;
      Ok (make ~weak (string_subrange ~first ~last s))
    with
    | Failure e -> Error e

  let encode e = String.concat "\"" [if e.weak then "W/" else ""; e.tag; ""]

  (* Etag conditions *)

  type cond = [ `Any | `Etags of t list ]

  let decode_cond = function
  | "*" -> Ok `Any
  | s ->
      try
        let parse_etag s = match decode (trim_ows s) with
        | Ok s -> s | Error e -> failwith e
        in
        let etags = String.split_on_char ',' s in
        let etags = List.rev @@ List.rev_map parse_etag etags in
        Ok (`Etags etags)
      with
      | Failure e -> Error e

  let encode_cond = function
  | `Any -> "*" | `Etags etags -> String.concat ", " (List.map encode etags)

  let eval_if_match c t = match t with
  | None -> false
  | Some etag ->
      match c with
      | `Any -> true
      | `Etags etags -> List.exists (strong_match etag) etags

  let eval_if_none_match c t = match c with
  | `Any -> Option.is_none t
  |  `Etags etags ->
      match t with
      | None -> true
      | Some etag -> not (List.exists (weak_match etag) etags)

  let eval_if_range rt t = match t with
  | None -> false | Some etag -> strong_match rt etag
end

module Range = struct
  type bytes = [ `First of int | `Last of int | `Range of int * int ]
  let eval_bytes ~length:len b =
    let max = len - 1 in
    match b with
    | `First f -> if f > max then None else Some (f, max)
    | `Last n -> if n = 0 then None else Some (len - n, max)
    | `Range (f, l) -> if f > max then None else Some (f, min l max)

  type t = [ `Bytes of bytes list | `Other of string * string ]

  let decode_range s = match String.index_opt s '-' with
  | None -> failwith err_miss_dash
  | Some i ->
      let f = string_subrange ~last:(i - 1) s in
      let l = string_subrange ~first:(i + 1) s in
      match f with
      | "" -> `Last (Digits.decode l |> error_to_failure)
      | f ->
          let f = Digits.decode f |> error_to_failure in
          match l with
          | "" -> `First f
          | l ->
              let l = Digits.decode l |> error_to_failure in
              if l < f then failwith "invalid range" else
              `Range (f, l)

  let decode s = match String.index_opt s '=' with
  | None -> Error err_miss_eq
  | Some i ->
      let unit = string_subrange ~last:(i - 1) s in
      let v = string_subrange ~first:(i + 1) s in
      match unit with
      | "bytes" ->
          let rs = String.split_on_char ',' v in
          if rs = [] then Error "no range" else
          (try Ok (`Bytes (List.rev (List.rev_map decode_range rs)))
          with Failure e -> Error e)
      | s when is_token s -> Ok (`Other (s, v))
      | s -> Error (err_token s)

  let encode = function
  | `Other (u, v) -> String.concat "=" [u;v]
  | `Bytes rs ->
      let int = Digits.encode in
      let encode_bytes acc = function
      | `First f -> "-" :: int f :: acc
      | `Last n -> int n :: "-" :: acc
      | `Range (f, l) -> int l :: "-" :: int f :: acc
      in
      let rs = List.rev (List.fold_left encode_bytes [] rs) in
      String.concat "" ("bytes" :: "=" :: rs)
end

module Response = struct
  let get_reason status = function
  | Some reason -> reason | None -> Status.reason_phrase status

  type t =
    { body : Body.t;
      explain : string; (* For the server *)
      headers : Headers.t;
      reason : string;
      status : Status.t;
      version : Version.t; }

  let make
      ?(explain = "") ?(headers = Headers.empty) ?reason
      ?(version = Version.v11) status body
    =
    (* [body] is not optional to entice programs to use [empty] for clarity. *)
    let reason = get_reason status reason in
    { body; version; status; reason; headers; explain }

  let empty ?explain ?headers ?reason status =
    make ?explain ?headers ?reason status Body.empty

  let _make ?(body = Body.empty) ?explain ?headers ?reason status =
    make ?explain ?headers ?reason status body

  let with_body body response = { response with body }
  let with_explain explain response = { response with explain }
  let with_headers headers response = { response with headers }
  let override_headers ~by:headers response =
    { response with headers = Headers.override response.headers ~by:headers }

  let with_status ?explain ?reason status response =
    let explain = match explain with None -> response.explain | Some e -> e in
    let reason = get_reason status reason in
    { response with status; reason; explain }

  let is_empty response = Body.is_empty response.body

  let pp ppf response =
    Format.pp_open_vbox ppf 0;
    Fmt.field "version" Version.pp ppf response.version; Fmt.cut ppf ();
    Fmt.field "status" Status.pp ppf response.status; Fmt.cut ppf ();
    Fmt.field "reason" Fmt.qstring ppf response.reason; Fmt.cut ppf ();
    Fmt.field "explain" Fmt.qstring ppf response.explain; Fmt.cut ppf ();
    Headers.pp ppf response.headers;
    if not (Headers.is_empty response.headers) then Fmt.cut ppf ();
    Fmt.field "body" Body.pp ppf response.body;
    Format.pp_close_box ppf ()

  (* Properties *)

  let version response = response.version
  let status response = response.status
  let reason response = response.reason
  let headers response = response.headers
  let body response = response.body
  let explain response = response.explain

  (* Responding *)

  let content ?explain ?reason ?headers ?content_type status s =
    let body = Body.of_string ?content_type s in
    make ?explain ?reason ?headers status body

  let text ?explain ?reason ?headers status s =
    let content_type = Media_type.text_plain in
    content ?explain ?reason ?headers ~content_type status s

  let html ?explain ?reason ?headers status s =
    let content_type = Media_type.text_html in
    content ?explain ?reason ?headers ~content_type status s

  let json ?explain ?reason ?headers status s =
    let content_type = Media_type.application_json in
    content ?explain ?reason ?headers ~content_type status s

  (* Redirect *)

  let redirect
      ?body ?explain ?(headers = Headers.empty) ?reason status loc
    =
    let headers = Headers.(def location loc) headers in
    _make ?body ?explain ~headers ?reason status

  let bad_request_400 ?body ?explain ?headers ?reason () =
    Error (_make ?body ?explain ?headers ?reason Status.bad_request_400)

  let unauthorized_401 ?body ?explain ?headers ?reason () =
    Error (_make ?body ?explain ?headers ?reason Status.unauthorized_401)

  let forbidden_403 ?body ?explain ?headers ?reason () =
    Error (_make ?body ?explain ?headers ?reason Status.forbidden_403)

  let not_found_404 ?body ?explain ?headers ?reason () =
    Error (_make ?body ?explain ?headers ?reason Status.not_found_404)

  let method_not_allowed_405
      ?body ?explain ?(headers = Headers.empty) ?reason ~allowed ()
    =
    let ms = String.concat ", " (List.map Method.encode allowed) in
    let headers = Headers.(def allow ms) headers in
    let status = Status.method_not_allowed_405 in
    Error (_make ?body ?explain ~headers ?reason status)

  let gone_410 ?body ?explain ?headers ?reason () =
    Error (_make ?body ?explain ?headers ?reason Status.gone_410)

  (* Server errors *)

  let server_error_500 ?body ?explain ?headers ?reason () =
    Error (_make ?body ?explain ?headers ?reason Status.server_error_500)

  let not_implemented_501 ?body ?explain ?headers ?reason () =
    Error (_make ?body ?explain ?headers ?reason Status.not_implemented_501)

  let service_unavailable_503 ?body ?explain ?headers ?reason () =
    Error (_make ?body ?explain ?headers ?reason Status.service_unavailable_503)

  (* Error handling *)

  let result = function Ok v | Error v -> v
  let map_errors ~only_empty f response =
    let status = status response in
    if 400 <= status && status <= 599 && (not only_empty || is_empty response)
    then f response else response
end

module Request = struct
  type t =
    { body : Body.t;
      headers : Headers.t;
      method' : Method.t;
      path : Path.t;
      query : string option;
      raw_path : string;
      service_path : Path.t;
      version : Version.t; }

  let make
      ?(headers = Headers.empty) ?(path = Path.root) ?(query = None)
      ?(service_path = Path.root) ~version method' ~raw_path body
    =
    { body; headers; method'; path; query; raw_path; service_path; version }

  let for_service_connector
      ~service_path ~version method' ~raw_path ~headers body =
    let path, query =
      match Path.and_query_string_of_request_target raw_path with
      | Ok v -> v | Error e -> failwith e
    in
    let service_path, path =
      if path = [] (* "*" request line FIXME not sure it's a good idea,
                          maybe we coud fail *)
      then [], [] else
      match Path.strip_prefix ~prefix:service_path path with
      | [] -> failwith "Cannot strip service path from requested URI"
      | path -> service_path, path
    in
    Result.ok @@
    make ~headers ~path ~query ~service_path ~version method' ~raw_path body

  let of_url
      ?(body = Body.empty) ?(headers = Headers.empty) ?(version = Version.v11)
      method' ~url
    =
    try
      let find_host_path ~target = match String.index_opt target '/' with
      | None -> target, "/"
      | Some i ->
          let host = String.sub target 0 i in
          let raw_path = String.sub target i (String.length target - i) in
          if host = "" then Fmt.failwith "URL '%s': no host found" url else
          host, raw_path
      in
      let scheme, target = Scheme.split ~url in
      let host, raw_path = find_host_path ~target in
      let headers = Headers.(def host) host Headers.empty in
      let service_path = Path.root in
      let path, query =
        match Path.and_query_string_of_request_target raw_path with
        | Ok v -> v | Error e -> Fmt.failwith "URL '%s': %s" url e
      in
      let request =
        make ~version ~headers ~service_path ~path ~query method' ~raw_path body
      in
      Ok (scheme, request)
    with
    | Failure e -> Error e

  let pp_query ppf = function
  | None -> Fmt.pf ppf "<none>" | Some q -> Fmt.pf ppf "%S" q

  let pp ppf request =
    Format.pp_open_vbox ppf 0;
    Fmt.field "method" Method.pp ppf request.method'; Fmt.cut ppf ();
    Fmt.field "path" Path.pp_dump ppf request.path; Fmt.cut ppf ();
    Fmt.field "query" pp_query ppf request.query; Fmt.cut ppf ();
    Fmt.field "version" Version.pp ppf request.version; Fmt.cut ppf ();
    Fmt.field "raw-path" Fmt.qstring ppf request.raw_path; Fmt.cut ppf ();
    Fmt.field "service-path" Path.pp_dump ppf request.service_path;
    Fmt.cut ppf ();
    Headers.pp ppf request.headers;
    if not (Headers.is_empty request.headers) then Fmt.cut ppf ();
    Fmt.field "body" Body.pp ppf request.body;
    Format.pp_close_box ppf ()

  (* Properties *)

  let body request = request.body
  let headers request = request.headers
  let method' request = request.method'
  let path request = request.path
  let query request = request.query
  let raw_path request = request.raw_path
  let service_path request = request.service_path
  let version request = request.version

  (* Echo *)

  let echo ?(status = Status.not_found_404) request =
    let body = Body.to_string (body request) in
    let body = match body with
    | Error e -> Printf.sprintf "<Error: %s>" e
    | Ok body -> body
    in
    let body = Format.asprintf "@[<v>%a@,%s@]" pp request body in
    Response.text status body

  (* Deconstructing and responding *)

  let redirect_to_path ?body ?explain ?headers ?reason request status path =
    let loc = Path.encode (Path.concat (service_path request) path) in
    Response.redirect ?body ?explain ?headers ?reason status loc

  let decode_header name dec request =
    match Headers.find name (headers request) with
    | None -> Ok None
    | Some v ->
        match dec v with
        | Ok v -> Ok (Some v)
        | Error e ->
            let reason = Fmt.str "%s: %s" (name :> string) e in
            Response.bad_request_400 ~reason ()

  let allow allowed r = match Method.constrain ~allowed (method' r) with
  | Ok _ as v -> v
  | Error allow ->
      Response.method_not_allowed_405 ~allowed:(List.map fst allow) ()


  let find_cookie ~name request =
    let cs = match Headers.(find cookie) (headers request) with
    | None -> Ok []
    | Some s -> Cookie.decode_list  s
    in
    match cs with Error _ as e -> e | Ok cs -> Ok (List.assoc_opt name cs)

  let to_query request =
    let url_query request = match query request with
    | None -> Ok Query.empty | Some q -> Ok (Query.decode q)
    in
    let body_query request =
      match Headers.(find ~lowervalue:true content_type (headers request)) with
      | None -> Response.bad_request_400 ~reason:"missing content type" ()
      | Some t ->
          match Media_type.get_type t with
          | t when t = Media_type.application_x_www_form_urlencoded ->
              begin match Body.to_string (body request) with
              | Error reason -> Response.server_error_500 ~reason ()
              | Ok body -> Ok (Query.decode body)
              end
          | t ->
              let explain = t in
              Error (Response.empty Status.unsupported_media_type_415 ~explain)
    in
    match method' request with
    | `GET | `HEAD -> url_query request
    | _ -> body_query request

  let clean_path request =
    let not_empty s = not (String.equal s "") in
    match path request with
    | [] | [""] -> Ok ()
    | p when List.for_all not_empty p -> Ok ()
    | p ->
        let p = match (List.filter not_empty p) with [] -> [""] | p -> p in
        let loc = Path.(encode @@ concat (service_path request) p) in
        let explain = "path cleaning" in
        Error (Response.redirect ~explain Status.moved_permanently_301 loc)

  let to_absolute_filepath ?(strip = [""]) ~file_root request =
    match Path.strip_prefix ~prefix:strip (path request) with
    | [] ->
        let explain = Fmt.str "could not strip prefix %a" Path.pp strip in
        Response.not_found_404 ~explain ()
    | p ->
        match Path.to_absolute_filepath p with
        | Error e -> Response.bad_request_400 ~explain:e ()
        | Ok filepath -> Ok (Path.prefix_filepath ~prefix:file_root filepath)

  let eval_if_none_match request etag ~headers:hs =
    let headers = Headers.(def etag) (Etag.encode etag) hs in
    match decode_header Headers.if_none_match Etag.decode_cond request with
    | Error _ as e -> e
    | Ok None -> Ok headers
    | Ok (Some cond) ->
        if Etag.eval_if_none_match cond (Some etag)
        then Ok headers
        else Error (Response.empty ~headers Status.not_modified_304)
end

module Connector = struct
  module Log = struct
    type dur_ns = int64
    type msg =
    [ `Service_exn of exn * Stdlib.Printexc.raw_backtrace
    | `Connector_exn of exn * Stdlib.Printexc.raw_backtrace
    | `Connection_reset
    | `Trace of dur_ns * Request.t option * Response.t option ]

    let pp_service_exn ppf e = Fmt.exn_backtrace ~kind:"service" ppf e
    let pp_connector_exn ppf e = Fmt.exn_backtrace ~kind:"connector" ppf e
    let pp_connection_reset ppf () = Fmt.pf ppf "Connection reset by peer."
    let pp_trace ppf dur_ns request response =
      let strf = Printf.sprintf in
      let dur =
        if Int64.(equal zero dur_ns) then "" else
        match Int64.compare dur_ns 1_000_000L (* < 1ms *) with
        | -1 -> strf " %3Luµs" (Int64.unsigned_div dur_ns 1_000L)
        | _ -> strf " %3Lums" (Int64.unsigned_div dur_ns 1_000_000L)
      in
      let method' req = match Request.method' req with
      | `POST | `PUT | `DELETE | `PATCH as m ->
          strf "\x1B[34m%s\x1B[0m" (Method.encode m)
      | m ->  Method.encode m
      in
      let query req = match Request.query req with
      | None -> ""
      | Some q -> strf "?%s" q
      in
      let path req =
        strf "\x1B[1m%s\x1B[0m%s" (Path.encode (Request.path req)) (query req)
      in
      let status resp = match Response.status resp with
      | st when st <= 299 -> strf "\x1B[32m%d\x1B[0m" st
      | st when st <= 399 -> strf "\x1B[93m%d\x1B[0m" st
      | st when 400 <= st && st <= 599 -> strf "\x1B[31m%d\x1B[0m" st
      | st -> string_of_int st
      in
      let data = match request, response with
      | Some request, Some response ->
          String.concat "" @@
          method' request :: " [" :: status response :: dur :: "] " ::
        path request :: " (" :: Response.reason response :: ")" ::
          (if Response.explain response = "" then [] else
           [ " "; Response.explain response])
      | Some req, None ->
          String.concat "" @@
          method' req :: " [" :: dur :: "] " ::
          path req :: " No response" :: []
      | None, Some resp ->
          String.concat "" @@
          "???" :: " [" :: status resp :: dur :: "] " ::
          "Can't decode request" :: " (" :: Response.reason resp :: ")" ::
          (if Response.explain resp = "" then [] else
           [ " "; Response.explain resp])
      | None, None -> "trace really ?"
      in
      Fmt.string ppf data

    let pp_msg ppf = function
    | `Trace (dur, request, response) -> pp_trace ppf dur request response
    | `Service_exn e -> pp_service_exn ppf e
    | `Connector_exn e -> pp_connector_exn ppf e
    | `Connection_reset -> pp_connection_reset ppf ()

    let quiet _ = ()
    let default ?(ppf = Format.err_formatter) ~trace () = function
    | `Trace _ when not trace -> ()
    | `Trace (dur, req, resp) -> pp_trace ppf dur req resp; Fmt.nl ppf ()
    | `Service_exn e -> pp_service_exn ppf e; Fmt.nl ppf ()
    | `Connector_exn e -> pp_connector_exn ppf e; Fmt.nl ppf ()
    | `Connection_reset -> pp_connection_reset ppf (); Fmt.nl ppf ()
  end
  module Default = struct
    let max_request_headers_byte_size = 64 * 1024
    let max_request_body_byte_size = 10 * 1024 * 1024
  end
end

module Http = struct
  module Base64 = Base64
  module Pct = Pct
  module Digits = Digits
  module Version = Version
  module Method = Method
  module Path = Path
  module Query = Query
  module Scheme = Scheme
  module Status = Status
  module Body = Body

  module Headers = Headers
  module Cookie = Cookie
  module Etag = Etag
  module Range = Range

  module Response = Response
  module Request = Request

  module Connector = Connector

  module Private = struct
    let string_subrange = string_subrange
    let string_lowercase = string_lowercase
    let trim_ows = trim_ows

    let decode_request_line b ~first ~crlf =
      (* https://www.rfc-editor.org/rfc/rfc9112#name-request-line *)
      let decode_request_target b ~first ~max =
        let rec loop b i =
          if i <= max && Bytes.get b i <> ' ' then loop b (i + 1) else i - 1
        in
        match loop b first with
        | last when last < first -> failwith err_empty_string
        | last -> last + 1, Bytes.sub_string b first (last - first + 1)
      in
      let first, meth = Method.decode' b ~first ~max:crlf in
      let first = decode_sp b ~first ~max:crlf in
      let first, target = decode_request_target b ~first ~max:crlf in
      let first = decode_sp b ~first ~max:crlf in
      let first, version = Version.decode_of_bytes b ~first ~max:crlf in
      if first = crlf then meth, target, version else failwith err_rl_garbage

    let decode_status_line b ~first ~crlf =
      (* HTTP https://www.rfc-editor.org/rfc/rfc9112#name-status-line *)
      let first, version = Version.decode_of_bytes b ~first ~max:crlf in
      let first = decode_sp b ~first ~max:crlf in
      let first, status = Status.decode_of_bytes b ~first ~max:crlf in
      let first = decode_sp b ~first ~max:crlf in
      let reason = Bytes.sub_string b first (crlf - first) in
      version, status, reason

    let decode_header_field = decode_header_field
    let decode_headers buf ~crlfs =
      let rec loop acc buf last_crlf = function
      | [] -> acc
      | crlf :: crlfs ->
          let first = last_crlf + 2 in
          let name, value = decode_header_field buf ~first ~crlf in
          let acc =
            (* This looks ok according to RFC 7230 3.2.2 *)
            if Headers.Name.equal name Headers.set_cookie
            then Headers.add_set_cookie value acc
            else Headers.add_value name value acc
          in
          loop acc buf crlf crlfs
      in
      loop Headers.empty buf (List.hd crlfs) (List.tl crlfs)

    let encode_header n v acc =
      let encode n acc v = n :: ": " :: v :: crlf :: acc in
      if not (String.equal n Headers.set_cookie) then encode n acc v else
      let vs = Headers.values_of_set_cookie_value v in
      List.fold_left (encode Headers.set_cookie) acc vs

    let encode_http11_response_head status ~reason hs =
      let status = string_of_int status in
      let hs = Headers.fold encode_header hs [crlf] in
      String.concat "" ("HTTP/1.1 " :: status :: " " :: reason :: crlf :: hs)

    let encode_http11_request_head method' ~request_target:trgt hs =
      let method' = Method.encode method' in
      let hs = Headers.fold encode_header hs [crlf] in
      String.concat "" (method' :: " " :: trgt :: " HTTP/1.1" :: crlf :: hs)
  end
end
