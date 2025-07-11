(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Url = Webs__url

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
  let field f pp_v ppf v = pf ppf "@[<h>%s: %a@]" f pp_v v
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
let err_header_miss_name = "missing header name"
let err_header_undefined n = Fmt.str "header %s undefined" n
let err_header_miss_delim = "missing ':' delimiter in header"
let err_header_value_char c = Fmt.str "%C not a header value character" c
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
      if name = "" then failwith err_header_miss_name else
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
  type of_file_ext_map = t String_map.t
  type to_file_ext_map = file_ext String_map.t

  let base_exts =
    (* Note the order matters. When the same media type appears more than
       once, the extension of the last occurence is used to represent it. *)
    [ ".aac",  "audio/aac";
      ".avi",  "video/x-msvideo";
      ".bin",  "application/octet-stream";
      ".bmp",  "image/bmp";
      ".bz",   "application/x-bzip";
      ".bz2",  "application/x-bzip2";
      ".css",  "text/css";
      ".gz",   "application/gzip";
      ".gif",  "image/gif";
      ".htm",  "text/html";
      ".html", "text/html";
      ".ics",  "text/calendar";
      ".jpg",  "image/jpeg";
      ".jpeg", "image/jpeg";
      ".js",   "text/javascript";
      ".json", "application/json";
      ".jsonldx", "application/ld+json";
      ".md",   "text/markdown;charset=utf-8";
      ".midi", "audio/midi";
      ".midi", "audio/x-midi";
      ".mjs",  "text/javascript";
      ".mp3",  "audio/mpeg";
      ".mpeg", "video/mpeg";
      ".oga",  "audio/ogg";
      ".ogv",  "video/ogg";
      ".ogx",  "application/ogg";
      ".opus", "audio/opus";
      ".otf",  "font/otf";
      ".png",  "image/png";
      ".pdf",  "application/pdf";
      ".rar",  "application/vnd.rar";
      ".rtf",  "application/rtf";
      ".svg",  "image/svg+xml";
      ".tar",  "application/x-tar";
      ".tif",  "image/tiff";
      ".tiff", "image/tiff";
      ".ts",   "video/mp2t";
      ".ttf",  "font/ttf";
      ".txt",  "text/plain;charset=utf-8";
      ".wav",  "audio/wav";
      ".weba", "audio/webm";
      ".webm", "video/webm";
      ".webp", "image/webp";
      ".woff", "font/woff";
      ".woff2","font/woff2";
      ".xhtml","application/xhtml+xml";
      ".xml",  "application/xml";
      ".zip",  "application/zip";
      ".zst",  "application/zstd";
      ".7z",   "application/x-7z-compressed"; ]

  let add_file_ext (of_ext_map, to_ext_map) (ext, t) =
    String_map.add ext t of_ext_map, String_map.add t ext to_ext_map

  let default_of_file_ext_map, default_to_file_ext_map =
    List.fold_left add_file_ext (String_map.empty, String_map.empty) base_exts

  let of_file_ext ?(map = default_of_file_ext_map) ext =
    let default = application_octet_stream in
    Option.value (String_map.find_opt ext map) ~default

  let of_filepath ?map file = of_file_ext ?map (Fpath.get_ext file)

  let to_file_ext ?(map = default_to_file_ext_map) t =
    match String_map.find_opt t map with
    | Some ext -> ext
    | None ->
        match String_map.find_opt (get_type t) map with
        | Some ext -> ext
        | None -> ".bin"
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
    let len = max - first + 1 in
    if len < 6 then failwith err_version else
    let[@inline] c b i = Bytes.get b (first + i) in
    if c b 0 = 'H' && c b 1 = 'T' && c b 2 = 'T' && c b 3 = 'P' &&
       c b 4 = '/' && is_digit (c b 5)
    then begin
      if len = 6 then (first + 6, (digit_to_int (c b 5), 0)) else
      let sep = c b 6 in
      if sep = ' ' then (first + 6, (digit_to_int (c b 5), 0)) else
      if len >= 8 && sep = '.' && is_digit (c b 7)
      then first + 8, (digit_to_int (c b 5), digit_to_int (c b 7))
      else failwith err_version
    end else failwith err_version

  let decode s =
    let len = String.length s in
    if not (len = 8 || len = 6) then Error err_version else
    match decode_of_bytes (Bytes.unsafe_of_string s) ~first:0 ~max:(len - 1)with
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
    Buffer.clear b;
    Url.Percent.decode_to_buffer b ~first ~last s; Buffer.contents b

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
        | c when c = '%' ||
                 Url.Percent.is_char_verbatim_in_uri_component c ->
            loop acc b s ~first (i + 1)
        | c -> Error (err_path_char c)
    in
    loop [] (Buffer.create 255) s ~first:1 1

  let buffer_encode_path b segs =
    let add_seg seg =
      Buffer.add_char b '/';
      Url.Percent.encode_to_buffer
        Url.Percent.is_char_verbatim_in_uri_component b seg
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
        begin match Url.Percent.is_hexdig hi &&
                    Url.Percent.is_hexdig lo with
        | false -> Buffer.add_char b '%'; incr i
        | true ->
            let c = (Url.Percent.hexdig_to_int hi lsl 4) lor
                    (Url.Percent.hexdig_to_int lo) in
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
        Bytes.set b !out (Url.Percent.unsafe_hexdig_of_int hi); incr out;
        Bytes.set b !out (Url.Percent.unsafe_hexdig_of_int lo); incr out;
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
  open Bytesrw

  (* Byte readers and writers *)

  type byte_writer = (bytes * int * int) option -> unit

  let string_to_byte_writer s = fun write ->
    (* N.B. unsafe is ok: the consumer is not supposed to mutate the bytes. *)
    write (Some (Bytes.unsafe_of_string s, 0, String.length s));
    write None

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
  | Bytes_reader of Bytes.Reader.t
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

  let of_bytes_reader ?content_length ?content_type r =
    make ?content_length ?content_type (Bytes_reader r)

  let of_string ?content_type s =
    let content_length = String.length s in
    make ~content_length ?content_type (Byte_writer (string_to_byte_writer s))

  let to_bytes_reader b = match b.content with
  | Empty -> Ok (Bytes.Reader.empty ())
  | Bytes_reader r -> Ok r
  | Byte_writer w -> Ok (Bytes.Reader.of_string (byte_writer_to_string w))
  | Custom _ -> Error "Don't know how to read custom body content"

  let to_string b = match b.content with
  | Empty -> Ok ""
  | Bytes_reader r -> Ok (Bytes.Reader.to_string r)
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
    | Bytes_reader _ -> Fmt.string ppf "<Bytes.Reader.t"
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

  let encode = function `Http -> "http" | `Https -> "https"
  let pp ppf s = Fmt.string ppf (encode s)
  let tcp_port = function `Http -> 80 | `Https -> 443
  let split ~url =
    let https = "https://" and http = "http://" in
    if String.starts_with ~prefix:https url
    then (`Https, string_chop_known_prefix ~prefix:https url) else
    if String.starts_with ~prefix:http url
    then (`Http, string_chop_known_prefix ~prefix:http url)
    else Fmt.failwith "Not an HTTP URL"
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
  let content_disposition = "content-disposition"
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
  let origin = "origin"
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

  let find' ?lowervalue n hs = match find ?lowervalue n hs with
  | None -> Fmt.error "%a: No such header" Name.pp n
  | Some v -> Ok v

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

  let decode_http11_header s =
    let crlf = String.length s in
    match decode_header_field (Bytes.unsafe_of_string s) ~first:0 ~crlf with
    | exception Failure e -> Error e | n, v -> Ok (Name.v n, v)

  let pp_header ppf (n, p) = Format.fprintf ppf "%s: %s" n p

  (* Encoding *)

  let pp ppf hs =
    let pp_header ppf (n, v)  =
      if not (n = "set-cookie") then Fmt.field n Fmt.qstring ppf v else
      let cs = values_of_set_cookie_value v in
      List.iter (Fmt.field "set-cookie" Fmt.qstring ppf) cs
    in
    Fmt.list pp_header ppf (String_map.bindings hs)

  let encode_http11_header n v acc =
    let encode n acc v = n :: ": " :: v :: crlf :: acc in
    if not (String.equal n set_cookie) then encode n acc v else
    let vs = values_of_set_cookie_value v in
    List.fold_left (encode set_cookie) acc vs

  let encode_http11 hs = String.concat "" (fold encode_http11_header hs [])



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

  let decode_host scheme hs =
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
    match find' host hs with
    | Error _ as e -> e
    | Ok  host -> find_hostname_port scheme host

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
      headers : Headers.t;
      log : string; (* For the server *)
      reason : string;
      status : Status.t;
      version : Version.t; }

  let make
      ?(headers = Headers.empty) ?(log = "") ?reason
      ?(version = Version.v11) status body
    =
    (* [body] is not optional to entice programs to use [empty] for clarity. *)
    let reason = get_reason status reason in
    { body; headers; log; reason; status; version }

  let empty ?headers ?log ?reason status =
    make ?log ?headers ?reason status Body.empty

  let _make ?(body = Body.empty) ?headers ?log ?reason status =
    make ?log ?headers ?reason status body

  let with_body body response = { response with body }
  let with_log log response = { response with log }
  let with_headers headers response = { response with headers }
  let override_headers ~by:headers response =
    { response with headers = Headers.override response.headers ~by:headers }

  let with_status ?log ?reason status response =
    let log = match log with None -> response.log | Some e -> e in
    let reason = get_reason status reason in
    { response with log; reason; status; }

  let is_empty response = Body.is_empty response.body

  let pp ppf response =
    Format.pp_open_vbox ppf 0;
    Fmt.field "version" Version.pp ppf response.version; Fmt.cut ppf ();
    Fmt.field "status" Status.pp ppf response.status; Fmt.cut ppf ();
    Fmt.field "reason" Fmt.qstring ppf response.reason; Fmt.cut ppf ();
    Fmt.field "log" Fmt.qstring ppf response.log; Fmt.cut ppf ();
    Headers.pp ppf response.headers;
    if not (Headers.is_empty response.headers) then Fmt.cut ppf ();
    Fmt.field "body" Body.pp ppf response.body;
    Format.pp_close_box ppf ()

  let encode_http11 ~include_body response =
    let ( let* ) = Result.bind in
    let version = Version.encode response.version in
    let status = string_of_int response.status in
    let reason = response.reason in
    let headers = response.headers in
    let headers = Headers.encode_http11 headers in
    let* body = if include_body then Body.to_string response.body else Ok "" in
    let msg =
      version :: " " :: status :: " " :: reason :: "\r\n" ::
      headers :: "\r\n" :: [body]
    in
    Ok (String.concat "" msg)

  (* Properties *)

  let version response = response.version
  let status response = response.status
  let reason response = response.reason
  let headers response = response.headers
  let body response = response.body
  let log response = response.log

  (* Responding *)

  let content ?content_type ?headers ?log ?reason status s =
    let body = Body.of_string ?content_type s in
    make ?headers ?log ?reason status body

  let text ?headers ?log ?reason status s =
    let content_type = Media_type.text_plain in
    content ~content_type ?headers ?log ?reason status s

  let html ?headers ?log ?reason status s =
    let content_type = Media_type.text_html in
    content ~content_type ?headers ?log ?reason status s

  let json ?headers ?log ?reason status s =
    let content_type = Media_type.application_json in
    content ~content_type ?headers ?log ?reason status s

  (* Redirect *)

  let redirect
      ?body ?(headers = Headers.empty) ?log ?reason status loc
    =
    let headers = Headers.(def location loc) headers in
    _make ?body ~headers ?log ?reason status

  let bad_request_400 ?body ?headers ?log ?reason () =
    Error (_make ?body ?headers ?log ?reason Status.bad_request_400)

  let unauthorized_401 ?body ?headers ?log ?reason () =
    Error (_make ?body ?headers ?log ?reason Status.unauthorized_401)

  let forbidden_403 ?body ?headers ?log ?reason () =
    Error (_make ?body ?headers ?log ?reason Status.forbidden_403)

  let not_found_404 ?body ?headers ?log ?reason () =
    Error (_make ?body ?headers ?log ?reason Status.not_found_404)

  let method_not_allowed_405
      ?body ?(headers = Headers.empty) ?log ?reason ~allowed ()
    =
    let ms = String.concat ", " (List.map Method.encode allowed) in
    let headers = Headers.(def allow ms) headers in
    let status = Status.method_not_allowed_405 in
    Error (_make ?body ~headers ?log ?reason status)

  let gone_410 ?body ?headers ?log ?reason () =
    Error (_make ?body ?headers ?log ?reason Status.gone_410)

  (* Server errors *)

  let server_error_500 ?body ?headers ?log ?reason () =
    Error (_make ?body ?headers ?log ?reason Status.server_error_500)

  let not_implemented_501 ?body ?headers ?log ?reason () =
    Error (_make ?body ?headers ?log ?reason Status.not_implemented_501)

  let service_unavailable_503 ?body ?headers ?log ?reason () =
    Error (_make ?body ?headers ?log ?reason Status.service_unavailable_503)

  let todo = not_implemented_501

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
      log : string;
      method' : Method.t;
      path : Path.t;
      query : string option;
      raw_path : string;
      scheme : Scheme.t;
      service_path : Path.t;
      version : Version.t; }

  let make
      ?(headers = Headers.empty) ?(log = "") ?(path = Path.root) ?(query = None)
      ?(scheme = `Https) ?(service_path = Path.root) ~version method'
      ~raw_path body
    =
    { body; headers; log; method'; path; query; raw_path; scheme; service_path;
      version }

  let for_service_connector
      ?log ?scheme ~service_path ~version method' ~raw_path ~headers body =
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
    make ?log ?scheme ~headers ~path ~query ~service_path ~version method'
      ~raw_path body

  let of_url
      ?(body = Body.empty) ?(headers = Headers.empty) ?log
      ?(version = Version.v11)
      method' ~url
    =
    try
      (* TODO use Webs_url ? *)
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
      let headers = Headers.(def host) host headers in
      let service_path = Path.root in
      let path, query =
        match Path.and_query_string_of_request_target raw_path with
        | Ok v -> v | Error e -> Fmt.failwith "URL '%s': %s" url e
      in
      let request =
        make ~headers ?log ~scheme ~service_path ~path ~query method'
          ~raw_path ~version body
      in
      Ok request
    with
    | Failure e -> Error e

  let _to_url ~host request =
    let scheme = Scheme.encode request.scheme in
    String.concat "" [scheme; "://"; host; request.raw_path]

  let to_url request = match Headers.(find' host) request.headers with
  | Error _ as e -> e
  | Ok host -> Ok (_to_url ~host request)

  let to_url' request =
    _to_url ~host:(Headers.(get host) request.headers) request

  let with_body body request = { request with body }
  let with_headers headers request = { request with headers }
  let override_headers ~by:headers request =
    { request with headers = Headers.override request.headers ~by:headers }

  let pp_query ppf = function
  | None -> Fmt.pf ppf "<none>" | Some q -> Fmt.pf ppf "%S" q

  let pp ppf request =
    Format.pp_open_vbox ppf 0;
    Fmt.field "method" Method.pp ppf request.method'; Fmt.cut ppf ();
    Fmt.field "path" Path.pp_dump ppf request.path; Fmt.cut ppf ();
    Fmt.field "query" pp_query ppf request.query; Fmt.cut ppf ();
    Fmt.field "version" Version.pp ppf request.version; Fmt.cut ppf ();
    Fmt.field "raw-path" Fmt.qstring ppf request.raw_path; Fmt.cut ppf ();
    Fmt.field "scheme" Scheme.pp ppf request.scheme; Fmt.cut ppf ();
    Fmt.field "service-path" Path.pp_dump ppf request.service_path;
    Fmt.cut ppf ();
    Headers.pp ppf request.headers;
    if not (Headers.is_empty request.headers) then Fmt.cut ppf ();
    Fmt.field "body" Body.pp ppf request.body;
    Format.pp_close_box ppf ()

  (* Properties *)

  let body request = request.body
  let headers request = request.headers
  let log request = request.log
  let method' request = request.method'
  let path request = request.path
  let query request = request.query
  let raw_path request = request.raw_path
  let scheme request = request.scheme
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

  let redirect_to_path ?body ?headers ?log ?reason request status path =
    let loc = Path.encode (Path.concat (service_path request) path) in
    Response.redirect ?body ?headers ?log ?reason status loc

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
              let log = t in
              Error (Response.empty Status.unsupported_media_type_415 ~log)
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
        let log = "path cleaning" in
        Error (Response.redirect ~log Status.moved_permanently_301 loc)

  let to_absolute_filepath ?(strip = [""]) ~file_root request =
    match Path.strip_prefix ~prefix:strip (path request) with
    | [] ->
        let log = Fmt.str "could not strip prefix %a" Path.pp strip in
        Response.not_found_404 ~log ()
    | p ->
        match Path.to_absolute_filepath p with
        | Error e -> Response.bad_request_400 ~log:e ()
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
          (if Response.log response = "" then [] else
           [ " "; Response.log response])
      | Some req, None ->
          String.concat "" @@
          method' req :: " [" :: dur :: "] " ::
          path req :: " No response" :: []
      | None, Some resp ->
          String.concat "" @@
          "???" :: " [" :: status resp :: dur :: "] " ::
          "Can't decode request" :: " (" :: Response.reason resp :: ")" ::
          (if Response.log resp = "" then [] else
           [ " "; Response.log resp])
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

    let find_crlf b ~first =
      let max_min_1 = Bytes.length b - 2 in
      let i = ref first in
      try
        while !i <= max_min_1 do
          if Bytes.get b !i = '\r' && Bytes.get b (!i + 1) = '\n'
          then raise_notrace Exit else incr i
        done;
        None
      with
      | Exit -> Some !i

    let decode_http11_response b ~first = match find_crlf b ~first with
    | None -> failwith "No status line found"
    | Some crlf ->
        let version, status, reason = decode_status_line b ~first ~crlf in
        let rec loop acc ctype b ~first = match find_crlf b ~first with
        | None -> failwith "End of headers not found"
        | Some crlf when crlf = first ->
            let body_start = crlf + 2 in
            let body_len = Bytes.length b - body_start in
            let body = Bytes.sub_string b body_start body_len in
            let body = Body.of_string ?content_type:ctype body in
            Response.make ~headers:acc ~version status body
        | Some crlf ->
            let name, value = decode_header_field b ~first ~crlf in
            let first = crlf + 2 in
            if Headers.Name.equal name Headers.content_type
            then loop acc (Some value) b ~first else
            let acc =
              (* This looks ok according to RFC 7230 3.2.2 *)
              if Headers.Name.equal name Headers.set_cookie
              then Headers.add_set_cookie value acc
              else Headers.add_value name value acc
            in
            loop acc ctype b ~first
        in
        loop Headers.empty None b ~first:(crlf + 2)

    let encode_http11_response_head status ~reason hs =
      let status = string_of_int status in
      let hs = Headers.fold Headers.encode_http11_header hs [crlf] in
      String.concat "" ("HTTP/1.1 " :: status :: " " :: reason :: crlf :: hs)

    let encode_http11_request_head method' ~request_target:trgt hs =
      let method' = Method.encode method' in
      let hs = Headers.fold Headers.encode_http11_header hs [crlf] in
      String.concat "" (method' :: " " :: trgt :: " HTTP/1.1" :: crlf :: hs)
  end
end

module Http = struct
  module Digits = Digits
  module Version = Version
  module Method = Method
  module Path = Path
  module Query = Query
  module Scheme = Scheme
  module Status = Status
  module Body = Body

  (* Headers *)

  module Headers = Headers
  module Cookie = Cookie
  module Etag = Etag
  module Range = Range

  (* Reponses and requests *)

  module Response = Response
  module Request = Request

  (* Connector tools. *)

  module Connector = Connector
end

module Http_client = struct
  let ( let* ) = Result.bind

  module type T = sig
    type t
    val id : t -> string
    val request : t -> Http.Request.t -> (Http.Response.t, string) result
  end

  type t = V : (module T with type t = 'a) * 'a -> t
  let make m c = V (m, c)

  let id (V ((module C), c)) = C.id c

  (* TODO we could likely expose some of the redirection logic here. *)

  let x_follow_location = Http.Headers.Name.v "x-follow-location"

  let find_rel_location ~loc rel request response =
    let scheme = Http.Scheme.encode (Http.Request.scheme request) in
    let* host = Http.Headers.(find' host) (Http.Request.headers request) in
    try match rel with
    | `Absolute_path -> Ok (String.concat "" [scheme; "://"; host; loc])
    | `Relative_path ->
        let path = Http.Request.raw_path request in
        begin match String.rindex_opt path '/' with
        | None -> Ok (String.concat "" [scheme; "://"; host; "/"; loc])
        | Some i ->
            let path = String.sub path 0 i in
            Ok (String.concat "" [scheme; "://"; host; path; "/"; loc ])
        end
        | `Empty | `Scheme -> raise Exit
    with
    | Exit ->
        let retract = Http.Response.result in
        Fmt.error "Could not construct redirect from %s to %s"
          (Http.Request.to_url request |> retract)
          loc

  let find_location request response =
    let* loc = Http.Headers.(find' location) (Http.Response.headers response) in
    match Url.kind loc with
    | `Absolute -> Ok loc
    | `Relative rel -> find_rel_location ~loc rel request response

  let request_host request =
    let scheme = Http.Request.scheme request in
    Http.Headers.decode_host scheme (Http.Request.headers request)

  let unconditional_redirection_drops headers =
    headers
    |> Http.Headers.(undef referer)
    |> Http.Headers.(undef origin)
    |> Http.Headers.(undef connection)
    |> Http.Headers.(undef if_match)
    |> Http.Headers.(undef if_none_match)
    |> Http.Headers.(undef if_modified_since)
    |> Http.Headers.(undef if_unmodified_since)
    |> Http.Headers.(undef if_range)

  let host_change_drops headers =
    headers
    |> Http.Headers.(undef authorization)
    |> Http.Headers.(undef proxy_authorization)
    |> Http.Headers.(undef cookie)

  let redirect_response visited request response =
    match Http.Response.status response with
    | 301 | 302 | 303 | 305 | 307 | 308 ->
        let* url = find_location request response in
        if List.mem url visited then Fmt.error "Redirection loop: %s" url else
        let version = Http.Request.version request in
        let method' = Http.Request.method' request in
        let headers = Http.Request.headers request in
        let headers = unconditional_redirection_drops headers in
        let* last_host = request_host request in
        let* request = Http.Request.of_url ~headers ~version method' ~url in
        let* new_host = request_host request in
        let request =
          if last_host = new_host then request else
          let headers = host_change_drops (Http.Request.headers request) in
          Request.with_headers headers request
        in
        Ok (Some (url, request))
    | _ -> Ok None

  let default_max_redirection = 10

  let request
      ?(max_redirections = default_max_redirection) (V ((module C), c)) ~follow
      request
    =
    let rec loop n follow visited request =
      if n <= 0 then Fmt.error "Too many redirects (%d)" max_redirections else
      let method' = Http.Request.method' request in
      let follow = match method' with `GET | `HEAD -> follow | _ -> false in
      let* response = C.request c request in
      if not follow then Ok response else
      let* redirect = redirect_response visited request response in
      match redirect with
      | Some (url, request) -> loop (n - 1) follow (url :: visited) request
      | None ->
          begin match visited with
          | [] -> Ok response
          | last :: _ ->
              let hs = Http.Response.headers response in
              let hs = Http.Headers.(hs |> def x_follow_location last) in
              Ok (Http.Response.with_headers hs response)
          end
    in
    loop max_redirections follow [] request

  let get httpc ~follow ~url =
    let* request' = Http.Request.of_url `GET ~url in
    let* response = request httpc ~follow request' in
    match Http.Response.status response with
    | 200 -> Http.Body.to_string (Http.Response.body response)
    | st -> Error (Format.asprintf "%a" Http.Status.pp st)
end
