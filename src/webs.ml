(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let strf = Format.asprintf
let pf = Format.fprintf
let pp_cut = Format.pp_print_cut
let pp_qstring ppf s = pf ppf "%S" s
let pp_field f pp_v ppf v = pf ppf "@[<h>(%s %a)@]" f pp_v v
let error_to_failure = function Ok v -> v | Error e -> failwith e

module Http = struct
  module Smap = Map.Make (String)

  let string_subrange ?(first = 0) ?(last = max_int) s =
    let max_idx = String.length s - 1 in
    let first = max 0 first in
    let last = min max_idx last in
    if first > last then "" else
    String.sub s first (last - first + 1)

  let string_starts_with ~prefix s =
    let len_a = String.length prefix in
    let len_s = String.length s in
    if len_a > len_s then false else
    let max_idx_a = len_a - 1 in
    let rec loop i =
      if i > max_idx_a then true else
      if String.unsafe_get prefix i <> String.unsafe_get s i
      then false else loop (i + 1)
    in
    loop 0

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

  module Base64 = struct
    let alpha =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

    let alpha_url =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

    let encode ?(url = false) s =
      let rec loop alpha len e ei s i  = match i >= len with
      | true -> Bytes.unsafe_to_string e
      | false ->
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

    let decode ?(url = false) s =
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
      let rec loop len d di s i = match i >= len with
      | true -> Bytes.unsafe_to_string d
      | false ->
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
      if len mod 4 <> 0 then (Error len) else
      let dlen = (len / 4) * 3 in
      let dlen = if s.[len - 1] = '=' then dlen - 1 else dlen in
      let dlen = if s.[len - 2] = '=' then dlen - 1 else dlen in
      try Ok (loop len (Bytes.create dlen) 0 s 0) with Alpha_error i -> Error i
  end

  module Pct = struct
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
    | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true | _ -> false

    let hexdig_to_int = function
    | '0' .. '9' as c -> Char.code c - 0x30
    | 'A' .. 'F' as c -> Char.code c - 0x37
    | 'a' .. 'f' as c -> Char.code c - 0x57
    | _ -> assert false

    let unsafe_hexdig_of_int i =
      if i < 10 then Char.unsafe_chr (i + 0x30) else
      Char.unsafe_chr (i + 0x37)

    let encode_to_buffer b s =
      for k = 0 to String.length s - 1 do match s.[k] with
      | c when is_non_pct_pchar c -> Buffer.add_char b c
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

    let encode s =
      (* XXX one day we should benchmark whether the scan first to determine
         length and then use Bytes directly is faster – see
         Query.pct_encode_space_as_plus) – one day. *)
      let b = Buffer.create (String.length s * 2) in
      encode_to_buffer b s;
      Buffer.contents b

    let decode s =
      let b = Buffer.create (String.length s) in
      decode_to_buffer b s ~first:0 ~last:(String.length s - 1);
      Buffer.contents b
  end

  let crlf = "\r\n"
  let err_miss_eq = "missing '='"
  let err_miss_dash = "missing '-'"
  let err_space_miss = "missing space"
  let err_empty_string = "empty string"
  let err_digits_neg d = strf "negative number (%d)" d
  let err_digits_char c = strf "%C is not a digit" c
  let err_digits_overflow = "sequence of digits overflows"
  let err_token_miss = "missing token"
  let err_token t = strf "%S is not an HTTP token" t
  let err_token_char c = strf "%C not a token character" c
  let err_version = "not an HTTP version"
  let err_path_start_slash = "no starting '/'"
  let err_path_char c = strf "%C not a path character" c
  let err_path_seg_stray_dir_sep = "stray directory separator in path segment"
  let err_path_empty = "empty list of segments"
  let err_rl_garbage = "remaining garbage on the request line"
  let err_header_undefined n = strf "header %s undefined" n
  let err_header_miss_delim = "missing ':' delimiter in header"
  let err_header_value_char c = "%C not a header value character"
  let err_headers_length_conflicts = "conflicting body length specification"
  let err_headers_length = "cannot determine body length"
  let err_empty_multi_value = "multi value cannot be the empty list"
  let err_etag = "not an entity-tag"

  (* HTTP whitespace, see https://tools.ietf.org/html/rfc7230#section-3.2.3 *)

  let[@inline] is_vchar = function '\x21' .. '\x7E' -> true | _ -> false
  let[@inline] is_ows c = c = ' ' || c = '\t'

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

  (* HTTP DIGIT https://tools.ietf.org/html/rfc5234#appendix-B.1 *)

  let[@inline] is_digit = function '0' .. '9' -> true | _ -> false
  let[@inline] digit_to_int c = Char.code c - 0x30 (* assert (is_digit c) *)
  let[@inline] digit_of_int i = Char.chr (i + 0x30) (* assert (0 <= i <= 9 *)
  let[@inline] str_digit_of_int i = String.make 1 (digit_of_int i)

  module Digits = struct
    let decode s =
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

  (* HTTP token, see https://tools.ietf.org/html/rfc7230#section-3.2.6 *)

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

  (* Names *)

  type name = string
  module Name = struct
    type t = name
    let v s = try lower_token_of_string s with Failure e -> invalid_arg e
    let equal = String.equal
    let compare = String.compare
    let pp = Format.pp_print_string
    let encode n = n
    let decode s = try Ok (lower_token_of_string s) with
    | Failure e -> Error e
  end

  (* HTTP method *)

  let meth_of_token = function
  | "GET" -> `GET | "HEAD" -> `HEAD | "POST" -> `POST | "PUT" -> `PUT
  | "DELETE" -> `DELETE | "CONNECT" -> `CONNECT | "OPTIONS" -> `OPTIONS
  | "TRACE" -> `TRACE | "PATCH" -> `PATCH | s -> `Other s

  let decode_method b ~first ~max =
    let first, token = decode_token b ~first ~max in
    first, meth_of_token token

  type meth =
  [ `GET | `HEAD | `POST | `PUT | `DELETE | `CONNECT | `OPTIONS | `TRACE
  | `PATCH | `Other of string ]

  module Meth = struct
    type t = meth

    let decode s = match meth_of_token s with
    | `Other s as o -> if is_token s then Ok o else Error (err_token s)
    | m -> Ok m

    let encode = function
    | `GET -> "GET" | `HEAD -> "HEAD" | `POST -> "POST" | `PUT -> "PUT"
    | `DELETE -> "DELETE" | `CONNECT -> "CONNECT" | `OPTIONS -> "OPTIONS"
    | `TRACE -> "TRACE" | `PATCH -> "PATCH"
    | `Other s -> if is_token s then s else invalid_arg (err_token s)

    let pp ppf m = Format.pp_print_string ppf (encode m)
  end

  (* Headers *)

  type headers = string Smap.t (* always lowercased by header_name *)

  module H = struct
    let name = Name.v

    (* Header values *)

    let values_of_set_cookie_value s = String.split_on_char '\x00' s
    let values_of_string ?(sep = ',') s =
      List.rev @@ List.rev_map trim_ows (String.split_on_char sep s)

    let values_to_string ?(sep = ',') = function
    | [] -> invalid_arg err_empty_multi_value
    | vs -> String.concat (String.make 1 sep) vs

    let is_token = is_token

    (* Header maps *)

    let empty = Smap.empty
    let is_empty = Smap.is_empty
    let mem = Smap.mem
    let find ?(lowervalue = false) n hs = match lowervalue with
    | true -> Option.map string_lowercase (Smap.find_opt n hs)
    | false -> Smap.find_opt n hs

    let undef = Smap.remove
    let get ?lowervalue n hs = match find ?lowervalue n hs with
    | None -> invalid_arg (err_header_undefined n)
    | Some v -> v

    let def = Smap.add
    let def_if_some n o hs = match o with None -> hs | Some v -> def n v hs
    let def_if_undef n v hs = if mem n hs then hs else def n v hs

    let _append_value sep n v hs = match Smap.find_opt n hs with
    | None -> Smap.add n v hs
    | Some v' -> Smap.add n (String.concat sep [v; v']) hs

    let add n v hs = _append_value "," n v hs
    let add_set_cookie v hs = _append_value "\x00" "set-cookie" v hs
    let fold = Smap.fold
    let override hs ~by =
      let merge_right _ _ v = Some v in
      Smap.union merge_right hs by

    let pp ppf hs =
      let pp_header ppf (n, v)  =
        (* if !first then first := false else pp_cut ppf (); *)
        if not (n = "set-cookie") then pp_field n pp_qstring ppf v else
        let cs = values_of_set_cookie_value v in
        List.iter (pp_field "set-cookie" pp_qstring ppf) cs
      in
      Format.pp_print_list pp_header ppf (Smap.bindings hs)

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

    (* Header lookups *)

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
  end

  (* Paths *)

  type fpath = string
  type path = string list

  module Path = struct

    (* Paths *)

    type t = path

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
      if prefix = [] || p = [] then None else
      let rec loop pre acc = match pre, acc with
      | s :: pre, a :: acc when String.equal s a -> loop pre acc
      | ([] | [""]), (_ :: _ as acc) -> Some acc
      | [], [] -> Some [""]
      | _ -> None
      in
      loop prefix p

    let concat p0 p1 = match p0, p1 with
    | [], p1 -> p1
    | p0, [] -> p0
    | p0, p1 ->
        match List.rev p0 with
        | "" :: r -> List.rev_append r p1
        | r -> List.rev_append r p1

    let relativize ~root path =
      let rec dotdots segs ~on:acc = match segs with
      | _ :: segs -> dotdots segs ~on:(".." :: acc) | [] -> acc
      in
      match root, path with (* Simpler if root paths are handled separately *)
      | [_], [""] -> ["."]
      | path, [""] -> dotdots (List.tl path) ~on:[]
      | [_], path -> path
      | root, path ->
          let rec loop last root path = match root, path with
          | r :: root, p :: path when String.equal r p -> loop r root path
          | [], [] -> [if last = "" then "." else last] (* root = path *)
          | [], q -> last :: q (* root = r and path = r/q *)
          | p, [] -> dotdots p ~on:[last] (* root = r/q and path = r *)
          | p, [""] -> dotdots p ~on:[last; ""] (* root = r/q and path = r/ *)
          | p, q -> dotdots (List.tl p) ~on:q (* root = r/p  path = r/q *)
          in
          loop "" root path

    (* File paths *)

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

      let filepath_ext p = match String.rindex_opt p '.' with
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
          | c when c = '%' || Pct.is_non_pct_pchar c ->
              loop acc b s ~first (i + 1)
          | c -> Error (err_path_char c)
      in
      loop [] (Buffer.create 255) s 1 1

    let buffer_encode_path b segs =
      let add_seg seg = Buffer.add_char b '/'; Pct.encode_to_buffer b seg in
      List.iter add_seg segs

    let encode segs =
      let b = Buffer.create 255 in buffer_encode_path b segs; Buffer.contents b

    let pp ppf p =
      let pp_sep ppf () = pf ppf "@ " and pp_seg ppf s = pf ppf "%S" s in
      Format.pp_print_list ~pp_sep pp_seg ppf p

    let and_query_of_request_target s =
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
      | None -> [], q
      | Some p ->
          match decode p with
          | Error _ -> [], None (* TODO what do we do ? this is bad request *)
          | Ok segs -> segs, q
  end

  (* Queries *)

  type query = string list Smap.t (* The list is never empty *)
  module Query = struct
    type t = query
    let empty = Smap.empty
    let is_empty = Smap.is_empty
    let mem = Smap.mem
    let def k v q = Smap.add k [v] q
    let add k v q =
      let vs = match Smap.find_opt k q with None -> [v] | Some vs -> vs @ [v] in
      Smap.add k vs q

    let undef = Smap.remove
    let find k q = match Smap.find_opt k q with
    | None -> None | Some vs -> Some (List.hd vs)

    let find_all k q = Option.value ~default:[] (Smap.find_opt k q)
    let fold f q acc =
      let bindings k vs acc = List.fold_left (fun acc v -> f k v acc) acc vs in
      Smap.fold bindings q acc

    let keep_only_first q = Smap.map (fun vs -> [List.hd vs]) q

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
          loop b (add k v acc) kvs
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
      let pp_sep ppf () = pf ppf "@ " and pp_v ppf v = pf ppf "\"%s\"" v in
      let pp_vs = Format.pp_print_list ~pp_sep pp_v in
      let pp_binding ppf (k, vs) = pp_field k pp_vs ppf vs in
      pf ppf "@[<v>%a@]" (Format.pp_print_list pp_binding) (Smap.bindings q)
  end

    (* MIME types *)

  type mime_type = string
  module Mime_type = struct
    type t = mime_type
    let application_json = "application/json"
    let application_octet_stream = "application/octet-stream"
    let application_x_www_form_urlencoded = "application/x-www-form-urlencoded"
    let text_css = "text/css"
    let text_html = "text/html;charset=utf-8"
    let text_javascript = "text/javascript"
    let text_plain = "text/plain;charset=utf-8"
    let multipart_byteranges = "multipart/byteranges"
    let multipart_form_data = "multipart/form-data"

    type file_ext = string
    type file_ext_map = t Smap.t
    let default_file_ext_map =
      lazy begin
        Smap.empty
        |> Smap.add ".aac"  "audio/aac"
        |> Smap.add ".avi"  "video/x-msvideo"
        |> Smap.add ".bin"  "application/octet-stream"
        |> Smap.add ".bmp"  "image/bmp"
        |> Smap.add ".bz"   "application/x-bzip"
        |> Smap.add ".bz2"  "application/x-bzip2"
        |> Smap.add ".css"	"text/css"
        |> Smap.add ".gz"	  "application/gzip"
        |> Smap.add ".gif"  "image/gif"
        |> Smap.add ".htm"  "text/html"
        |> Smap.add ".html" "text/html"
        |> Smap.add ".ics"	"text/calendar"
        |> Smap.add ".jpeg" "image/jpeg"
        |> Smap.add ".jpg"  "image/jpeg"
        |> Smap.add ".js"	  "text/javascript"
        |> Smap.add ".json"	"text/javascript"
        |> Smap.add ".jsonldx" "application/ld+json"
        |> Smap.add ".md"   "text/markdown;charset=utf-8"
        |> Smap.add ".midi"	"audio/midi audio/x-midi"
        |> Smap.add ".mjs"  "text/javascript"
        |> Smap.add ".mp3"  "audio/mpeg"
        |> Smap.add ".mpeg" "video/mpeg"
        |> Smap.add ".oga"  "audio/ogg"
        |> Smap.add ".ogv"  "video/ogg"
        |> Smap.add ".ogx"  "application/ogg"
        |> Smap.add ".opus"	"audio/opus"
        |> Smap.add ".otf"	"font/otf"
        |> Smap.add ".png"	"image/png"
        |> Smap.add ".pdf"	"application/pdf"
        |> Smap.add ".rar"	"application/vnd.rar"
        |> Smap.add ".rtf"	"application/rtf"
        |> Smap.add ".svg"	"image/svg+xml"
        |> Smap.add ".tar"	"application/x-tar"
        |> Smap.add ".tif"  "image/tiff"
        |> Smap.add ".tiff"	"image/tiff"
        |> Smap.add ".ts"	  "video/mp2t"
        |> Smap.add ".ttf"	"font/ttf"
        |> Smap.add ".txt"	"text/plain;charset=utf-8"
        |> Smap.add ".wav"	"audio/wav"
        |> Smap.add ".weba"	"audio/webm"
        |> Smap.add ".webm"	"video/webm"
        |> Smap.add ".webp"	"image/webp"
        |> Smap.add ".woff"	"font/woff"
        |> Smap.add ".woff2" "font/woff2"
        |> Smap.add ".xhtml" "application/xhtml+xml"
        |> Smap.add ".xml"  "application/xml"
        |> Smap.add ".zip"  "application/zip"
        |> Smap.add ".7z"	  "application/x-7z-compressed"
      end

    let of_file_ext ?map:m ext =
      let m = match m with
      | None -> Lazy.force default_file_ext_map
      | Some m -> m
      in
      let default = application_octet_stream in
      Option.value (Smap.find_opt ext m) ~default

    let of_filepath ?map file = of_file_ext ?map (Path.filepath_ext file)
  end

  (* Etags *)

  module Etag = struct

    (* Etags *)

    type t = { weak : bool; tag : string }
    let v ~weak tag = { weak; tag }
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
        Ok (v ~weak (string_subrange ~first ~last s))
      with
      | Failure e -> Error e

    let encode e = String.concat {|"|} [if e.weak then "W/" else ""; e.tag; ""]

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
    let eval_bytes ~len b =
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

  module Cookie = struct
    type atts = string
    let atts
        ?max_age ?domain ?(path = []) ?(secure = true) ?(http_only = true)
        ?(same_site = "strict") ()
      =
      let max_age = match max_age with
      | None -> "" | Some a -> ";max-age=" ^ string_of_int a
      in
      let domain = match domain with None -> "" | Some d -> ";domain=" ^ d in
      let path = if path = [] then "" else ";path=" ^ (Path.encode path) in
      let secure = if secure then ";Secure" else "" in
      let http_only = if http_only then ";httponly" else "" in
      let same_site = ";samesite=" ^ same_site in
      String.concat "" [max_age; domain; path; secure; http_only; same_site]

    let atts_default = atts ()

    let encode ?(atts = atts_default) ~name value =
      String.concat "" [name; "="; value; atts]

    let decode_list s =
      (* Very lax parsing. TODO better,
         see https://tools.ietf.org/html/rfc6265#section-4.2*)
      let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | c :: cs ->
          match String.index_opt c '=' with
          | None -> Error "illegal cookie pair"
          | Some i ->
              let n = string_subrange ~last:(i - 1) c in
              let v = string_subrange ~first:(i + 1) c in
              let v =
                if v = "" then ""  else
                let len = String.length v - 1 in
                if v.[0] = '\"' && v.[len - 1] = '\"' && len > 1
                then string_subrange ~first:1 ~last:(len - 2) v else
                v
              in
              loop ((n, v) :: acc) cs
      in
      loop [] (H.values_of_string ~sep:';' s)
  end

  (* Status *)

  type status = int

  module Status = struct
    type t = status
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
    | s -> "Unknown"

    let pp ppf s = pf ppf "@[%d %s@]" s (reason_phrase s)
  end

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
  let payload_too_large_413 = 413
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

  (* Versions *)

  type version = int * int

  module Version = struct
    type t = version
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

    let pp ppf v = Format.pp_print_string ppf (encode v)
  end

  (* Low-level codecs *)

  (* Request target *)

  let decode_request_target b ~first ~max =
    let rec loop b i =
      if i <= max && Bytes.get b i <> ' ' then loop b (i + 1) else i - 1
    in
    match loop b first with
    | last when last < first -> failwith err_empty_string
    | last -> last + 1, Bytes.sub_string b first (last - first + 1)

  (* HTTP request line, https://tools.ietf.org/html/rfc7230#section-3.1.1 *)

  let decode_request_line b ~first ~crlf =
    let first, meth = decode_method b ~first ~max:crlf in
    let first = decode_sp b ~first ~max:crlf in
    let first, target = decode_request_target b ~first ~max:crlf in
    let first = decode_sp b ~first ~max:crlf in
    let first, version = Version.decode_of_bytes b ~first ~max:crlf in
    if first = crlf then meth, target, version else failwith err_rl_garbage

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

  (* Misc. *)

  let encode_resp_header_section (maj, min) st reason hs =
    let enc_header n v acc =
      let encode n acc v = n :: ": " :: v :: crlf :: acc in
      if not (String.equal n H.set_cookie) then encode n acc v else
      List.fold_left (encode H.set_cookie) acc (H.values_of_set_cookie_value v)
    in
    let maj = str_digit_of_int maj and min = str_digit_of_int min in
    let st = string_of_int st in
    let hs = H.fold enc_header hs [crlf] in
    String.concat "" @@
    "HTTP/" :: maj :: "." :: min :: " " :: st :: " " :: reason :: crlf :: hs

  module Private = struct
    let trim_ows = trim_ows
    let decode_request_line = decode_request_line
    let decode_header_field = decode_header_field
    let encode_resp_header_section = encode_resp_header_section
  end
end

module Resp = struct

  (* Response bodies *)

  type connection = ..
  type consumer = (bytes * int * int) option -> unit
  type body =
  | Empty
  | Stream of (consumer -> unit)
  | Direct of (connection -> unit)

  let empty_body = Empty
  let stream_body producer = Stream producer
  let direct_body producer = Direct producer
  let body_of_string s =
    Stream begin fun yield ->
      (* N.B. the consumer is not supposed to mutate the bytes. *)
      yield (Some (Bytes.unsafe_of_string s, 0, String.length s));
      yield None
    end

  let pp_body ppf = function
  | Empty -> pf ppf "<empty>"
  | Stream _ -> pf ppf "<stream>"
  | Direct _ -> pf ppf "<direct>"

  (* Responses *)

  type t =
    { version : Http.version;
      status : Http.status;
      reason : string;
      headers : Http.headers;
      body : body;
      (* For the server *)
      explain : string; }

  let get_reason st = function
  | Some s -> s | None -> Http.Status.reason_phrase st

  let v
      ?(version = (1,1)) ?(explain = "") ?reason ?(body = empty_body)
      ?headers:(hs = Http.H.empty) status
    =
    let reason = get_reason status reason in
    let headers = match body with
    | Empty -> Http.H.(def content_length "0" hs)
    | _ -> hs
    in
    { version; status; reason; headers; body; explain }

  let version r = r.version
  let status r = r.status
  let reason r = r.reason
  let headers r = r.headers
  let body r = r.body
  let explain r = r.explain
  let with_status ?(explain = "") ?reason status r =
    let explain = if r.explain = "" then explain else r.explain in
    let reason = get_reason status reason in
    { r with status; reason; explain }

  let with_headers headers r = { r with headers }
  let override_headers ~by:headers r =
    { r with headers = Http.H.override r.headers ~by:headers }

  let with_body body r = { r with body }
  let pp ppf r =
    let sep = Format.pp_print_cut in
    Format.pp_open_vbox ppf 1;
    pp_field "version" Http.Version.pp ppf r.version; sep ppf ();
    pp_field "status" Http.Status.pp ppf r.status; sep ppf ();
    pp_field "reason" Format.pp_print_string ppf r.reason; sep ppf ();
    Http.H.pp ppf r.headers;
    pp_field "body" pp_body ppf r.body;
    Format.pp_close_box ppf ()

  (* Pre-canned answers *)

  let result = function Ok v | Error v -> v

  let content ?explain ?(headers = Http.H.empty) ~mime_type:t st s =
    let l = string_of_int (String.length s) in
    let hs = Http.H.empty in
    let hs = Http.H.(hs |> def content_length l |> def content_type t) in
    let hs = Http.H.override hs ~by:headers in
    v ?explain st ~headers:hs ~body:(body_of_string s)

  let text ?explain ?headers st s =
    content ?explain ?headers ~mime_type:Http.Mime_type.text_plain st s

  let html ?explain ?headers st s =
    content ?explain ?headers ~mime_type:Http.Mime_type.text_html st s

  let json ?explain ?headers st s =
    content ?explain ?headers ~mime_type:Http.Mime_type.application_json st s

  let octets ?explain ?headers st s =
    content ?headers ~mime_type:Http.Mime_type.application_octet_stream st s

  let redirect ?explain ?(headers = Http.H.empty) st loc =
    let hs = Http.H.(empty |> def location loc) in
    let hs = Http.H.override hs ~by:headers in
    v ?explain st ~headers:hs

  let bad_request_400 ?explain ?reason ?headers () =
    Error (v ?explain ?reason ?headers Http.bad_request_400)

  let unauthorized_401 ?explain ?reason ?headers () =
    Error (v ?explain ?reason ?headers Http.unauthorized_401)

  let forbidden_403 ?explain ?reason ?headers () =
    Error (v ?explain ?reason ?headers Http.forbidden_403)

  let not_found_404 ?explain ?reason ?headers () =
    Error (v ?explain ?reason ?headers Http.not_found_404)

  let method_not_allowed_405
      ?explain ?reason ?(headers = Http.H.empty) ~allowed ()
    =
    let ms = String.concat ", " (List.map Http.Meth.encode allowed) in
    let hs = Http.H.(empty |> def allow ms) in
    let hs = Http.H.override hs ~by:headers in
    Error (v ?explain ?reason ~headers:hs Http.method_not_allowed_405)

  let gone_410 ?explain ?reason ?headers () =
    Error (v ?explain ?reason ?headers Http.gone_410)

  let server_error_500 ?explain ?reason ?headers () =
    Error (v ?explain ?reason ?headers Http.server_error_500)

  let not_implemented_501 ?explain ?reason ?headers () =
    Error (v ?explain ?reason ?headers Http.not_implemented_501)

  let map_errors ?(only_empty = false) f r =
    let st = status r in
    if 400 <= st && st <= 599
    then (if only_empty && body r <> Empty then r else f r)
    else r
end

module Req = struct

  (* Request bodies *)

  type body = unit -> (bytes * int * int) option
  let empty_body () = None
  let body_to_string body = match body () with
  | None -> ""
  | Some (bytes, start, len) ->
      let b = Buffer.create 1024 in
      Buffer.add_subbytes b bytes start len;
      let rec go b = match body () with
      | None -> Buffer.contents b
      | Some (bytes, start, len) -> Buffer.add_subbytes b bytes start len; go b
      in
      go b

  (* Requests *)

  type t =
    { service_path : Http.path;
      version : Http.version;
      meth : Http.meth;
      request_target : string;
      path : Http.path;
      query : string option;
      headers : Http.headers;
      body_length : int option;
      body : unit -> (bytes * int * int) option; }

  let v
      ?(service_path = [""]) ?(version = (1,1)) ?body_length
      ?(body = empty_body) ?(headers = Http.H.empty) meth request_target
    =
    let path, query =
      Http.Path.and_query_of_request_target request_target
    in
    let body_length = match body_length with
    | None -> if body == empty_body then Some 0 else None
    | Some l -> l
    in
    { service_path; version; meth; request_target; path; query; headers;
      body_length; body; }

  let version r = r.version
  let meth r = r.meth
  let request_target r = r.request_target
  let service_path r = r.service_path
  let path r = r.path
  let query r = r.query
  let headers r = r.headers
  let body_length r = r.body_length
  let body r = r.body
  let with_headers headers r = { r with headers }
  let with_body ~body_length body r = { r with body_length; body }
  let with_path path r = { r with path }
  let with_service_path service_path r = { r with service_path }
  let pp_query ppf = function None -> pf ppf "" | Some q -> pf ppf "%S" q
  let pp_body_length ppf = function
  | None -> pf ppf "unknown" | Some l -> pf ppf "%d" l

  let pp ppf r =
    pf ppf "@[<v>";
    pp_field "service-path" Http.Path.pp ppf r.service_path; pp_cut ppf ();
    pp_field "version" Http.Version.pp ppf r.version; pp_cut ppf ();
    pp_field "method" Http.Meth.pp ppf r.meth; pp_cut ppf ();
    pp_field "request-target" Format.pp_print_string ppf r.request_target;
    pp_cut ppf ();
    pp_field "path" Http.Path.pp ppf r.path; pp_cut ppf ();
    pp_field "query" pp_query ppf r.query; pp_cut ppf ();
    Http.H.pp ppf r.headers; pp_cut ppf ();
    pp_field "body-length" pp_body_length ppf r.body_length;
    pf ppf "@]"

  (* Request responses *)

  let service_redirect ?explain status p r =
    let loc = Http.Path.(encode @@ concat (service_path r) p) in
    Resp.redirect ?explain status loc

  let echo ?(status = Http.not_found_404) r =
    let body = body_to_string (body r) in
    let body = Format.asprintf "@[<v>%a@,%s@]" pp r body in
    Resp.text status body

  (* Request deconstruction *)

  module Allow = struct
    type req = t
    type 'a t = Http.meth * 'a

    let meths allowed r =
      let rec loop mr = function
      | m :: ms -> if (fst m) = mr then Ok (snd m) else loop mr ms
      | [] -> Resp.method_not_allowed_405 ~allowed:(List.map fst allowed) ()
      in
      loop (meth r) allowed

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

  let decode_header h dec req = match Http.H.find h (headers req) with
  | None -> Ok None
  | Some v ->
      match dec v with
      | Ok v -> Ok (Some v)
      | Error e ->
          let reason = strf "%s: %s" (h :> string) e in
          Error (Resp.v Http.bad_request_400 ~reason)

  let bad_strip_404 =
    Resp.v ~explain:"could not strip prefix" Http.not_found_404

  let forward_service ~strip r =
    match Http.Path.strip_prefix ~prefix:strip (path r) with
    | None -> Error bad_strip_404
    | Some path ->
        let service_path = Http.Path.concat (service_path r) strip in
        Ok { r with path; service_path }

  let to_absolute_filepath ?(strip = [""]) ~root r =
    match Http.Path.strip_prefix ~prefix:strip (path r) with
    | None -> Error bad_strip_404
    | Some p ->
        match Http.Path.to_absolute_filepath p with
        | Error e -> Error (Resp.v ~explain:e Http.bad_request_400)
        | Ok filepath -> Ok (Http.Path.prefix_filepath root filepath)

  let to_query r =
    let url_query r = match query r with
    | None -> Ok Http.Query.empty | Some q -> Ok (Http.Query.decode q)
    in
    let body_query r =
      match Http.H.(find ~lowervalue:true content_type (headers r)) with
      | None ->
          Error (Resp.v ~reason:"missing content type" Http.bad_request_400)
      | Some t ->
          (* TODO proper Mimetype decoding *)
          match String.split_on_char ';' t with
          | (t :: _) when
              String.equal (String.trim t)
                Http.Mime_type.application_x_www_form_urlencoded ->
              Ok (Http.Query.decode @@ body_to_string (body r))
          | _ -> Error (Resp.v Http.unsupported_media_type_415)
    in
    match meth r with
    | `GET | `HEAD -> url_query r
    | _ -> body_query r

  let to_service ~strip r =
    match Http.Path.strip_prefix ~prefix:strip (path r) with
    | None ->
        Error (Resp.v ~explain:"could not strip path" Http.bad_request_400)
    | Some path ->
        let service_path = Http.Path.concat (service_path r) strip in
        Ok { r with service_path; path }

  let clean_path r =
    let not_empty s = not (String.equal s "") in
    match path r with
    | p when List.for_all not_empty p -> Ok r
    | [] | [""] -> Ok r
    | p ->
        let path = match (List.filter not_empty p) with [] -> [""] | p -> p in
        let path = Http.Path.encode path in
        let explain = "path cleaning" in
        Error (Resp.redirect ~explain Http.moved_permanently_301 path)
end

type service = Req.t -> Resp.t

module Connector = struct
  type dur_ns = int64
  type log_msg =
  [ `Service_exn of exn * Stdlib.Printexc.raw_backtrace
  | `Connector_exn of exn * Stdlib.Printexc.raw_backtrace
  | `Connection_reset
  | `Trace of dur_ns * Req.t option * Resp.t option ]

  let no_log _ = ()

  let pp_exn_backtrace ~kind ppf (e, bt) = (* so many lines. *)
    let string = Format.pp_print_string in
    let exn ppf e = string ppf (Printexc.to_string e) in
    let pp_backtrace_str ppf s =
      let stop = String.length s - 1 (* there's a newline at the end *) in
      let rec loop left right =
        if right = stop then string ppf (String.sub s left (right - left)) else
        if s.[right] <> '\n' then loop left (right + 1) else
        begin
          string ppf (String.sub s left (right - left));
          Format.pp_print_cut ppf ();
          loop (right + 1) (right + 1)
        end
      in
      if s = "" then string ppf "No backtrace available." else
      loop 0 0
    in
    Format.fprintf ppf "@[<v>Unexpected %s exception: %a@,%a@]"
      kind exn e pp_backtrace_str (Printexc.raw_backtrace_to_string bt)

  let pp_service_exn ppf e = pp_exn_backtrace ~kind:"service" ppf e
  let pp_connector_exn ppf e = pp_exn_backtrace ~kind:"connector" ppf e
  let pp_trace ppf dur_ns req resp =
    let strf = Printf.sprintf in
    let dur = match Int64.(equal zero dur_ns) with
    | true -> ""
    | false ->
        match Int64.compare dur_ns 1_000_000L (* < 1ms *) with
        | -1 -> strf " % 3Luµs" (Int64.unsigned_div dur_ns 1_000L)
        | _ -> strf " % 3Lums" (Int64.unsigned_div dur_ns 1_000_000L)
    in
    let meth req = match Req.meth req with
    | `POST | `PUT | `DELETE | `PATCH as m ->
        strf "\x1B[34m%s\x1B[0m" (Http.Meth.encode m)
    | m ->  Http.Meth.encode m
    in
    let query req = match Req.query req with
    | None -> ""
    | Some q -> strf "?%s" q
    in
    let path req =
      strf "\x1B[1m%s\x1B[0m%s" (Http.Path.encode (Req.path req)) (query req)
    in
    let status resp = match Resp.status resp with
    | st when st <= 299 -> strf "\x1B[32m%d\x1B[0m" st
    | st when st <= 399 -> strf "\x1B[93m%d\x1B[0m" st
    | st when 400 <= st && st <= 599 -> strf "\x1B[31m%d\x1B[0m" st
    | st -> string_of_int st
    in
    match req, resp with
    | Some req, Some resp ->
        let data =
          String.concat "" @@
          meth req :: " [" :: status resp :: dur :: "] " ::
          path req :: " (" :: Resp.reason resp :: ")" ::
          (if Resp.explain resp = "" then [] else [ " "; Resp.explain resp])
        in
        Format.pp_print_string ppf data
    | Some req, None ->
        let data =
          String.concat "" @@
          meth req :: " [" :: dur :: "] " ::
          path req :: " No response" :: []
        in
        Format.pp_print_string ppf data
    | None, Some resp ->
        let data =
          String.concat "" @@
          "???" :: " [" :: status resp :: dur :: "] " ::
          "???" :: " (" :: Resp.reason resp :: ")" ::
          (if Resp.explain resp = "" then [] else [ " "; Resp.explain resp])
        in
        Format.pp_print_string ppf data
    | None, None ->
        Format.pp_print_string ppf "trace really ?"

  let pp_connection_reset ppf () =
    Format.fprintf ppf "Connection reset by peer."

  let pp_log_msg ppf = function
  | `Trace (dur, req, resp) -> pp_trace ppf dur req resp
  | `Service_exn e -> pp_service_exn ppf e
  | `Connector_exn e -> pp_connector_exn ppf e
  | `Connection_reset -> pp_connection_reset ppf ()

  let default_log ?(ppf = Format.err_formatter) ~trace () = function
  | `Trace _ when not trace -> ()
  | `Trace (dur, req, resp) ->
      pp_trace ppf dur req resp; Format.pp_print_newline ppf ()
  | `Service_exn e -> pp_service_exn ppf e; Format.pp_print_newline ppf ()
  | `Connector_exn e -> pp_connector_exn ppf e; Format.pp_print_newline ppf ()
  | `Connection_reset ->
      pp_connection_reset ppf (); Format.pp_print_newline ppf ()
end

(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers

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
