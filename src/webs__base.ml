(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Preliminaries *)

let[@inline] upper_to_lower c (* assert is_upper *) =
  Char.(unsafe_chr (code c + 32))

let string_subrange ?(first = 0) ?(last = max_int) s =
  let max_idx = String.length s - 1 in
  let first = max 0 first in
  let last = min max_idx last in
  if first > last then "" else String.sub s first (last - first + 1)

let string_chop_known_prefix ~prefix s =
  let len = String.length prefix in
  String.sub s len (String.length s - len)

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
  type 'a t = Format.formatter -> 'a -> unit
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
let err_space_miss = "missing space"
let err_empty_string = "empty string"
let err_token_miss = "missing token"
let err_token t = Fmt.str "%S is not an HTTP token" t
let err_token_char c = Fmt.str "%C not a token character" c
let err_header_value_char c = Fmt.str "%C not a header value character" c
let err_header_miss_delim = "missing ':' delimiter in header"
let err_header_miss_name = "missing header name"

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

let is_token_char c = (is_upper[@inlined]) c || (is_lower_tchar[@inlined]) c
let is_token s =
  if s = "" then false else
  let max = String.length s - 1 in
  let rec loop s i =
    if i > max then true else
    if is_token_char s.[i] then loop s (i + 1) else false
  in
  loop s 0

let decode_token b ~first ~max =
  let rec loop b i max =
    if i <= max && is_token_char (Bytes.get b i)
    then loop b (i + 1) max else i - 1
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
