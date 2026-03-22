(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = string list (* The list is never empty *) Webs__base.String_map.t
let empty = Webs__base.String_map.empty
let define k v q = Webs__base.String_map.add k [v] q
let undefine = Webs__base.String_map.remove
let append_value k v q =
  let vs = match Webs__base.String_map.find_opt k q with
  | None -> [v] | Some vs -> vs @ [v]
  in
  Webs__base.String_map.add k vs q

(* Lookup *)

let find_first k q = match Webs__base.String_map.find_opt k q with
| None -> None | Some vs -> Some (List.hd vs)

let find_all k q = Option.value ~default:[] (Webs__base.String_map.find_opt k q)
let fold f q acc =
  let bindings k vs acc = List.fold_left (fun acc v -> f k v acc) acc vs in
  Webs__base.String_map.fold bindings q acc

(* Predicates and comparisons *)

let is_empty = Webs__base.String_map.is_empty
let mem = Webs__base.String_map.mem
let equal = Webs__base.String_map.equal Repr.equal
let compare = Webs__base.String_map.compare Repr.compare

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
      begin match Webs__url.Percent.is_hexdig hi &&
                  Webs__url.Percent.is_hexdig lo with
      | false -> Buffer.add_char b '%'; incr i
      | true ->
          let c = (Webs__url.Percent.hexdig_to_int hi lsl 4) lor
                  (Webs__url.Percent.hexdig_to_int lo) in
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
      Bytes.set b !out (Webs__url.Percent.unsafe_hexdig_of_int hi); incr out;
      Bytes.set b !out (Webs__url.Percent.unsafe_hexdig_of_int lo); incr out;
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
      loop b (append_value k v acc) kvs
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

(* Formatting *)

let pp ppf q =
  let open Webs__base in
  let pp_sep ppf () = Fmt.pf ppf "@ " in
  let pp_v ppf v = Fmt.pf ppf "\"%s\"" v in
  let pp_vs = Fmt.list ~pp_sep pp_v in
  let pp_binding ppf (k, vs) = Fmt.field k pp_vs ppf vs in
  Fmt.pf ppf "@[<v>%a@]" (Fmt.list pp_binding) (String_map.bindings q)
