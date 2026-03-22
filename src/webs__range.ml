(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)


let err_miss_dash = "missing '-'"
let err_miss_eq = "missing '='"

(* Byte ranges *)

type bytes = First of int | Last of int | Range of { first : int; last : int }
let eval_bytes ~length:len b =
  let max = len - 1 in
  match b with
  | First f -> if f > max then None else Some (f, max)
  | Last n -> if n = 0 then None else Some (len - n, max)
  | Range {first; last} ->
      if first > max then None else Some (first, min last max)

(* Ranges *)

type t = Bytes of bytes list | Other of string * string

(* Converting *)

let decode_range s = match String.index_opt s '-' with
| None -> failwith err_miss_dash
| Some i ->
    let first = Webs__base.string_subrange ~last:(i - 1) s in
    let last = Webs__base.string_subrange ~first:(i + 1) s in
    match first with
    | "" -> Last (Webs__digits.decode last |> Result.error_to_failure)
    | first ->
        let first = Webs__digits.decode first |> Result.error_to_failure in
        match last with
        | "" -> First first
        | last ->
            let last = Webs__digits.decode last |> Result.error_to_failure in
            if last < first then failwith "invalid range" else
            Range {first; last}

let decode s = match String.index_opt s '=' with
| None -> Error err_miss_eq
| Some i ->
    let unit = Webs__base.string_subrange ~last:(i - 1) s in
    let v = Webs__base.string_subrange ~first:(i + 1) s in
    match unit with
    | "bytes" ->
        let rs = String.split_on_char ',' v in
        if rs = [] then Error "no range" else
        (try Ok (Bytes (List.rev (List.rev_map decode_range rs)))
        with Failure e -> Error e)
    | s when Webs__base.is_token s -> Ok (Other (s, v))
    | s -> Error (Webs__base.err_token s)

let encode = function
| Other (u, v) -> String.concat "=" [u;v]
| Bytes rs ->
    let int = Webs__digits.encode in
    let encode_bytes acc = function
    | First f -> "-" :: int f :: acc
    | Last n -> int n :: "-" :: acc
    | Range {first; last} -> int last :: "-" :: int first :: acc
    in
    let rs = List.rev (List.fold_left encode_bytes [] rs) in
    String.concat "" ("bytes" :: "=" :: rs)

(* Predicates and comparisons *)

let equal = Repr.equal
let compare = Repr.compare

(* Formatting *)

let pp ppf r = Format.pp_print_string ppf (encode r)
