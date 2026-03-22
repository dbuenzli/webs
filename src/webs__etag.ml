(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let err_etag = "not an entity-tag"
let is_etagc = function
| '\x21' | '\x23' .. '\x7E' | '\x80' .. '\xFF' -> true | _ -> false

type t = { weak : bool; tag : string }
let make ~weak tag = { weak; tag }
let tag s = s.tag
let is_weak s = s.weak

(* Converting *)

let decode s =
  try
    (* See https://www.rfc-editor.org/rfc/rfc9110#name-etag *)
    let max = String.length s - 1 in
    if max < 1 then failwith err_etag else
    let weak = s.[0] = 'W' && s.[1] = '/' in
    let start = if weak then 2 else 0 in
    if not (s.[start] = '\"' && s.[max] = '\"') then failwith err_etag else
    let first = start + 1 and last = max - 1 in
    for i = first to last
    do if not (is_etagc s.[i]) then failwith err_etag done;
    Ok (make ~weak (Webs__base.string_subrange ~first ~last s))
  with
  | Failure e -> Error e

let encode e = String.concat "\"" [if e.weak then "W/" else ""; e.tag; ""]

(* Matching *)

let weak_match e0 e1 = String.equal e0.tag e1.tag
let strong_match e0 e1 =
  not e0.weak && not e1.weak && String.equal e0.tag e1.tag

(* Etag conditions *)

type cond = [ `Any | `Etags of t list ]

let decode_cond = function
| "*" -> Ok `Any
| s ->
    try
      let parse_etag s = match decode (Webs__base.trim_ows s) with
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

(* Predicates and comparisons *)

let equal = Repr.equal
let compare = Repr.compare

(* Formatting *)

let pp ppf etag = Format.pp_print_string ppf (encode etag)
