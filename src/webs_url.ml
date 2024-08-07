(*---------------------------------------------------------------------------
   Copyright (c) 2024 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let string_subrange ?(first = 0) ?last s =
  let max = String.length s - 1 in
  let last = match last with
  | None -> max
  | Some l when l > max -> max
  | Some l -> l
  in
  let first = if first < 0 then 0 else first in
  if first > last then "" else String.sub s first (last - first + 1)

let white = function ' ' | '\t' .. '\r'  -> true | _ -> false
let alpha = function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false
let digit = function '0' .. '9' -> true | _ -> false

(* URL munging *)

type t = string

let scheme_char c =
  alpha c || digit c || Char.equal c '+' || Char.equal c '-' ||
  Char.equal '.' c

let find_scheme_colon u =
  if u = "" || not (alpha u.[0]) then None else
  let max = String.length u - 1 in
  let i = ref 1 in
  while !i <= max && scheme_char u.[!i] do incr i done;
  if !i > max || u.[!i] <> ':' then None else Some !i

let classify s = match find_scheme_colon s with
| None -> `Rel | Some _ -> `Url

let scheme u = match find_scheme_colon u with
| None -> None | Some i -> Some (String.sub u 0 i)

let authority u = match String.index u ':' with
| exception Not_found -> None
| i ->
    let max = String.length u - 1 in
    if i + 2 >= max then None else
    if not (u.[i + 1] = '/' && u.[i + 2] = '/') then None else
    let first = i + 3 in
    let last = match String.index_from u first '/' with
    | exception Not_found -> max
    | j -> j - 1
    in
    if last - first < 0 then None else
    Some (string_subrange ~first ~last u)

let path_and_query u = match String.index u ':' with
| exception Not_found -> None
| i ->
    let max = String.length u - 1 in
    if i = max then None else
    match u.[i + 1] = '/' with
    | false -> Some (string_subrange ~first:(i + 1) u)
    | true ->
        if i + 1 = max then Some "/" else
        match u.[i + 2] = '/' with
        | false -> Some (string_subrange ~first:(i + 1) u)
        | true ->
            match String.index_from u (i + 3) '/' with
            | exception Not_found -> None
            | i -> Some (string_subrange ~first:i u)

let list_of_text_scrape s = (* See .mli to understand what it does *)
  let rec find_stop s i max stop =
    if i > max then i else
    if stop s.[i] then i else find_stop s (i + 1) max stop
  in
  let parse_att s i max =
    let j = find_stop s i max (Fun.negate white) in
    if not (j < max && s.[j] = '=') then None else
    let k = find_stop s (j + 1) max (Fun.negate white) in
    if not (k < max && (s.[k] = '\'' || s.[k] = '\"')) then None else
    let l = find_stop s (k + 1) max (Char.equal s.[k]) in
    if not (l <= max) then None else
    let url = String.trim (string_subrange ~first:(k + 1) ~last:(l - 1) s) in
    if url = "" then None else Some (url, l + 1)
  in
  let rec find_next acc s i max =
    if i > max then List.rev acc else
    match s.[i] with
    | 's' when i + 5 <= max && s.[i+1] = 'r' && s.[i+2] = 'c' ->
        begin match parse_att s (i + 3) max with
        | None -> find_next acc s (i + 3) max
        | Some (url, next) -> find_next (url :: acc) s next max
        end
    | 'h' when i + 6 <= max &&
               s.[i+1] = 'r' && s.[i+2] = 'e' && s.[i+3] = 'f' ->
        begin match parse_att s (i + 4) max with
        | None -> find_next acc s (i + 4) max
        | Some (url, next) -> find_next (url :: acc) s next max
        end
    | 'h' when i + 7 <= max &&
               s.[i+1] = 't' && s.[i+2] = 't' && s.[i+3] = 'p' ->
        let stop =
          if i = 0 then Some white else
          match s.[i - 1] with
          | '\"' | '\'' as c -> Some (Char.equal c)
          | '<' -> Some (Char.equal '>')
          | c when white c -> Some white
          | _ -> None
        in
        begin match stop with
        | None -> find_next acc s (i + 1) max
        | Some stop ->
            let stop = find_stop s i max stop in
            let url = string_subrange ~first:i ~last:(stop - 1) s in
            if not (String.starts_with ~prefix:"http://" url ||
                    String.starts_with ~prefix:"https://" url)
            then find_next acc s (i + 1) max
            else find_next (url :: acc) s stop max
        end
    | _ -> find_next acc s (i + 1) max
  in
  find_next [] s 0 (String.length s - 1)
