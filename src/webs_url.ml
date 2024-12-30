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

(* Urls *)

type scheme = string
type authority = string
type path = string
type query = string
type fragment = string
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

let find_authority_last ~start u =
  let max = String.length u - 1 in
  if start > max then None else
  if start + 1 > max then Some (start - 1) else
  if not (u.[start] = '/' && u.[start + 1] = '/') then Some (start - 1) else
  let i = ref (start + 2) in
  while (!i <= max && u.[!i] <> '/' && u.[!i] <> '?' && u.[!i] <> '#')
  do incr i done;
  Some (!i - 1)

let scheme u = match find_scheme_colon u with
| None -> None | Some i -> Some (String.sub u 0 i)

let authority u =
  let start = match find_scheme_colon u with
  | None -> 0 | Some i -> i + 1
  in
  let first = start + 2 in
  match find_authority_last ~start u with
  | None -> None
  | Some last when last >= first -> Some (string_subrange ~first ~last u)
  | Some _ -> None

let path_first u =
  let start = match find_scheme_colon u with
  | None -> 0 | Some i -> i + 1
  in
  let first = match find_authority_last ~start u with
  | None -> start | Some last -> last + 1
  in
  let max = String.length u - 1 in
  if first > max || u.[first] = '#' || u.[first] = '?' then None else Some first

let path_last u ~first =
  let max = String.length u - 1 in
  let i = ref (first + 1) in
  while (!i <= max && u.[!i] <> '?' && u.[!i] <> '#') do incr i done;
  !i - 1

let path u = match path_first u with
| None -> None
| Some first -> Some (string_subrange ~first ~last:(path_last u ~first) u)

let query u =
  let max = String.length u - 1 in
  let i = ref 0 in
  while (!i <= max && u.[!i] <> '?' && u.[!i] <> '#') do incr i done;
  if !i > max || u.[!i] = '#' then None else begin
    incr i;
    let first = !i in
    while (!i <= max && u.[!i] <> '#') do incr i done;
    let last = !i - 1 in
    Some (string_subrange ~first ~last u)
  end

let fragment u =
  let max = String.length u - 1 in
  let i = ref 0 in
  while (!i <= max && u.[!i] <> '#') do incr i done;
  if !i > max then None else Some (string_subrange ~first:(!i + 1) u)

(* Kinds *)

type relative_kind = [ `Scheme | `Abs_path | `Rel_path | `Empty ]
type kind = [ `Abs | `Rel of relative_kind ]

let relative_kind s =
  let len = String.length s in
  if len = 0 then `Empty else
  if s.[0] = '/'
  then (if len > 1 && s.[1] = '/' then `Scheme else `Abs_path)
  else `Rel_path

let kind s = match find_scheme_colon s with
| Some _ -> `Abs
| None -> `Rel (relative_kind s)

(* Operations *)

let update ?scheme:s ?authority:a ?path:p ?query:q ?fragment:f u =
  let add_scheme s u = match s with None -> u | Some s -> s :: ":" :: u in
  let add_authority a u = match a with None -> u | Some a -> "//" :: a :: u in
  let add_path p u = match p with None -> u | Some p -> p :: u in
  let add_query q u = match q with None -> u | Some q -> "?" :: q :: u in
  let add_frag f u = match f with None -> u | Some f -> "#" :: f :: u in
  let s = match s with None -> scheme u | Some s -> s in
  let a = match a with None -> authority u | Some a -> a in
  let p = match p with None -> path u | Some p -> p in
  let q = match q with None -> query u | Some q -> q in
  let f = match f with None -> fragment u | Some f -> f in
  String.concat "" @@ add_scheme s @@ add_authority a @@ add_path p @@
  add_query q @@ add_frag f @@ []

let path_and_rest u = match path_first u with
| None -> None | Some first -> Some (string_subrange ~first u)

let drop_path_and_rest u = match path_first u with
| None -> u | Some first -> string_subrange ~last:(first - 1) u

let append root u = match kind u with
| `Abs -> u
| `Rel `Scheme ->
    begin match scheme root with
    | None -> u | Some scheme -> String.concat ":" [scheme; u]
    end
| `Rel `Abs_path -> String.concat "" [drop_path_and_rest root; u]
| `Rel `Rel_path ->
    if root <> "" && root.[String.length root - 1] = '/'
    then String.concat "" [root; u] else
    begin match String.rindex root '/' with
    | exception Not_found -> String.concat "/" [root; u]
    | i ->
        match find_scheme_colon root with
        | None -> String.concat "" [string_subrange ~last:i root; u]
        | Some j when j + 2 = i -> String.concat "/" [root; u]
        | Some _ -> String.concat "" [string_subrange ~last:i root; u]
    end
| `Rel `Empty -> root

(* Scraping *)

let list_of_text_scrape ?root s = (* See .mli to understand what it does *)
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
    let add_url url acc = match root with
    | None -> url :: acc | Some root -> (append root url) :: acc
    in
    if i > max then List.rev acc else
    match s.[i] with
    | 's' when i + 5 <= max && s.[i+1] = 'r' && s.[i+2] = 'c' ->
        begin match parse_att s (i + 3) max with
        | None -> find_next acc s (i + 3) max
        | Some (url, next) -> find_next (add_url url acc) s next max
        end
    | 'h' when i + 6 <= max &&
               s.[i+1] = 'r' && s.[i+2] = 'e' && s.[i+3] = 'f' ->
        begin match parse_att s (i + 4) max with
        | None -> find_next acc s (i + 4) max
        | Some (url, next) -> find_next (add_url url acc) s next max
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
            else find_next (add_url url acc) s stop max
        end
    | _ -> find_next acc s (i + 1) max
  in
  find_next [] s 0 (String.length s - 1)

(* Formatting *)

let pp = Format.pp_print_string
let pp_kind ppf k = Format.pp_print_string ppf @@ match k with
| `Abs -> "abs" | `Rel `Scheme -> "rel-scheme"
| `Rel `Abs_path -> "rel-abs-path" | `Rel `Rel_path -> "rel-rel-path"
| `Rel `Empty -> "rel-empty"
