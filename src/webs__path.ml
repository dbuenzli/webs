(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let err_path_start_slash = "no starting '/'"
let err_path_char c = Webs__base.Fmt.str "%C not a path character" c
let err_path_seg_stray_dir_sep = "stray directory separator in path segment"
let err_path_empty = "empty list of segments"

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

type fpath = Webs__base.Fpath.t

let char_is_not_dir_sep c = not (Char.equal c '/' || Char.equal c '\\')
let has_no_dir_seps s = String.for_all char_is_not_dir_sep s
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

let filepath_ext = Webs__base.Fpath.get_ext

(* Converting *)

let decode_segment b ~first ~last s =
  Buffer.clear b;
  Webs__url.Percent.decode_to_buffer b ~first ~last s; Buffer.contents b

(* The following decode allows percents not necessarily followed
   by two hex-digits, RFC 3986 wouldn't allow that, in the whatwg
   we get a validation error but the parsing continues. In
   practice curling URLs with such paths works. *)

let decode s =
  if s = "" then Error Webs__base.err_empty_string else
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
               Webs__url.Percent.is_char_verbatim_in_uri_component c ->
          loop acc b s ~first (i + 1)
      | c -> Error (err_path_char c)
  in
  loop [] (Buffer.create 255) s ~first:1 1

let buffer_encode_path b segs =
  let add_seg seg =
    Buffer.add_char b '/';
    Webs__url.Percent.encode_to_buffer
      Webs__url.Percent.is_char_verbatim_in_uri_component b seg
  in
  List.iter add_seg segs

let encode segs =
  let b = Buffer.create 255 in buffer_encode_path b segs; Buffer.contents b

let and_query_string_of_request_target s =
  let subrange ?first ?last s =
    Some (Webs__base.string_subrange ?first ?last s)
  in
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

(* Predicates and comparisons *)

let equal = Repr.equal
let compare = Repr.compare
let is_none p = equal none p
let is_root p = equal root p

(* Formatting *)

let pp ppf p = Webs__base.Fmt.string ppf (String.concat "/" ("" :: p))
let pp_dump ppf p =
  let pp_sep ppf () = Webs__base.Fmt.pf ppf "@ " in
  let pp_seg ppf s = Webs__base.Fmt.pf ppf "%S" s in
  Webs__base.Fmt.list ~pp_sep pp_seg ppf p
