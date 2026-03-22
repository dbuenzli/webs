(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = string

(* Constants *)

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

(* Operations *)

let get_type s =
  let rec after_token s i max =
    if i <= max && Webs__base.is_token_char s.[i]
    then after_token s (i + 1) max else i
  in
  let max = String.length s - 1 in
  let after = after_token s 0 max in
  if after > max then s else
  if s.[after] <> '/' then String.sub s 0 after else
  let after = after_token s (after + 1) max in
  if after > max then s else String.sub s 0 after

(* Converting with file extensions *)

type fpath = Webs__base.Fpath.t
type file_ext = Webs__base.Fpath.file_ext
type of_file_ext_map = t Webs__base.String_map.t
type to_file_ext_map = file_ext Webs__base.String_map.t

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
  Webs__base.String_map.add ext t of_ext_map,
  Webs__base.String_map.add t ext to_ext_map

let default_of_file_ext_map, default_to_file_ext_map =
  let empty = Webs__base.String_map.empty in
  List.fold_left add_file_ext (empty, empty) base_exts

let of_file_ext ?(map = default_of_file_ext_map) ext =
  let default = application_octet_stream in
  Option.value (Webs__base.String_map.find_opt ext map) ~default

let of_filepath ?map file = of_file_ext ?map (Webs__base.Fpath.get_ext file)

let to_file_ext ?(map = default_to_file_ext_map) t =
  match Webs__base.String_map.find_opt t map with
  | Some ext -> ext
  | None ->
      match Webs__base.String_map.find_opt (get_type t) map with
      | Some ext -> ext
      | None -> ".bin"

(* Predicates and comparisons *)

let equal = Repr.equal
let compare = Repr.compare
let is_none t = equal none t
(* Formatting *)

let pp = Format.pp_print_string
