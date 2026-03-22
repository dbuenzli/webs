(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Media type constants and file extensions.

    @canonical Webs.Media_type *)

(** {1:media_types Media types} *)

type t = string
(** The type for
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-media-type}media types}. *)

(** {1:constants Constants} *)

val none : t
(** [none] is [""], a content type for when there is none. See also
    {!is_none}. *)

val application_json : t
(** [application_json] is ["application/json"], JSON text. *)

val application_octet_stream : t
(** [application_octet_stream] is ["application/octet-stream"],
    arbitrary bytes. *)

val application_x_www_form_urlencoded : t
(** [application_x_www_form_urlencoded] is
    ["application/x-www-form-urlencoded"]. *)

val text_css : t
(** [text_css] is ["text/css"], a CSS stylesheet. *)

val text_html : t
(** [text_html] is ["text/html; charset=utf-8"], UTF-8 encoded HTML text. *)

val text_javascript : t
(** [text_jvascript] is ["text/javascript"], JavaScript code. *)

val text_plain : t
(** [text_plain] is ["text/plain; charset=utf-8"], UTF-8 encoded plain
    text. *)

val multipart_byteranges : t
(** [multipart_byteranges] is ["multipart/byteranges"]. *)

val multipart_form_data : t
(** [multipart_form_data] is ["multipart/form-data"]. *)

(** {1:operations Operations} *)

val get_type : t -> string
(** [get_type t] is a lenient parser for the [type/subtype] part of the
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-media-type}[media-type]}
    [t] (i.e. it drops the parameters). This parses and returns one or two
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-tokens}tokens}
    separated by [/]. Can be used on lowercased
    {!Http.Headers.content_type} header values to match types. *)

(** {1:exts Converting with file extensions} *)

type fpath = string
(** The type for file paths. *)

type file_ext = string
(** The type for file extensions, including the [.] character. *)

type of_file_ext_map = t Map.Make(String).t
(** The type for maps from {{!file_ext}file extensions} to media types. *)

val of_file_ext : ?map:of_file_ext_map -> file_ext -> t
(** [of_file_ext ~map ext] is the value of [ext] in [map] or
    ["application/octet-stream"] if [ext] is unbound in [map].
    [map] defaults to {!default_of_file_ext_map}. *)

val of_filepath : ?map:of_file_ext_map -> fpath -> t
(** [of_filepath ~map f] is [of_file_ext ~map (Http.Path.filepath_ext f)]. *)

val default_of_file_ext_map : of_file_ext_map
(** [default_to_file_ext_map] is a default extension to media type
    map. The map is documented by its implementation (sorry). Non
    self-describing [text/*] media types have the parameter
    [charset=utf-8]. *)

type to_file_ext_map = file_ext Map.Make(String).t
(** The type for maps from media types to {{!file_ext}file extensions}. *)

val to_file_ext : ?map:to_file_ext_map -> t -> file_ext
(** [to_file_ext ~map t] is the value [t] in [map] or [".bin"] if both [t]
    and [to_type t] are unbound in [map]. [map] defaults to
    {!default_to_file_ext_map}. *)

val default_to_file_ext_map : of_file_ext_map
(** [default_to_file_ex_map] is a default media type to extension map.
    The map is documented by its implementation (sorry). *)

(** {1:predicates Predicates and comparisons} *)

val is_none : t -> bool
(** [is_none t] is [equal none t]. *)

val equal : t -> t -> bool
(** [equal] tests media types for (binary) equality. See also {!get_type}. *)

val compare : t -> t -> int
(** [compare] is a total order on media types compatible with {!equal}. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats media types for inspections. *)
