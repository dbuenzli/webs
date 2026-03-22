(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Absolute paths.

    @canonical Webs.Http.Path *)

(** {1:paths Paths} *)

type t = string list
(** The type for {e absolute} URL paths represented as {e non-empty}
    lists of {e percent-decoded} path segments:
    {ul
    {- The empty list denotes the absence of a path.}
    {- Path segments can be empty [""].}
    {- The root path [/] is represented by [[""]].}
    {- The path [/a] is represented by [["a"]], see more
        examples {{!Path.decode}here}.}}

    {b Warning.} You should {b never} concatenate these segments
    with a separator to get a file path: they may contain stray
    percent-decoded directory separators. Use the function
    {!Path.to_absolute_filepath} to interpret paths as file
    paths. *)

val none : t
(** [none] is the path [[]] for when there is none. See also {!is_none}. *)

val root : t
(** [root] is the root path [[""]]. See also {!is_root}. *)

(** {1:operation Operations} *)

val undot_and_compress : t -> t
(** [undot_and_compress p] removes ["."] and [".."]  according to
    the RFC 3986
    {{:https://www.rfc-editor.org/rfc/rfc3986#section-5.2.4}algorithm}
    and suppresses non-final empty [""] segments. *)

val strip_prefix : prefix:t -> t -> t
(** [strip_prefix ~prefix p] removes the prefix path [prefix] from
    [p].

    If [prefix] ends with an empty segment, it matches any
    corresponding segment at that point so that stripping [/a/]
    from [/a/b] results in [/b]. However stripping [/a/] from [/a]
    yields [[]] ({!none}).

    If [p] is not prefixed by [prefix], or if any of [prefix]
    or [p] is [[]] ({!none}), [[]] is returned.

    Given a path [p] and the same path [p'] with a trailing slash,
    the set of paths prefixed by [p] is the the set of path
    prefixed by [p'] plus [p] itelf. Stripping [p] to itself
    yields {!root} (see {{!Webs_kurl.root_paths}here} for
    why we think that's desirable).

    A few examples basic edge cases {!root} and {!none}:
    {ul
    {- [strip_prefix [""] (_ :: _ as p) = p]}
    {- [strip_prefix (_ :: _ as p) p = [""]]}
    {- [strip_prefix _ [] -> []]}
    {- [strip_prefix [] _ = []]}}

    Stripping a prefix [/a]:
    {ul
    {- [strip_prefix ["a"] [""] = []]}
    {- [strip_prefix ["a"] ["a"] = [""]]}
    {- [strip_prefix ["a"] ["a"; ""] = [""]]}
    {- [strip_prefix ["a"] ["b"] = []]}
    {- [strip_prefix ["a"] ["a"; "b"] = ["b"]]}
    {- [strip_prefix ["a"] ["a"; "b"; ""] = ["b"; ""]]}
    {- [strip_prefix ["a"] ["a"; ""; "b"] = [""; "b"]]}}

    Stripping a prefix [/a/]:
    {ul
    {- [strip_prefix ["a"; ""] [""] = []]}
    {- [strip_prefix ["a"; ""] ["a"] = []]}
    {- [strip_prefix ["a"; ""] ["b"] = []]}
    {- [strip_prefix ["a"; ""] ["a"; ""] = [""]]}
    {- [strip_prefix ["a"; ""] ["a"; "b"] = ["b"]]}
    {- [strip_prefix ["a"; ""] ["a"; "b"; ""] = ["b"; ""]]}
    {- [strip_prefix ["a"; ""] ["a"; ""; "b"] = [""; "b"]]}} *)

val concat : t -> t ->  t
(** [concat p0 p1] concatenates [p0] and [p1]. If [p0] ends with
    an empty segment and [p1] is not {!none} that empty segment is dropped.
    A few examples:
    {ul
    {- [concat p [] = p]}
    {- [concat [] p = p]}
    {- [concat [""] [""] = [""]]}
    {- [concat [""] ["a"; "b"] = ["a"; "b"]]}
    {- [concat ["a"] [""] = ["a"; ""]]}
    {- [concat ["a"; ""] [""] = ["a"; ""]]}
    {- [concat ["a"; "b"] ["c"; "d"] = ["a"; "b"; "c"; "d"]]}
    {- [concat ["a"; "b"; ""] ["c"; "d"] = ["a"; "b"; "c"; "d"]]}
    {- [concat ["a"; "b"; ""] [""] = ["a"; "b"; ""]]}
    {- [concat ["a"; "b"; ""] [""; "c"] = ["a"; "b"; ""; "c"]]}} *)

val relative : src:t -> dst:t -> t
(** [relative ~src ~dst] is the relative path [rel] that goes from
    absolute [src] to absolute [dst]. This means that
    [undot_and_compress (concat src rel)] should yield
    [dst].

    {b Warning.} This function assumes both [src] and [dst] have
    no relative or empty path components. If needed use
    {!undot_and_compress} to ensure that. *)

(** {1:filepath File paths} *)

type fpath = string
(** The type for file paths. *)

val has_dir_seps : string -> bool
(** [has_dir_seps s] is true iff [s] contains a '/' or a '\\'
    character. *)

val to_absolute_filepath : t -> (fpath, string) result
(** [to_absolute_filepath p] is an absolute file path for the path
    {!undot_and_compress}[ p]. Errors if any of the path segments
    contains a stray slash or backslash or if [p] is the empty
    list. The result always uses [/] as a directory separator
    regardless of the platform and is guaranteed to be free of any [.]
    or [..] segments. *)

val prefix_filepath : prefix:fpath -> fpath -> fpath
(** [prefix_filepath ~prefix p] prefixes [p] by [prefix] avoiding
    introducing empty segments. This function assumes [/] is the
    directory separator regardless of the platform. *)

val filepath_ext : fpath -> string
(** [filepath_ext p] is the file extension of file path [p].
    This function assumes [/] is the directory separator regardless
    of the platform. *)

(** {1:conv Converting} *)

val encode : t -> string
(** [encode p] encodes an
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-uri-references}
    [absolute-path]}
    for [p] as follows:

    {ol
    {- In each segment {{:http://www.rfc-editor.org/rfc/rfc3986#section-2.1}
     percent-encode} any byte that is not
     {{:http://www.rfc-editor.org/rfc/rfc3986#section-2.3}[unreserved]},
     {{:http://www.rfc-editor.org/rfc/rfc3986#section-2.2}[sub-delims]},
     [':'] or ['@'] to produce a valid URL
     {{:http://www.rfc-editor.org/rfc/rfc3986#section-3.3}[segment].}}
    {- Prepends each segment with a ['/'].}
    {- Concatenate the result.}}

    The empty list is special cased and yields [""]. This is for
    encoding HTTP paths, use {!to_absolute_filepath} to
    convert paths to file paths.

    Here are a few examples:
    {ul
    {- [encode [] = ""]}
    {- [encode [""] = "/"]}
    {- [encode [""; ""] = "//"]}
    {- [encode [""; "a"] = "//a"]}
    {- [encode ["a";"b";"c"] = "/a/b/c"]}
    {- [encode ["a";"b";"";"c";] = "/a/b//c"]}
    {- [encode ["a";"b";"c";""] = "/a/b/c/"]}
    {- [encode ["a";"b";"c";" "] = "/a/b/c/%20"]}
    {- [encode ["a";"b";"c";"";""] = "/a/b/c//"]}
    {- [encode ["a"; "b/"; "c"] = "/a/b%2F/c"]}
    {- [encode ["r\xC3\xC9volte"] = "/r%C3%C9volte"]}
    {- [encode ["a"; "not%20"; "b"] = "/a/not%2520/b"]}} *)

val decode : string -> (t, string) result
(** [decode s] decodes an
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-uri-references}
    [absolute-path]}
    to its
    {{:http://www.rfc-editor.org/rfc/rfc3986#section-2.1}percent-decoded}
    list of segments. By definition of [absolute-path] the list of
    segments is never empty.

    Here are a few examples:
    {ul
    {- [decode "/" = Ok [""]]}
    {- [decode "//" = Ok ["";""]]}
    {- [decode "//a" = Ok ["";"a"]]}
    {- [decode "/a/b/c" = Ok ["a";"b";"c"]]}
    {- [decode "/a/b//c" = Ok ["a";"b";"";"c"]]}
    {- [decode "/a/b/c/" = Ok ["a";"b";"c";""]]}
    {- [decode "/a/b/c/%20" = Ok ["a";"b";"c";" "]]}
    {- [decode "/a/b//c//" = Ok ["a";"b";"";"c";"";""]]}
    {- [decode "/a/b%2F/c" = Ok ["a"; "b/"; "c"]]}
    {- [decode "/r%C3%C9volte" = Ok ["r\xC3\xC9volte"]]}
    {- [decode  "/a/not%2520/b" = Ok ["a"; "not%20"; "b"]]}
    {- [decode "" = Error _]}
    {- [decode "a/b/c" = Error _]}} *)

val and_query_string_of_request_target :
  string -> (t * string option, string) result
(** [and_query_string_of_request_target s] parses a path and a query
    string (without the '?') form the request target [s] (which can be
    an URL). This returns [Ok (none, None)] on ["*"]. *)

(** {1:predicates Predicates and comparisons} *)

val is_none : t -> bool
(** [is_none p] is [true] iff [equal none p]. *)

val is_root : t -> bool
(** [is_root p] is [true] iff [equal root p]. *)

val equal : t -> t -> bool
(** [equal] tests paths for equality. Segments are tested using binary
    equality. *)

val compare : t -> t -> int
(** [compare] is a total order on paths compatible with {!equal}. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats paths for inspection. *)

val pp_dump : Format.formatter -> t -> unit
(** [pp_dump] formats paths for deeper inspection. *)
