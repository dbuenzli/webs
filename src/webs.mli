(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** HTTP interactions. Open to use it.

    Open the module to use it. It defines only these modules in your scope. *)

(** Media type constants and file extensions. *)
module Media_type : sig

  (** {1:media_types Media types} *)

  type t = string
  (** The type for
      {{:https://www.rfc-editor.org/rfc/rfc9110#name-media-type}media types}. *)

  (** {1:constants Constants} *)

  val none : t
  (** [none] is [""], a content type for when there is none. *)

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
      [map] defaults to {!default_file_ext_map}. *)

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
end

(** HTTP {{!Http.Request}requests} and {{!Http.Response}responses}.

    Along with a few codecs and protocol logic fragments.

    {b References.}
    {ul
    {- {{:https://www.rfc-editor.org/rfc/rfc9110}HTTP semantics} –
    {{:https://www.rfc-editor.org/rfc/rfc9111}HTTP caching} -
    {{:https://www.rfc-editor.org/rfc/rfc9112}HTTP/1.1} –
    {{:https://www.rfc-editor.org/rfc/rfc9113}HTTP/2} –
    {{:https://www.rfc-editor.org/rfc/rfc9114}HTTP/3}.}} *)
module Http : sig

  (** {1:codecs Base codecs and types} *)

  (** Percent-encoding codec.

      Percent-encoding codecs according to
      {{:https://www.rfc-editor.org/rfc/rfc3986#section-2.1}RFC 3986}.

      {b Note.} This should not be used for URI query strings and
      [application/x-www-form-urlencoded] which is slightly different.
      The {!Query} module handles that. *)
  module Pct : sig

    type kind = [
      | `Uri_component
      (**  Percent-encodes anything but
           {{:https://www.rfc-editor.org/rfc/rfc3986#section-2.3}
           [unreserved]} and
           {{:https://www.rfc-editor.org/rfc/rfc3986#section-2.2}
           sub-delims} URI characters. In other words only
           ['a'..'z'], ['A'..'Z'], ['0'..'9'], ['-'], ['.'], ['_'], ['~']
           and ['!'], ['$'], ['&'], ['\''], ['('], [')']
           ['*'], ['+'], [','], [';'], ['='] are not percent-encoded. *)
      | `Uri
      (** Percent-encodes like [`Uri_component] except it also
           preserves
           {{:https://www.rfc-editor.org/rfc/rfc3986#section-2.2}
           gen-delims} URI characters. In other words in addition to those
           characters above, [':'], ['/'], ['?'], ['#'], ['\['], ['\]'], ['@']
           are not percent-encoded. *)
      ]
    (** The kind of percent encoding. *)


    val encode : kind -> string -> string
    (** [encode kind s] is the percent encoding of [s] according to
        [kind]. *)

    val decode : string -> string
    (** [decode s] is the percent decoding of [s]. *)
  end

  (** Decimal digits codec.

      These represent non-negative integers. The module
      detects overflows and turns them into errors. *)
  module Digits : sig
    val decode : string -> (int, string) result
    (** [decode s] is the non-empty sequence of
        {{:https://www.rfc-editor.org/rfc/rfc5234#appendix-B.1}decimal digits}
        [s] as a non-negative integer. *)

    val encode : int -> string
    (** [encode_digits n] is the non-negative integer [n] as a sequence
        of decimal digits.

        @raise Invalid_argument if [n] is negative. *)
  end

  (** Versions. *)
  module Version : sig
    type t = int * int
    (** The type for
        {{:https://www.rfc-editor.org/rfc/rfc9112#name-http-version}
        HTTP versions}. Both integers must be in the interval [\[0;9\]]. *)

    val v11 : t
    (** [v11] is [(1, 1)]. *)

    val v20 : t
    (** [v20] is [(2, 0)]. *)

    val v30 : t
    (** [v30] is [(3, 0)]. *)

    (** {1:converting Converting} *)

    val decode : string -> (t, string) result
    (** [decode s] decodes a version from [s]. Single digit versions
        are also parsed, some serializations (e.g. the [curl] tool) do
        that. *)

    val encode : t -> string
    (** [encode v] encodes the version [v]. Assumes correct integer ranges. *)

    val pp : Format.formatter -> t -> unit
    (** [pp] formats versions for inspection. *)
  end

  (** Methods and method constraints. *)
  module Method : sig

    (** {1:methods Methods} *)

    type t =
    [ `GET
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#GET}[GET]} *)
    | `HEAD
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#HEAD}[HEAD]} *)
    | `POST
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#POST}[POST]} *)
    | `PUT
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#PUT}[PUT]} *)
    | `DELETE
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#DELETE}[DELETE]} *)
    | `CONNECT
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#CONNECT}[CONNECT]} *)
    | `OPTIONS
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#OPTIONS}[OPTIONS]} *)
    | `TRACE
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#TRACE}[TRACE]} *)
    | `PATCH
    (** {{:http://www.rfc-editor.org/rfc/rfc5789}[PATCH]} '*)
    | `Other of string
    (** Other {{:https://www.rfc-editor.org/rfc/rfc9110#name-tokens}token} *)
    ]
    (** The type for
        {{:https://www.rfc-editor.org/rfc/rfc9110#name-methods}
        request methods}. *)

    val decode : string -> (t, string) result
    (** [decode s] decodes a method from [s]. *)

    val encode : t -> string
    (** [encode m] encodes [m] to a method.

        @raise Invalid_argument if [m] is [`Other t] and [t] is not
        a {!Webs.Http.Headers.value_is_token}. *)

    val pp : Format.formatter -> t -> unit
    (** [pp] formats methods for inspection. *)

    (** {1:constraints Constraints} *)

    type 'a constraint' = t * 'a
    (** The type for constraining methods to ['a]. *)

    val constrain :
      allowed:'a constraint' list -> t -> ('a, 'a constraint' list) result
    (** [constrain ~allowed m] constrains [m] to [allowed]. This is [Ok m]
        if [m] is constrained by [allowed] and [Error allowed] otherwise. *)

    val connect : [> `CONNECT] constraint'
    (** [connect] adds [`CONNECT] to the constraint set. *)

    val delete : [> `DELETE] constraint'
    (** [delete] adds [`DELETE] to the constraint set. *)

    val get : [> `GET] constraint'
    (** [get] adds [`GET] to the constraint set. *)

    val head : [> `HEAD] constraint'
    (** [head] adds [`HEAD] to the constraint set. *)

    val options : [> `OPTIONS] constraint'
    (** [options] adds [`OPTIONS] to the constraint set. *)

    val other : string -> 'a ->  'a constraint'
    (** [other s v] adds a constraint for method [s] represented
        by [v] to the constraint set. *)

    val patch : [> `PATCH] constraint'
    (** [patch] adds [`PATCH] to the constraint set. *)

    val post : [> `POST] constraint'
    (** [post] adds [`POST] to the constraint set. *)

    val put : [> `PUT] constraint'
    (** [put] adds [`PUT] to the constraint set. *)

    val trace : [> `TRACE] constraint'
    (** [trace] adds [`TRACE] to the constraint set. *)
  end

  (** Absolute paths. *)
  module Path : sig

    (** {1:paths Absolute paths} *)

    type t = string list
    (** The type for {e absolute} URI paths represented as {e non-empty}
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
    (** [none] is the path [[]] for when there is none. *)

    val root : t
    (** [root] is the root path [[""]]. *)

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
        yields {!root} (see {{!Webs_bazaar.Kurl.root_paths}here} for
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
    (** [to_absolute_filepath p] is an absolute file path for
        {!undot_and_compress}[ p]. Errors if any of the path segments
        contains a stray slash or backslash or if [p] is the empty
        list. The result always uses [/] as a directory separator
        regardless of the platform and is guaranteed to be free of
        any [.] or [..] segments. *)

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
         [':'] or ['@'] to produce a valid URI
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
    (** [and_query_string_of_request_target s] parses a path and a
        query string (without the '?') form the request target [s]
        (which can be an URL). *)

    val pp : Format.formatter -> t -> unit
    (** [pp] formats paths for inspection. *)

    val pp_dump : Format.formatter -> t -> unit
    (** [pp_dump] formats paths for deeper inspection.. *)
  end

  (** Queries.

      A datatype and codecs to handle the quirky queries found in
      URIs and some media types. *)
  module Query : sig

    (** {1:queries Queries} *)

    type t
    (** The type for queries as key-values maps. Both keys and values
        are properly decoded. Note that keys can map to multiple
        values. *)

    val empty : t
    (** [empty] is the empty key-values map. *)

    val def : string -> string -> t -> t
    (** [def k v q] is [q] with [k] bound only to value [v]. See also
        {!add_value}. *)

    val undef : string -> t -> t
    (** [undef k q] is [q] with [k] unbound. *)

    val add_value : string -> string -> t -> t
    (** [add_value k v q] is [q] with [k] bound to [find_all k q @ [v]].
        See also {!def}. *)

    (** {1:lookups Lookups} *)

    val find_first : string -> t -> string option
    (** [find_first k q] is the value of [k]'s first binding in [q], if any. *)

    val find_all : string -> t -> string list
    (** [find_all k q] are all the values bound to [k] or the empty
        list if [k] is unbound. *)

    val fold : (string -> string -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold f q acc] folds over all the key-value bindings. For keys
        with multiple values folds over them in the same order
        as given by {!find_all}. *)

    (** {1:predicates Predicates} *)

    val is_empty : t -> bool
    (** [is_empty q] is true if [q] is {!empty}. *)

    val mem : string -> t -> bool
    (** [mem k q] is true [iff] key [k] is bound in [q]. *)

    (** {1:conv Converting} *)

    val decode : string -> t
    (** [decode s] decodes the
        {{:https://url.spec.whatwg.org/#application/x-www-form-urlencoded}
        [application/x-www-form-urlencoded]} [s] to a query. If a key
        is defined more than once, the first definition is returned by
        {!find_first} and the left-to-right order preserved by
        {!find_all}'s list. The input string is not checked for UTF-8
        validity. *)

    val encode : t -> string
    (** [encode q] encodes [q] to an
        {{:https://url.spec.whatwg.org/#application/x-www-form-urlencoded}
        [application/x-www-form-urlencoded]}
        string. *)

    val pp : Format.formatter -> t -> unit
    (** [pp] formats queries for inspection. *)
  end

  (** HTTP schemes. *)
  module Scheme : sig
    type t =
      [ `Http
      (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-http-uri-scheme}
         http URI scheme}. *)
      | `Https
      (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-https-uri-scheme}
          https URI schemes}. *) ]
    (** The type for
        {{:https://www.rfc-editor.org/rfc/rfc9110#name-http-related-uri-schemes}
        HTTP URI schemes}. *)

    val encode : t -> string
    (** [encode s] encoes [s] as a lowercase US-ASCII token. *)

    val tcp_port : t -> int
    (** [tcp_port s] is [80] for [`Http] and 443 for [`Https]. *)

    val pp : Format.formatter -> t -> unit
    (** [pp] formats schemes for inspection. *)
  end

  (** Status codes. *)
  module Status : sig
    type t = int
    (** The type for
        {{:https://www.rfc-editor.org/rfc/rfc9110#name-status-codes}
        status codes}. *)

    val reason_phrase : t -> string
    (** [reason_phrase s] is [s]'s reason phrase. *)

    val pp : Format.formatter -> t -> unit
    (** [pp] formats status codes for inspection. *)

    (** {1:predef Predefined status codes} *)

    (** {2:informational Informational 1xx} *)

    val continue_100 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-100-continue}[100]} *)

    val switching_protocols_101 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-101-switching-protocols}
        [101]} *)

    (** {2:sucessful Sucessful 2xx} *)

    val ok_200 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-200-ok}[200]} *)

    val created_201 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-201-created}[201]} *)

    val accepted_202 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-202-accepted}[202]} *)

    val non_authoritative_information_203 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-203-non-authoritative-infor}[203]} *)

    val no_content_204 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-204-no-content}[204]} *)

    val reset_content_205 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-205-reset-content}
        [205]} *)

    val partial_content_206 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-206-partial-content}
        [206]} *)

    (** {2:redirection Redirection 3xx} *)

    val multiple_choices_300 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-300-multiple-choices}
        [300]} *)

    val moved_permanently_301 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-301-moved-permanently}
        [301]} *)

    val found_302 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-302-found}[302]} *)

    val see_other_303 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-303-see-other}[303]} *)

    val not_modified_304 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-304-not-modified}
        [304]} *)

    val use_proxy_305 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-305-use-proxy}[305]} *)

    val temporary_redirect_307 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-307-temporary-redirect}
        [307]} *)

    val permanent_redirect_308 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-308-temporary-redirect}
        [308]} *)

    (** {2:client_error Client Error 4xx} *)

    val bad_request_400 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-400-bad-request}[400]} *)

    val unauthorized_401 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-401-unauthorized}
        [401]} *)

    val payement_required_402 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-402-payment-required}
        [402]} *)

    val forbidden_403 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-403-forbidden}[403]} *)

    val not_found_404 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-404-not-found}[404]} *)

    val method_not_allowed_405 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-405-method-not-allowed}
        [405]} *)

    val not_acceptable_406 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-406-not-acceptable}
        [406]} *)

    val proxy_authentication_required_407 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-407-proxy-authentication-re}[407]} *)

    val request_time_out_408 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-408-request-timeout}
        [408]} *)

    val conflict_409 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-409-conflict}[409]} *)

    val gone_410 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-410-gone}[410]} *)

    val length_required_411 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-411-length-required}
        [411]} *)

    val precondition_failed_412 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-412-precondition-failed}
        [412]} *)

    val content_too_large_413 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-413-content-too-large}
        [413]} *)

    val uri_too_long_414 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-414-uri-too-long}
        [414]} *)

    val unsupported_media_type_415 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-415-unsupported-media-type}[415]} *)

    val range_not_satisfiable_416 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-416-range-not-satisfiable}[416]} *)

    val expectation_failed_417 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-417-expectation-failed}
        [417]} *)

    val i'm_a_teapot_418 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc2324#section-2.3.2}[418]} *)

    val upgrade_required_426 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-426-upgrade-required}
        [436]} *)

    (** {2:server_error Server Error 5xx} *)

    val server_error_500 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-500-internal-server-error}[500]} *)

    val not_implemented_501 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-501-not-implemented}
        [501]} *)

    val bad_gateway_502 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-502-bad-gateway}[502]} *)

    val service_unavailable_503 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-503-service-unavailable}
        [503]} *)

    val gateway_time_out_504 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-504-gateway-timeout}
        [504]} *)

    val http_version_not_supported_505 : t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-505-http-version-not-suppor}[505]} *)
  end

  (** Message bodies. *)
  module Body : sig

    open Bytesrw

    (** {1:byte_rw Bytes writers} *)

    type byte_writer = (bytes * int * int) option -> unit
    (** The type for writing bytes. Push control flow.

        {ul
        {-  The function should be called with [Some (byte, first, length)]
            to write the [bytes] from [first] to [first+length]. These
            bytes should not be modified until the function returns
            and the function must only read the given [bytes].}
        {-  The function gets called with [None] to signal that there is no
            longer any bytes to write.}} *)

    (** {1:contents Body contents} *)

    type 'a writer = 'a -> unit
    (** The type for body writers. These are functions defining bodies
        by writing them on the structure ['a] given to them. Typically
        for service responses the connector provides a structure to write
        on. *)

    type custom_content = ..
    (** The type for custom body contents.

        This allows bodies to expose connector specific readable or
        writable representations. For example the
        {!Webs_unix.Fd.Writer} custom content defines a body content
        by a function that writes directly on an output file
        descriptor provided by the connector. *)

    type content =
    | Empty (** Empty body. *)
    | Bytes_reader of Bytes.Reader.t (** Bytes reader, pulls bytes. *)
    | Byte_writer of byte_writer writer (** Function that pushes bytes. *)
    | Custom of custom_content (** Custom content. *)
    (** The type for body contents. *)

    (** {1:bodies Bodies} *)

    type t
    (** The type for bodies. *)

    val make :
      ?content_length:int -> ?content_type:Media_type.t ->
      content -> t
    (** [make c] is a body with content [c] and
        {ul
        {- [content_type] the content type. Defaults to
           {!Media_type.application_octet_stream}.}
        {- [content_length] the content length in bytes, if known.
           Defaults to [None]}}

        Raises [Invalid_argument] if [content_length] is negative. *)

    val empty : t
    (** [empty s] is a body with {!Empty} content. The {!content_type} is
        {!Media_type.none}, the {!content_length} is [0] and {!close}
        is a nop. *)

    val of_custom_content :
      ?content_length:int -> ?content_type:Media_type.t -> custom_content -> t
    (** [of_custom_content c] is a body defined by the custom content [c]. *)

    val of_byte_writer :
      ?content_length:int -> ?content_type:Media_type.t ->
      byte_writer writer -> t
    (** [of_byte_writer w] is a body written by [w]. *)

    val of_bytes_reader :
      ?content_length:int -> ?content_type:Media_type.t -> Bytes.Reader.t -> t
    (** [of_byte_reader b] is a body from the given byte reader. *)

    val of_string : ?content_type:Media_type.t -> string -> t
    (** [of_string s] is a body made of string [s] (uses a
        {!Byte_writer}). {!content_length} is set to the length of [s]. *)

    val to_bytes_reader : t -> (Bytes.Reader.t, string) result
    (** [to_bytes_reader b] is a bytes reader on the inbound body [b].
        This errors on {!Custom} content. It works on {!Byte_writer}s
        but entails a full copy in memory. *)

    val to_string : t -> (string, string) result
    (** [to_string b] reads the body to a string. This errors on
        {!Custom} content. *)

    val pp : Format.formatter -> t -> unit
    (** [pp] formats bodies for inspection. It guarantees not to
        touch the content. *)

    (** {1:operations Operations} *)
(*
    val close : t -> unit -> unit
    (** [close b] is the function to close the body. *)
*)
    (** {1:predicates Predicates} *)

    val is_empty : t -> bool
    (** [is_empty b] is [true] iff [content b] is {!Empty}. Note that
        this does not rule out a writer that doesn't write any data. *)

    (** {1:properties Properties} *)

    val content : t -> content
    (** [content b] is the content of [b]. *)

    val content_type : t -> Media_type.t
    (** [content_type b] is the media type of [b]. *)

    val content_length : t -> int option
    (** [content_length b] is the content length of [b], if known. *)
  end

  (** {1:headers Headers} *)

  (** Headers.

      A datatype to handle the quirky HTTP headers. *)
  module Headers : sig

    (** {1:header_field_names Header names} *)

    (** Header names.

        HTTP header names are US-ASCII case insensitive. Values of type
        {!Name.t} represent US-ASCII lowercased HTTP
        {{:https://www.rfc-editor.org/rfc/rfc9110#name-tokens}tokens}. *)
    module Name : sig

      type t = private string
      (** The type for lowercased HTTP header field
          {{:https://www.rfc-editor.org/rfc/rfc9110#name-field-names}
          field-name}s. *)

      val v : string -> t
      (** [v s] is a name from [s]. Raises [Invalid_argument] if
          [s] is not a header name.  Use {!decode} if you
          need to handle failures. *)

      val equal : t -> t -> bool
      (** [equal n n'] is [true] iff [n] and [n'] are equal. *)

      val compare : t -> t -> int
      (** [compare] is {!String.compare}. *)

      val decode : string -> (t, string) result
      (** [decode s] decodes a name from [s]. *)

      val encode : t -> string
      (** [encode n] encodes a name for [s]. *)

      val pp : Format.formatter -> t -> unit
      (** [pp] formats header names for inspection. *)
    end

    val name : string -> Name.t
    (** [name n] is {!Name.v}. *)

    (** {1:headers Headers} *)

    type t
    (** The type for HTTP headers. Maps header names to string values
        such that for:
        {ul
        {- Single valued headers, the string is the value.}
        {- Multi-valued headers, the string is the values separated by
          commas [',']. Use {!Headers.values_of_string} on the string.}
        {- The {!set_cookie} header, must be treated specially since it
           can be repeated but does not follow the syntax of
           multi-valued headers. The values are stored in the string
           separated by ['\x00'] values. Use {!Headers.add_set_cookie} and
           {!Headers.values_of_set_cookie_value} to handle the field. Encoders
           must write the cookies in separate {!set_cookie} headers.}} *)

    val empty : t
    (** [empty] has no header definition. *)

    val def : Name.t -> string -> t -> t
    (** [def n v hs] is [hs] with [n] defined to [v]. *)

    val def_if_some : Name.t -> string option -> t -> t
    (** [def_some n o hs] is [hs] with [n] defined to [v] if [o]
        is [Some v] and [hs] otherwise. *)

    val def_if_undef : Name.t -> string -> t -> t
    (** [def_if_undef n v hs] is [hs] with [n] defined to [v] if [n] is
        not defined in [hs]. *)

    val undef : Name.t -> t -> t
    (** [undef n hs] is [hs] with [n] undefined. *)

    val add_value : Name.t -> string -> t -> t
    (** [add_value n v hs] appends [v] to the multi-valued header [n] in
        [hs]. *)

    val add_set_cookie : string -> t -> t
    (** [add_set_cookie c hs] adds a {!set_cookie} header with value [c].
        This appends to {!set_cookie}, see {!t}. *)

    val override : t -> by:t -> t
    (** [override hs ~by] are the headers of both [hs] and [by]
        with those of [by] taking over. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf hs] prints an unspecified representation of [hs]
        on [ppf]. *)

    val encode_http11 : t -> string
    (** [encode_http11 hs] is [hs] as sequence of crlf terminated
        HTTP/1.1 headers. This correctly handles the
        {!Headers.set_cookie} header. *)

    (** {1:lookups Lookups} *)

    val find : ?lowervalue:bool -> Name.t -> t -> string option
    (** [find n hs] is the value of [n] in [hs] (if any).
        If [lowervalue] is [true] (defaults to [false])
        the US-ASCII uppercase letter are mapped on lowercase.

        If [n] is a multi-valued header use {!values_of_string} on
        the result. If [n] is {!set_cookie} you must use
        {!values_of_set_cookie_value}. *)

    val find' : ?lowervalue:bool -> Name.t -> t -> (string, string) result
    (** [find'] is like {!find}. Except if the header is absent it
        returns an error message of the form ["%s: No such header"]. *)

    val get : ?lowervalue:bool -> Name.t -> t -> string
    (** [get n hs] is like {!find} but raises [Invalid_argument] if [n]
        is not defined in [hs]. *)

    val fold : (Name.t -> string -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold f m acc] folds [f] over the bindings of [hs] starting with
        [acc]. *)

    (** {2:lookup_header_specific Header specific} *)

    val request_body_length : t ->
      ([ `Length of int | `Chunked ], string) result
    (** [request_body_length hs] determines the message body length of
        a request (the rules for responses is a bit different) as per
        {{:https://www.rfc-editor.org/rfc/rfc9112#name-message-body-length}
        HTTP/1.1 specification}, by looking at the {!content_type} and
        {!transfer_encoding} in [hs]. *)

    val decode_host : Scheme.t -> t -> (string * int, string) result
    (** [decode_host scheme r] decodes the {!host} header into a
        hostname and a port number. If no port number is found in the
        header one is derived from [scheme] with {!Scheme.tcp_port}.
        Errors if the header is missing or on decoding errors. *)

    val for_connector : t -> Body.t -> t
    (** [for_connector hs body] are the headers of [hs] prepared for
        output a connector that will write a request or response with
        body [body]. It performs the logic described in the
        {{!page-connector_conventions.service_responses}service responses}
        and
        {{!page-connector_conventions.service_responses}client requests}
        conventions. *)

    (** {1:values Header values} *)

    val values_of_set_cookie_value : string -> string list
    (** [values_of_set_cookie_value v] decodes [v] as stored in
        by {!add_set_cookie} in the {!t} type to a list of cookies. *)

    val values_of_string : ?sep:char -> string -> string list
    (** [values_of_string s] splits the string [s] at [','] (or [sep]
        if specified) characters and trims the resulting strings from
        {{:https://www.rfc-editor.org/rfc/rfc9110#name-whitespace} optional
        whitespace}, and lowercases the result if [lowercase] is [true].

        Note that by definition the result is never the
        empty list, the function returns [[""]] on [""]. *)

    val values_to_string : ?sep:char -> string list -> string
    (** [values_to_string vs] is [String.concat "," vs] but
        raise [Invalid_argument] if [vs] is [[]]. TODO why ? *)

    val value_is_token : string -> bool
    (** [value_is_token s] is [true] iff [s] in an HTTP
        a {{:https://www.rfc-editor.org/rfc/rfc9110#name-tokens}token}. *)

    val decode_http11_header : string -> (Name.t * string, string) result
    (** [decode_http11_header s] decodes a header from [s]. *)

    val pp_header : Format.formatter -> (Name.t * string) -> unit
    (** [pp_header] formats a header for inspection with HTTP/1.1 syntax. *)

    (** {1:predicates Predicates} *)

    val is_empty : t -> bool
    (** [is_empty hs] is [true] iff [hs] is has no definition. *)

    val mem : Name.t -> t -> bool
    (** [mem n hs] is [true] iff [n] is defined in [hs]. *)

    (** {1:standard_header_names Standard header names} *)

    val accept : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-accept}[accept]} *)

    val accept_charset : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-accept-charset}
        [accept-charset]} *)

    val accept_encoding : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-accept-encoding}
        [accept-encoding]} *)

    val accept_language : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-accept-language}
        [accept-language]} *)

    val accept_ranges : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-accept-ranges}
        [accept-ranges]} *)

    val age : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9111#name-age}[age]} *)

    val allow : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-allow}[allow]} *)

    val authorization : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-authorization}
        [authorization]} *)

    val cache_control : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9111#name-cache-control}
        [cache-control]} *)

    val connection : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-connection}
        [connection]} *)

    val content_disposition : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc6266}[content-disposition]} *)

    val content_encoding : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-content-encoding}
        [content-encoding]}*)

    val content_language : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-content-language}
        [content-language]}*)

    val content_length : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-content-length}
        [content-length]} *)

    val content_location : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-content-location}
        [content-location]} *)

    val content_range : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-content-range}
        [content-range]} *)

    val content_type : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-content-type}
        [content-type]} *)

    val cookie : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc6265#section-4.2}
        [cookie]} *)

    val date : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-date}[date]} *)

    val etag : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-etag}[etag]} *)

    val expect : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-expect}[expect]} *)

    val expires : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9111#name-expires}[expires]} *)

    val from : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-from}[from]} *)

    val host : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-host-and-authority}
        [host]} *)

    val if_match : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-if-match}[if-match]} *)

    val if_modified_since : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-if-modified-since}
        [if-modified-since]} *)

    val if_none_match : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-if-none-match}
        [if-none-match]} *)

    val if_range : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-if-range}[if-range]} *)

    val if_unmodified_since : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-if-unmodified-since}
        [if-unmodified-since]} *)

    val last_modified : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-last-modified}
        [last-modified]} *)

    val location : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-location}[location]} *)

    val max_forwards : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-max-forwards}
        [max-forwards]} *)

    val origin : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc6454#section-7}[origin]}. *)

    val pragma : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9111#name-pragma}[pragma]} *)

    val proxy_authenticate : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-proxy-authenticate}
        [proxy-authenticate]} *)

    val proxy_authorization : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-proxy-authorization}
        [proxy-authorization]} *)

    val range : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-range}[range]} *)

    val referer : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-referer}[referer]} *)

    val retry_after : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-retry-after}
        [retry-after]} *)

    val server : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-server}[server]} *)

    val set_cookie : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc6265#section-4.1}
        [set-cookie]} *)

    val te : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-te}[te]} *)

    val trailer : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-trailer}[trailer]} *)

    val transfer_encoding : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9112#name-transfer-encoding}
        [transfer-encoding]} *)

    val upgrade : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-upgrade}[upgrade]} *)

    val user_agent : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-user-agent}
        [user-agent]} *)

    val vary : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-vary}[vary]} *)

    val via : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-via}[via]} *)

    val warning : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9111#name-warning}[warning]} *)

    val www_authenticate : Name.t
    (** {{:https://www.rfc-editor.org/rfc/rfc9110#name-www-authenticate}
        [www-authenticate]} *)
  end

  (** Cookies.

      Support for {{:https://www.rfc-editor.org/rfc/rfc6265}cookies}. *)
  module Cookie : sig

    type name = string
    (** The type for cookie names. *)

    type attributes
    (** The type for {{:https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie#attributes}cookie attributes}. *)

    val default_attributes : attributes
    (** [default_attributes] are cookie attributes with [secure] set
        to [true], [http_only] set to [true], [same_site] set to
        ["strict"] and no other attribute specified. *)

    val attributes :
      ?init:attributes ->
      ?domain:string option -> ?http_only:bool -> ?max_age:int option ->
      ?path:Path.t -> ?same_site:string -> ?secure:bool -> unit -> attributes
    (** [atts ()] are the given {{:https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie#attributes}cookie attributes}. Those unspecified take
        the value of [init] which defaults to {!default_attributes}. *)

    (** {1:converting Converting} *)

    val encode : ?attributes:attributes -> name:name -> string -> string
    (** [encodes ~atts name value] encodes a cookie named [name] with
        value [value] and attributes [atts] (defaults to
        {!default_attributes}) for {!Headers.add_set_cookie}. *)

    val decode_list : string -> ((name * string) list, string) result
    (** [decode_list s] parses the
        {{:https://www.rfc-editor.org/rfc/rfc6265#section-4.2.1}cookie string}
        of a {!Headers.cookie} header value. *)
  end

  (** Entity tags.

      Support for
      {{:https://www.rfc-editor.org/rfc/rfc9110#name-conditional-requests}
      conditional requests}. *)
  module Etag : sig

    (** {1:etags Etags} *)

    type t
    (** The type for
        {{:https://www.rfc-editor.org/rfc/rfc9110#name-etag}etags}. *)

    val make : weak:bool -> string -> t
    (** [make ~weak tag] is the etag [tag]. [weak] indicates if the etag is
        weak.

        {b Warning.}  The function does not check that the bytes of
        [tag] are valid; each should be one of [0x21], \[[0x23];[0x7E]\]
        or \[[0x80];[0xFF]\]. *)

    val is_weak : t -> bool
    (** [is_weak e] is [true] iff [e] is weak. *)

    val tag : t -> string
    (** [tag e] is the entity tag of [e]. *)

    val weak_match : t -> t -> bool
    (** [weak_match e0 e1] is [true] iff [e0] and [e1]
        {{:https://www.rfc-editor.org/rfc/rfc9110#name-comparison-2}
        weakly match}. *)

    val strong_match : t -> t -> bool
    (** [strong_match e0 e1] is [true] iff [e0] and [e1]
        {{:https://www.rfc-editor.org/rfc/rfc9110#name-comparison-2}
        strongly match}. *)

    val decode : string -> (t, string) result
    (** [decode s] is an
        {{:https://www.rfc-editor.org/rfc/rfc9110#name-etag}etags} from [s]. *)

    val encode : t -> string
    (** [encode etag] is [etag] as an etag. *)

    (** {1:conds Etag conditions} *)

    type cond = [ `Any | `Etags of t list (** *) ]
    (** The type for etags conditions. This represents the value
        of {!Http.Headers.if_match} or {!Http.Headers.if_none_match} headers. *)

    val decode_cond : string -> (cond, string) result
    (** [decode_cond s] parses an etag condition from [s]. *)

    val encode_cond : cond -> string
    (** [encode_cond c] serializes condition [c]. *)

    val eval_if_match : cond -> t option -> bool
    (** [eval_if_match c t] evaluates the logic of an
        {!Http.Headers.if_match} header condition [c] on an entity represented
        by [t] ([None] means the representation does not exist). This is:
        {ul
        {- [true] if [c] is [None] (no condition).}
        {- [true] if [t] is [Some _] and [c] is [Some `Any].}
        {- [true] if [t] is [Some etag], [c] is [Some (`Etags etags)] and
           [etag] {{!strong_match}strongly matches} one of the [etags].}
        {- [false] otherwise.}} *)

    val eval_if_none_match : cond -> t option -> bool
    (** [eval_if_none_match c t] evaluates the logic of an
        {!Http.Headers.if_none_match} header condition [c] on an entity
        represented by [t] ([None] means the representation does not exist).
        This is:
        {ul
        {- [true] if [t] is [None] and [c] is [Some `Any].}
        {- [true] if [t] is [Some etag], [c] is [Some (`Etags etags)] and
           [etag] {{!weak_match}weakly matches} none of the [etags]}
        {- [false] otherwise.}} *)

    val eval_if_range : t -> t option -> bool
    (** [eval_if_range req t] evaluates the logic of an
        {!Http.Headers.if_range} header
        etag [req] on an entity represented by [t] ([None] means the
        representation does not exist). This is:
        {ul
        {- [true] if [t] is [Some etag] and [etag] {{!strong_match}strongly
            matches} [req]}
        {- [false] otherwise.}} *)
  end

  (** Range requests.

      Support for {{:https://www.rfc-editor.org/rfc/rfc9110#name-range-requests}
      range requests}. *)
  module Range : sig

    (** {1:bytes Byte ranges} *)

    type bytes =
    [ `First of int (** First given offset to last offset *)
    | `Last of int (** At most last given [n] bytes. *)
    | `Range of int * int (** First offset and last offset. *) ]
    (** The type for byte range specifications. Offsets are zero-based. *)

    val eval_bytes : length:int -> bytes -> (int * int) option
    (** [eval_bytes ~length b] given a representation length [len] and
        byte range [b] returns a concrete zero-based byte range or
        [None] if the range cannot be satisfied for [length].  *)

    (** {1:ranges Ranges} *)

    type t =
    [ `Bytes of bytes list (** Byte ranges. *)
    | `Other of string * string (** Range unit and value. *) ]
    (** The type for ranges. *)

    val decode : string -> (t, string) result
    (** [decode s] decodes a
        {{:https://www.rfc-editor.org/rfc/rfc9110#name-range}range} header
        value. *)

    val encode : t -> string
    (** [encode r] serializes ranges [r] in unit [u].
        It's the client duty to make sure ranges are valid. *)
  end

  (** {1:req_resp Responses and requests} *)

  (** HTTP responses.

      The {{!page-connector_conventions.service_responses}service
      responses} and
      {{!page-connector_conventions.client_responses}client responses}
      conventions may help understanding how connectors use and
      construct these values. *)
  module Response : sig

    (** {1:responses Responses} *)

    type t
    (** The type for HTTP responses. *)

    val make :
      ?headers:Headers.t -> ?log:string -> ?reason:string ->
      ?version:Version.t -> Status.t -> Body.t -> t
    (** [make status body] is a response with given [status] and [body] and:
        {ul
        {- [headers], the response headers. Defaults to {!Http.Headers.empty}.
           Note that in general it is better to let bodies define the
           content type and content length headers. See the
           {{!page-connector_conventions.service_responses}service
           response conventions}.}
        {- [log], see {!log}. Defaults to [""].}
        {- [reason], the status reason phrase.
           Defaults to {!Http.Status.reason_phrase}[ status].}
        {- [version], the HTTP version, see {!version}. Defaults to
           {!Version.v11}.}} *)

    val empty :
      ?headers:Headers.t -> ?log:string -> ?reason:string -> Status.t -> t
    (** [empty status] is {!make}[ status Body.empty]. *)

    val is_empty : t -> bool
    (** [is_empty response] is {!Body.is_empty}[ (body response)]. *)

    val with_body : Body.t -> t -> t
    (** [with_body b response] is [response] with body [b]. *)

    val with_log : string -> t -> t
    (** [with_log response] is [response] with log [log]. *)

    val with_headers : Headers.t -> t -> t
    (** [with_headers hs response] is [response] with headers [hs]. *)

    val override_headers : by:Headers.t -> t -> t
    (** [override_headers ~by response] is [response] with headers
        {!Headers.override}[ (headers response) ~by]. *)

    val with_status : ?log:string -> ?reason:string -> Status.t -> t -> t
    (** [with_status status response] is [response] with status [status], reason
        phrase [reason] (defaults to {!Http.Status.reason_phrase}[
        status], use [reason response] to keep the previous reason) and
        log [log] (defaults to [log response]). *)

    val pp : Format.formatter -> t -> unit
    (** [pp] formats responses for inspection. Guarantees not consume
        the {!val-body}. *)

    val encode_http11 : include_body:bool -> t -> (string, string) result
    (** [encode_http11 ~include_body response] encodes [response] to an
        HTTP/1.1 response. If [include_body] is [true] the body is consumed
        and included in the result. If [false] the body is left untouched
        and the encoding stops after the header final double CRLF. *)

    (** {1:properties Properties} *)

    val body : t -> Body.t
    (** [body response] is the body of [response]. *)

    val log : t -> string
    (** [log response] is the log of [response]. The log is a
        server-side {!reason} {b not meant to be sent to the
        client}. It can be used to log further details or explanations
        about the answer that one may not want to disclose to the
        client.  *)

    val headers : t -> Headers.t
    (** [headers response] are the headers of [response]. *)

    val reason : t -> string
    (** [reason response] is the reason phrase of [response]. *)

    val status : t -> Status.t
    (** [status response] is the
        {{:https://www.rfc-editor.org/rfc/rfc9110#name-status-codes}status}
        of [response]. *)

    val version : t -> Version.t
    (** [version response] is the version of [response].
        {ul
        {- For service connectors this is mostly irrelevant: the connector
           decides how it wants to send the response to the client. But it
           can be used as a hint for which HTTP version to use.}
        {- For client connectors this should be the HTTP version of the
           response read by the connector.}} *)

    (** {1:responding Responding}

        See also {{!Webs.Http.Request.deconstruct}request deconstruction}
        combinators. *)

    (** {2:simple Simple content} *)

    val content :
      ?content_type:Media_type.t ->  ?headers:Headers.t -> ?log:string ->
      ?reason:string -> Status.t -> string -> t
    (** [content status s] is
        {!make}[ status (]{!Body.of_string}[ ?content_type s)]. *)

    val text :
      ?headers:Headers.t -> ?log:string -> ?reason:string -> Status.t ->
      string -> t
    (** [text] responds with UTF-8 encoded plain text:
        {!content} with {!Media_type.text_plain}. *)

    val html :
      ?headers:Headers.t -> ?log:string -> ?reason:string -> Status.t ->
      string -> t
    (** [html] responds with UTF-8 encoded HTML text:
        {!content} with {!Media_type.text_html}.  *)

    val json :
      ?headers:Headers.t -> ?log:string -> ?reason:string -> Status.t ->
      string -> t
    (** [json] responds with JSON text: {!content} with
        {!Media_type.application_json}. *)

    (** {2:redirections Redirections} *)

    val redirect :
      ?body:Body.t -> ?headers:Headers.t ->
      ?log:string -> ?reason:string -> Status.t -> string -> t
    (** [redirect status loc] is a response with status [status]
        and {!Http.Headers.location} set to [loc] on headers.
        [body] defaults to {!Body.empty}.

        See also {!Request.redirect_to_path}.

        {b Warning.} It is your duty to properly percent-encode [loc]
        using for example {!Pct} or {!Path.encode}. *)

    (** {2:client_errors Client errors} *)

    val bad_request_400 :
      ?body:Body.t -> ?headers:Headers.t -> ?log:string ->
      ?reason:string -> unit -> ('a, t) result
    (** [bad_request_400 ()] is [Error r] with [r] a response with status
        {!Status.bad_request_400}. [body] defaults to {!Body.empty}. *)

    val unauthorized_401 :
      ?body:Body.t -> ?headers:Headers.t -> ?log:string ->
      ?reason:string -> unit -> ('a, t) result
    (** [unauthorized_401 ()] is [Error r] with [r] a response with status
        {!Status.unauthorized_401}. [body] defaults to {!Body.empty}. *)

    val forbidden_403 :
      ?body:Body.t -> ?headers:Headers.t -> ?log:string ->
      ?reason:string -> unit -> ('a, t) result
    (** [forbidden_403 ()] is [Error r] with [r] a response with status
        {!Status.forbidden_403}. [body] defaults to {!Body.empty}. *)

    val not_found_404 :
      ?body:Body.t -> ?headers:Headers.t -> ?log:string ->
      ?reason:string -> unit -> ('a, t) result
    (** [not_found_404 ()] is [Error r] with [r] a response with status
        {!Status.not_found_404}. [body] defaults to {!Body.empty}. *)

    val method_not_allowed_405 :
      ?body:Body.t -> ?headers:Headers.t -> ?log:string ->
      ?reason:string -> allowed:Method.t list -> unit -> ('a, t) result
    (** [method_not_allowed_450 ~allowed ()] is [Error r] with [r] a
        response with status {!Status.method_not_allowed_405} and
        {!Http.Headers.allow} set on [headers] with the [allowed]
        methods (which can be empty). [body] defaults to
        {!Body.empty}. *)

    val gone_410 :
      ?body:Body.t -> ?headers:Headers.t -> ?log:string ->
      ?reason:string -> unit -> ('a, t) result
    (** [gone_410 ()] is [Error r] with [r] a response with
        status {!Status.gone_410}. [body] defaults to {!Body.empty}. *)

    (** {2:server_errors Server errors} *)

    val server_error_500 :
      ?body:Body.t -> ?headers:Headers.t -> ?log:string ->
      ?reason:string -> unit -> ('a, t) result
    (** [server_error_500 ()] is [Error r] with [r] a response with status
        {!Status.server_error_500}. [body] defaults to {!Body.empty}. *)

    val not_implemented_501 :
      ?body:Body.t -> ?headers:Headers.t -> ?log:string ->
      ?reason:string -> unit -> ('a, t) result
    (** [not_implemented_501 ()] is [Error r] with [r] a response with
        status {!Status.not_implemented_501}. [body] defaults to
        {!Body.empty}. *)

    val service_unavailable_503 :
      ?body:Body.t -> ?headers:Headers.t -> ?log:string ->
      ?reason:string -> unit -> ('a, t) result
    (** [service_unavailable_503 ()] is [Error r] with [r] a response with
        status {!Status.service_unavailable_503}. [body] defaults to
        {!Body.empty}. *)

    (** {2:error_map Error handling} *)

    val result : ('a, 'a) result -> 'a
    (** [result r] is [Result.fold ~ok:Fun.id ~error:Fun.id]. It retracts
        the result type. *)

    val map_errors : only_empty:bool -> (t -> t) -> t -> t
    (** [map_errors ~only_empty f response] maps reponse [response] with
        [f] if [r]'s status is a 4XX or 5XX. If [only_empty] is [true] it
        does so only whenever {!is_empty}[ response] is [true].

        The idea of {!map_errors} is that service and service building blocks
        define their errors as responses with empty bodies. This function
        is then called just before handing over the reponse to the
        connector to define a page content for [response] with
        {!with_body}. *)
  end

  (** HTTP requests.

      The {{!page-connector_conventions.service_requests}service request}
      and {{!page-connector_conventions.client_requests}client request}
      conventions may help understanding how connectors construct and
      use these values. *)
  module Request : sig

    (** {1:requests Requests} *)

    type t
    (** The type for HTTP requests. *)

    val make :
      ?headers:Headers.t -> ?log:string -> ?path:Path.t ->
      ?query:string option -> ?scheme:Scheme.t -> ?service_path:Path.t ->
      version:Version.t -> Method.t -> raw_path:string -> Body.t -> t
    (** [make method' ~raw_path body] is a request with given [method'],
        [raw_path] and [body] and:

        {ul
        {- [headers], the request headers. Defaults to {!Http.Headers.empty}.
           Note that in general it is better to let bodies define the
           content type and content length headers. See the
           {{!page-connector_conventions.client_requests}client
           requests conventions}.}
        {- [log], see {!log}. Defaults to [""].}
        {- [path], see {!path}. Defaults to {!Path.none}.}
        {- [query], see {!query}. Defaults to [None].}
        {- [scheme] is the scheme of the request, see {!scheme}.
           Defaults to [`Https].}
        {- [service_path], see {!service_path}. Defaults to {!Path.root}.}
        {- [version], the HTTP version, see {!version}.}}

        Usually you should rather use {!for_service_connector}
        or {!of_url}. This ensures that derived data like {!path} and {!query}
        are computed correctly. *)

    val for_service_connector :
      ?log:string -> ?scheme:Scheme.t -> service_path:Path.t ->
      version:Version.t -> Method.t -> raw_path:string -> headers:Headers.t ->
      Body.t -> (t, Response.t) result
    (** [for_service_connector ~service_path ~version method'
        ~raw_path ~headers body] is a request that satisfies the
        {{!page-connector_conventions.service_requests} service
        requests conventions}.

        In case the response cannot satisfy them an error
        response is returned according to the
        {{!page-connector_conventions.service_connector_responses}
        connector responses conventions}. *)

    val of_url :
      ?body:Body.t -> ?headers:Headers.t -> ?log:string ->
      ?version:Version.t -> Method.t -> url:Webs_url.t -> (t, string) result
    (** [of_url method' ~url body] is a scheme and [method'] request
        on [url] ensuring that the request satsifies the
        {{!page-connector_conventions.client_requests}client request
        conventions}.

        [headers] defaults to {!Headers.empty} and a suitable
        {!Headers.host} is added to it. [body] defaults to
        {!Body.empty}, {!version} defaults to {!Version.v11}.

        An error is returned if the scheme is neither [http] or
        [https] or if a decoding error occurs. Both {!path} and
        {!query} are derived with a {!service_path} of {!Path.root}. *)

    val to_url : t -> (Webs_url.t, string) result
    (** [to_url request] is an URL for [r] of the given
        scheme. This can be seen as the inverse of {!of_url}. This errors
        if no {!Headers.host} header can be found in the request header. *)

    val to_url' : t -> Webs_url.t
    (** [to_url'] is like {!to_url} but raises [Invalid_argument] if
        there is no {!Headers.host} header. *)

    val with_body : Body.t -> t -> t
    (** [with_body b request]  is [request] with body [b]. *)

    val with_headers : Headers.t -> t -> t
    (** [with_headers hs request] is [request] with headers [hs]. *)

    val override_headers : by:Headers.t -> t -> t
    (** [override_headers ~by request] is [request] with headers
        {!Headers.override}[ (headers response) ~by]. *)

    val pp : Format.formatter -> t -> unit
    (** [pp] formats responses for inspection. Guarantees not to
        consume the {!val-body}. *)

    (** {1:properties Properties} *)

    val body : t -> Body.t
    (** [body request] is the body of [request]. *)

    val headers : t -> Headers.t
    (** [headers request] are the HTTP headers of [request]. Should always
        at least includes at the {!Http.Headers.host} header. *)

    val log : t -> string
    (** [log request] is the log of [request]. The log is a
        client-side explanatation {b not meant to be sent to the
        server}. It can be used to log further details or explanations
        about the request. *)

    val method' : t -> Method.t
    (** [method' request] is the
        {{:https://www.rfc-editor.org/rfc/rfc9110#name-methods}HTTP method}
        of [request]. *)

    val path : t -> Path.t
    (** [path request] is the absolute path of the {!raw_path} of [request]
        {{!Http.Path.strip_prefix}stripped} by the {!service_path} of
        [request]. This is the path you want your service to handle. *)

    val query : t -> string option
    (** [query request] is the the query part (without the [?])  of
        the {!raw_path} of [request]. [None] is returned if there is
        no [?] at all. [Some ""] is returned if there is a [?]
        followed by emptyness. To decode the query and possibly handle
        those that are [POST]ed aswell, see {!to_query}. *)

    val raw_path : t -> string
    (** [raw_path request] is the
        {{:https://www.rfc-editor.org/rfc/rfc9112#name-request-target}request
        target} (HTTP/1.1) or
        {{:https://www.rfc-editor.org/rfc/rfc9113#section-8.3.1-2.4.1}
        [:path]} pseudo-header (HTTP/2, HTTP/3) of [request].  Usually one
        rather wants to use the convenience {!val-path} and
        {!val-query} which are derived from this value as this makes
        the service insensitive to where it is attached, see
        {!service_path}. *)

    val scheme : t -> Scheme.t
    (** [scheme request] is the scheme of the request. {b Note that
        this is indicative}. In particular services should not rely on
        this as this may depend on the way you are proxied or not be
        set by the connector. It it is mainly used by client
        connectors to be able to reconstruct the requested URL for
        example to follow redirections. *)

    val service_path : t -> Path.t
    (** [service_path r] is service path of [r].
        {ul
        {- For service connectors this is the path on which the root
           of the service is attached. This is usually defined by the service
           connector. The {!val-path} value of [r] is the path mentioned in
           {!raw_path} stripped by this path.}
        {- For client connectors this is irrelevant and set to
           {!Path.root}}} *)

    val version : t -> Version.t
    (** [version r] is the HTTP version of the request.
        {ul
        {- For service connectors this should be the HTTP version of the
           request made on the connector. Note that if the service connector
           interfaces with a gateway this may be different from the actual
           version used by the gateway with the client.}
        {- For client connectors this is mostly irrelevant: the connectors
           decides how they want to talk to the client. But it can
           be used as hint for which HTTP version to use.}} *)

    (** {1:echo Echo} *)

    val echo : ?status:Status.t -> t -> Response.t
    (** [echo request] is a response with status [status] (defaults to
        [404]) and a [text/plain] body that has all the properties of
        [request], including the request body, which is consumed
        by the function with {!Body.to_string}.

        {b Note.} In general using [echo] violates numerous HTTP musts. *)

    (** {1:deconstruct Deconstructing and responding}

        Request deconstruction and response helpers. When they error
        these functions directly do with responses that have the
        right statuses and, unless otherwise noted, empty bodies. *)

    (** {2:redirect Redirection} *)

    val redirect_to_path :
      ?body:Body.t -> ?headers:Headers.t -> ?log:string ->
      ?reason:string -> t -> Status.t -> Path.t -> Response.t
    (** [redirect_to_path request status path] redirects to [path] in
        the service of [request]. This is {!Response.redirect}[ status
        loc] with [loc] the result of {{!Path.encode}encoding} the
        path:
        {[Path.concat (service_path request) path]} *)

    (** {2:header_decoding Header decoding} *)

    val decode_header :
      Headers.Name.t -> (string -> ('a, string) result) -> t ->
      ('a option, Response.t) result
    (** [decode_header h dec r] decodes header [h] (if any) in [r].
        Errors with {!Http.Status.bad_request_400} in case of decoding
        errors and transfers the error message to {!Response.reason}. *)

    (** {2:method_constraints Method constraints} *)

    val allow : 'a Method.constraint' list -> t -> ('a, Response.t) result
    (** [allow ms r] is:
          {ul
          {- [Ok (Req.meth r)] if [List.mem (Req.meth r, Req.meth r) ms]}
          {- [Error _] with a {!Http.Status.method_not_allowed_405}
             response otherwise.}} *)

    (** {2:cookies Cookies} *)

    val find_cookie : name:string -> t -> (string option, string) result
    (** [find_cookie ~name r] is the value of cookie [name] or [None] if
        undefined in [r]. Errors on header or cookie decoding errors.

        {b FIXME.} Why is this a string error ? *)

    (** {2:queries Queries} *)

    val to_query : t -> (Query.t, Response.t) result
    (** [to_query r] extracts a query from [r]. This is
        {ul
        {- [Ok q] with [q] parsed from [Req.query r] if [r]'s
           method is [`GET] or [`HEAD].}
        {- [Ok q] with [q] parsed from the request body on
           other methods and the content type is
           {!Media_type.application_x_www_form_urlencoded}.
           In this case the {!Webs.Http.Request.query} is ignored.}
        {- [Error _] with a:
        {ul
        {- {!Http.Status.unsupported_media_type_415} response if the
           content type is unsupported}
        {- {!Http.Status.bad_request_400} reponse on decoding errors.}}}}

        {b Warning.} {!Http.Query.t} values are untrusted,
        you need to properly validate their data. *)

    (** {2:clean Path cleaning} *)

    val clean_path : t -> (unit, Response.t) result
    (** [clean_path r] is:
        {ul
        {- [Ok ()] if [r]'s path is [[]], [[""]] or if it has no empty segment.}
        {- [Error _] with a {!Http.Status.moved_permanently_301} to [r]'s
        path without empty segments or the root if that results in the empty
        path.}}

        {b Note.} There's more than one way to handle empty segments
        and trailing slashes in request paths. The scheme proposed
        here simply always redirects to paths in which all empty
        segments, and thus trailing slashes, are removed; except on
        the root path. The advantage of this scheme is that no
        elaborate file extension logic on the final segment is needed
        to route file serving (I no longer understand this
        comment).

        {b Warning.} This cleaning does not touch dot segments or
        percent-encoded directory separators that may be present in the
        path. You should still use
        {{!Http.Path.to_absolute_filepath}that function} or
        {!to_absolute_filepath} for mapping paths to file paths. *)

    (** {2:file_path Absolute file paths} *)

    val to_absolute_filepath :
      ?strip:Path.t -> file_root:Path.fpath -> t ->
      (Path.fpath, Response.t) result
    (** [absolute_filepath ~strip ~file_root request] is:
        {ul
        {- [Ok file] with [file] an {e absolute} file path strictly rooted
           in [file_root]. [file] is made by
           {{!Http.Path.strip_prefix}stripping} [strip] (defaults to [[""]])
           from [r]'s {!val-path},
           {{!Http.Path.to_absolute_filepath} converting} the result
          to an absolute filepath and
          {{!Http.Path.prefix_filepath}prefixing} it with [file_root].}
        {- [Error r] with [r] an empty {!Http.Status.not_found_404} response
          if stripping [strip] results in [None] and
          {!Http.Status.bad_request_400} if the absolute path conversion
          fails.}} *)

    (** {2:etag Etags} *)

    val eval_if_none_match :
      t -> Etag.t -> headers:Headers.t -> (Headers.t, Response.t) result
    (** [eval_if_none_match request etag ~headers] is
        {ul
        {- [Ok hs] with [hs] the value [headers]
           added with a {!Headers.etag} set to [etag].
           If [request] has no {!Headers.if_none_match} header or if
           it has one and {!Etag.eval_if_none_match} returns [true]
           on [etag].}
        {- [Error r] with [r] an empty {!Http.Status.not_modified_304} response
           with headers [headers] added with a {!Headers.etag} set to [etag].
           If [r] has a {!Headers.if_none_match} header and that
           {!Etag.eval_if_none_match} returns [false].}
        {- [Error r] with [r] an empty {!Http.Status.bad_request_400} response
           if the {!Headers.if_none_match} decoding errors.}}

        {b Design note.} This slightly abuses the idea of the [result] design
        idea which was rather to carry error responses in the [Error _]
        case. But… it's convenient. *)
  end

  (** {1:connector_tools Connector tools} *)

  (** Tool for connectors. *)
  module Connector : sig

    (** Connector log messages.

        This is a {e suggested} log message format for connectors.
        They can be emitted by connectors to track activity and report
        unexpected events. *)
    module Log : sig

      type dur_ns = int64
      (** The type for integer nanosecond duration. *)

      type msg =
      [ `Service_exn of exn * Stdlib.Printexc.raw_backtrace
      | `Connector_exn of exn * Stdlib.Printexc.raw_backtrace
      | `Connection_reset
      | `Trace of dur_ns * Request.t option * Response.t option ]
      (** The type for connector log messages. *)

      val pp_msg : Format.formatter -> msg -> unit
      (** [pp_log_msg] is a unspecified formatter for log messages. *)

      val quiet : msg -> unit
      (** [quiet] is [Fun.const ()]. *)

      val default :
        ?ppf:Format.formatter -> trace:bool -> unit -> (msg -> unit)
        (** [default_log ~ppf ~trace] logs message on [ppf] (defaults to
            {!Format.err_formatter}) and [`Trace] messages iff [trace] is
            true. *)
    end

    (** Default values for connector properties. *)
    module Default : sig
      val max_request_headers_byte_size : int
      (** [max_request_headers_byte_size] is 64k in bytes. *)

      val max_request_body_byte_size : int
      (** [default_max_body_byte_size] s 10Mo in bytes. *)
    end

    (** Private codecs (unstable).

        {b Warning.} This API is unstable. It may change between
        minor versions of the library. Use at your own risk. *)
    module Private : sig

      (**/**)
      val string_subrange : ?first:int -> ?last:int -> string -> string
      val string_lowercase : string -> string
      (**/**)

      val trim_ows : string -> string
      (** [trim_ows] trims starting and ending HTTP ows. *)

      val decode_request_line :
        bytes -> first:int -> crlf:int -> Method.t * string * Version.t
      (** [decode_request_line b ~first ~crlf] decodes a request line
          that starts at [first] and whose ending CRLF starts at
          [crlf]. Raises [Failure] on errors. *)

      val decode_status_line :
        bytes -> first:int -> crlf:int -> Version.t * Status.t * string
      (** [decode_status_line b ~first ~crlf] decodes a status line
          that starts at [first] and whose ending CRLF starts at [crlf].
          Raises [Failure] on errors. *)

      val decode_header_field :
        bytes -> first:int -> crlf:int -> Headers.Name.t * string
      (** [decode_header_field b ~first ~crlf] decodes a header field
          that starts at [first] and whose ending CRLF starts at
          [crlf]. Raises [Failure] on errors. *)

      val decode_headers : bytes -> crlfs:int list -> Headers.t
      (** [decode_headers b crlfs] decodes the headers. [b] has the
          header section with the start line (either request or status
          line) or finished by the first [crlfs]. *)

      val decode_http11_response : bytes -> first:int -> Response.t
      (** [decode_http11_response b ~first] decodes an HTTP/1.1 full
          response from [b] starting at [first]. The result satisfies
          {{!page-connector_conventions.client_responses}client
          responses}.  Raises [Failure] on errors. *)

      val encode_http11_response_head :
        Status.t -> reason:string -> Headers.t -> string
      (** [encode_http11_response_head] is the HTTP/1.1 head for a response
          with the given parameters. This has the final double CRLF. *)

      val encode_http11_request_head :
        Method.t -> request_target:string -> Headers.t -> string
      (** [encode_http11_request_head] is the HTTP/1.1 head for a request
          with the given paramters. This has the final double CRLF. *)
    end
  end
end

(** HTTP clients.

    See {{!Http_client.examples}examples}. *)
module Http_client : sig

  (** {1:clients Clients} *)

  val default_max_redirection : int
  (** [default_max_redirection] is the default maximal number of
      redirections when they are followed, see {!val-request}. *)

  type t
  (** The type for HTTP clients.  *)

  val id : t -> string
  (** [id httpc] identifies the underlying implementation of [httpc]. *)

  val request :
    ?max_redirections:int -> t -> follow:bool ->
    Http.Request.t -> (Http.Response.t, string) result
  (** [request httpc ~follow request] performs request [request] via
      [httpc]. To construct a request from an URL use
      {!Http.Request.of_url}.  Read more details about how [request]
      is interpreted by client connectors in the
      {{!page-connector_conventions.client_connectors}client connector
      conventions}.

      If [follow] is [true] and the request is [GET] or [HEAD], HTTP
      responses are automatically
      {{:https://www.rfc-editor.org/rfc/rfc9110#name-redirection-3xx}
      redirected} on 301, 302, 303, 305, 307 and 308. In this case the
      the original request is modified as follows:
      {ul
      {- The headers {!Http.Headers.referer}, {!Http.Headers.origin},
         {!Http.Headers.connection} and the conditional request
         headers {!Http.Headers.if_match}
         {!Http.Headers.if_none_match}, {!Http.Headers.if_modified_since}
         {!Http.Headers.if_unmodified_since}, {!Http.Headers.if_range}
         are dropped}
      {- If the host changes, the {!Http.Headers.authorization},
         {!Http.Headers.proxy_authorization} and
         {!Http.Headers.cookie} are dropped}}

      In case there was a follow, the final requested URL can be found in the
      response in the {!x_follow_location} header.
  *)

  val get : t -> follow:bool -> url:Webs_url.t -> (string, string) result
  (** [get c ~follow ~url] is the body of a [GET] request on [url].
      For the semantics of [follow] see {!request}.

      {b Note.} This is voluntarily kept bare bones (e.g. no headers
      can be specified). Anything more complex should use {!request}. *)

  val x_follow_location : Http.Headers.Name.t
  (** [x_follow_location] is the final location that was requested
      when {!follow} is true. *)

  (** {1:examples Examples}

      This fetches {:https://example.org} with {!Webs_spawn_client}
      and {!Http_client.get}.
 {[
 open Webs

 let main () =
   let httpc = Webs_spawn_client.make () in
   let url = "https://example.org" in
   match Http_client.get httpc ~follow:true ~url with
   | Error e -> prerr_endline e; 1
   | Ok page -> print_endline page; 0

 let () = if !Sys.interactive then () else exit (main ())
]}

    This shows how to use {!Http_client.request} to implement
    {!Http_client.get}.
{[
open Webs
let ( let* ) = Result.bind

let get httpc ~follow ~url =
  let* request = Http.Request.of_url `GET ~url in
  let* response = Http_client.request httpc ~follow req in
  match Http.Response.status response with
  | 200 -> Http.Body.to_string (Http.Response.body response)
  | st -> Error (Format.asprintf "%a" Http.Status.pp st)
]}
*)

  (** {1:connectors Client connectors}

      If you devise your own HTTP client it should provide constructor
      functions that return {!Http_client.t} values directly.  These
      values are constructed with {!make}. *)

  (** Client connector. *)
  module type T = sig

    type t
    (** The type for HTTP clients. *)

    val id : t -> string
    (** See {!Webs.Http_client.id}. *)

    val request : t -> Http.Request.t -> (Http.Response.t, string) result
    (** [request httpc request] perform request [request] with [httpc].

        This function should follow the
        {{!page-connector_conventions.client_connectors}client
        connector conventions}. *)
  end

  val make : (module T with type t = 'a) -> 'a -> t
  (** [make impl httpc] packs an HTTP client implementation [impl] and
      its specific implementation [httpc]. *)
end
