(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Web service interface.

    Consult the {{!page-web_service_howto}web service howto} for quick
    steps to run your first service. A few tools are in {!Webs_kit}
    and gateway connectors to run services can be found
    {{!page-index.connectors}here}.

    Open the module to use it. It defines only modules and types in
    your scope. *)

(** {1:services Services} *)

(** HTTP nuts and bolts.

    Just types, values, codecs and protocol logic fragments.

    {b References.}
    {ul
    {- R. Fielding et al.
    {{:https://tools.ietf.org/html/rfc7230}
    {e Hypertext Transfer Protocol (HTTP/1.1): Message Syntax and Routing}}.
    2014}
    {- R. Fielding et al.
    {{:https://tools.ietf.org/html/rfc7231}
    {e Hypertext Transfer Protocol (HTTP/1.1): Semantics and Content}}.
    2014}} *)
module Http : sig

  (** {1:codecs Codecs} *)

  (**/**)
  val string_subrange : ?first:int -> ?last:int -> string -> string
  val string_starts_with : prefix:string -> string -> bool
  val string_lowercase : string -> string
  (**/**)

  (** {2:encs Encodings} *)

  (** [base64] and [base64url] codecs.

      As defined in {{:https://tools.ietf.org/html/rfc4648}RFC 4684}. *)
  module Base64 : sig

    (** {1:error Decode errors} *)

    type error =
    | Invalid_letter of (bool * int * char) (** index and letter. *)
    | Unexpected_eoi of bool (** *)
    (** The type for decoding errors. The boolean is [true] if that was
        a [base64url] decode. *)

    val error_message : error -> string
    (** [error_message e] is an error message for [e]. *)

    val error_string : ('a, error) result -> ('a, string) result
    (** [error_string r] is [Result.map_error error_message r]. *)

    (** {1:base64 [base64]} *)

    val encode : string -> string
    (** [encode s] is the
        {{:https://tools.ietf.org/html/rfc4648#section-4}[base64]}
        encoding of [s]. *)

    val decode : string -> (string, error) result
    (** [decode s] is the
        {{:https://tools.ietf.org/html/rfc4648#section-4}[base64]}
        decode of [s]. *)

    (** {1:base64url [base64url]} *)

    val url_encode : string -> string
    (** [url_encode] is like {!encode} but for the
        {{:https://datatracker.ietf.org/doc/html/rfc4648#section-5}[base64url]}
        encoding. *)

    val url_decode : string -> (string, error) result
    (** [url_decode] is like {!decode} but for the
        {{:https://datatracker.ietf.org/doc/html/rfc4648#section-5}[base64url]}
        encoding. *)
  end

  (** Percent-encoding codec.

      Codecs percent-encoding according to
      {{:https://tools.ietf.org/html/rfc3986#section-2.1}RFC 3986}.

      {b Note.} This should not be used for URI query strings and
      [application/x-www-form-urlencoded] which is slightly different.
      The {!Query} module handles that. *)
  module Pct : sig
    val encode : string -> string
    (** [encode s] is the percent-encoding of [s].

        {b TODO.} Make that more subtle for now
        we percent-encode what is not percent-encoded
        {{:https://tools.ietf.org/html/rfc3986#section-3.3}[pchar]}
        in RFC 3986. *)

    val decode : string -> string
    (** [decode s] is the percent-encoding decode of [s]. *)
  end

  (** HTTP digits codec.

      These represent non-negative integers. The module
      detects overflows and turns them into errors. *)
  module Digits : sig
    val decode : string -> (int, string) result
    (** [decode s] is the non-empty sequence of
        {{:https://tools.ietf.org/html/rfc5234#appendix-B.1}decimal digits}
        [s] as a non-negative integer. *)

    val encode : int -> string
    (** [encode_digits n] is the non-negative integer [n] as a sequence
        of decimal digits.

        @raise Invalid_argument if [n] is negative. *)
  end

  (** {2:names Names}

      HTTP often requires to perform US-ASCII case insensitive
      comparisons on HTTP tokens.  Values of type {!name} represent
      US-ASCII lowercased HTTP tokens. *)

  type name = private string
  (** The type for lowercased HTTP
      {{:https://tools.ietf.org/html/rfc7230#section-3.2.6}tokens}.
      In particular header
      {{:https://tools.ietf.org/html/rfc7230#section-3.2}
      field-name}s. *)

  (** Names. *)
  module Name : sig

    type t = name
    (** See {!name}. *)

    val v : string -> name
    (** [v s] is a name from [s]. Raises [Invalid_argument] if
        [s] is not a header name.  Use {!decode} if you
        need to handle failures. *)

    val equal : name -> name -> bool
    (** [equal n n'] is [true] iff [n] and [n'] are equal. *)

    val compare : name -> name -> int
    (** [compare] is {!String.compare}. *)

    val decode : string -> (name, string) result
    (** [decode s] decodes a name from [s]. *)

    val encode : name -> string
    (** [encode n] encodes a name for [s]. *)

    val pp : Format.formatter -> name -> unit
    (** [pp] is an unspecified formatter for header names. *)
  end

  (** {2:versions Versions} *)

  type version = int * int
  (** The type for {{:https://tools.ietf.org/html/rfc7230#section-2.6}HTTP
      versions}. Both integers must be in the interval [\[0;9\]]. *)

  (** HTTP versions. *)
  module Version : sig
    type t = version
    (** The type for versions. See {!type:version}. *)

    val decode : string -> (version, string) result
    (** [decode s] decodes a version from [s]. *)

    val encode : version -> string
    (** [encode v] encodes the version [v]. Assumes correct integer ranges. *)

    val pp : Format.formatter -> version -> unit
    (** [pp] is an unspecified formatter for versions. *)
  end


  (** {1:methods Methods} *)

  type meth =
  [ `GET
     (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.1}[GET]} *)
  | `HEAD
     (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.2}[HEAD]} *)
  | `POST
     (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.3}[POST]} *)
  | `PUT
     (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.4}[PUT]} *)
  | `DELETE
     (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.5}[DELETE]} *)
  | `CONNECT
     (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.6}[CONNECT]} *)
  | `OPTIONS
     (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.7}[OPTIONS]} *)
  | `TRACE
     (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.8}[TRACE]} *)
  | `PATCH
     (** {{:http://tools.ietf.org/html/rfc5789}[PATCH]} '*)
  | `Other of string
    (** other {{:https://tools.ietf.org/html/rfc7230#section-3.2.6}token} *)
  ]
  (** The type for HTTP {{:https://tools.ietf.org/html/rfc7231#section-4}
      request methods}. *)

  (** HTTP request methods *)
  module Meth : sig

    type t = meth
    (** See {!meth}. *)

    val decode : string -> (meth, string) result
    (** [decode s] decodes an HTTP method from [s]. *)

    val encode : meth -> string
    (** [encode m] encodes [m] to an HTTP method.

        @raise Invalid_argument if [m] is [`Other t] and [t] is not
        a {!H.is_token}. *)

    val pp : Format.formatter -> meth -> unit
    (** [pp] is an unspecified formatter for methods. *)
  end

  (** {1:paths_and_queries Paths and queries} *)

  type fpath = string
    (** The type for file paths. *)

  type path = string list
  (** The type for absolute URI paths represented as {e non-empty}
      lists of {e percent-decoded} path segments. The empty list denotes
      the absence of a path.

      Path segments can be empty [""]. The root path [/] is
      represented by the list [[""]] and [/a] by [["a"]], see more
      examples {{!Path.decode}here}.

      {b WARNING.} You should never concatenate these segments with a
      separator to get a file path because they may contain stray
      percent-decoded directory separators. Use the function
      {!Path.to_absolute_filepath} to interpret paths as file
      paths. *)

  (** Paths. *)
  module Path : sig

    (** {1:paths Paths} *)

    type t = path
    (** See {!type-path}. *)

    val undot_and_compress : path -> path
    (** [undot_and_compress p] removes ["."] and [".."]  according to
        the RFC 3986
        {{:https://tools.ietf.org/html/rfc3986#section-5.2.4}algorithm}
        and suppresses non-final empty [""] segments. *)

    val strip_prefix : prefix:path -> path -> path option
    (** [strip_prefix ~prefix p] removes the prefix path [prefix] from
        [p].  If [prefix] ends with an empty segment, it matches any
        corresponding segment at that point (so that stripping [/a/]
        from [/a/b] results in [/b]).

        If [p] is not prefixed by [prefix], [None] is returned [Some
        []] is ever returned.

        Given a path [p] and the same path [p'] with a trailing
        slash, the set of paths prefixed by [p] is the the set of
        path prefixed by [p'] plus [p] itelf.

        A few examples:
        {ul
        {- [strip_prefix [""] (_ :: _ as l) = Some l]}
        {- [strip_prefix _ [] -> None]}
        {- [strip_prefix [] _ = None]}
        {- [strip_prefix ["a"] ["b"] = None]}
        {- [strip_prefix ["a"] ["a"] = Some [""]]}
        {- [strip_prefix ["a"] ["a"; ""] = Some [""]]}
        {- [strip_prefix ["a"] ["a"; "b"; ] = Some ["b"]]}
        {- [strip_prefix ["a"] ["a"; "b"; ""] = Some ["b"; ""]]}
        {- [strip_prefix ["a"] ["a"; ""; "b"] = Some [""; "b"]]}
        {- [strip_prefix ["a"; ""] ["a"] = None]}
        {- [strip_prefix ["a"; ""] ["a"; ""] = Some [""]]}
        {- [strip_prefix ["a"; ""] ["a"; "b"; ] = Some ["b"]]}
        {- [strip_prefix ["a"; ""] ["a"; "b"; ""] = Some ["b"; ""]]}
        {- [strip_prefix ["a"; ""] ["a"; ""; "b"] = Some [""; "b"]]}} *)

    val concat : path -> path -> path
    (** [concat p0 p1] concatenates [p0] and [p1]. If [p0] ends with
        an empty segment and [p1] is non-empty that empty segment is dropped.
        A few examples:
        {ul
        {- [concat p0 [] = p0]}
        {- [concat [] p1 = p1]}
        {- [concat [""] ["a"; "b"] = ["a"; "b"]]}
        {- [concat ["a"] [""] = ["a"; ""]]}
        {- [concat ["a"; ""] [""] = ["a"; ""]]}
        {- [concat ["a"; "b"] ["c"; "d"] = ["a"; "b"; "c"; "d"]]}
        {- [concat ["a"; "b"; ""] ["c"; "d"] = ["a"; "b"; "c"; "d"]]}
        {- [concat ["a"; "b"; ""] [""] = ["a"; "b"]]}
        {- [concat ["a"; "b"; ""] [""; "c"] = ["a"; "b"; ""; "c"]]}} *)

    val relative : src:path -> dst:path -> path
    (** [relative ~src ~dst] is the relative path [rel] that goes from
        absolute [src] to absolute [dst]. This means that
        [undot_and_compress (concat src rel)] should yield
        [dst]. *)

    (** {1:filepath File paths} *)

    val has_dir_seps : string -> bool
    (** [has_dir_seps s] is true iff [s] contains a '/' or a '\\'
        character. *)

    val to_absolute_filepath : path -> (fpath, string) result
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

    val encode : path -> string
    (** [encode p] encodes an
        {{:https://tools.ietf.org/html/rfc7230#section-2.7}[absolute-path]}
        for [p] as follows:

        {ol
        {- In each segment {{:http://tools.ietf.org/html/rfc3986#section-2.1}
         percent-encode} any byte that is not
         {{:http://tools.ietf.org/html/rfc3986#section-2.3}[unreserved]},
         {{:http://tools.ietf.org/html/rfc3986#section-2.2}[sub-delims]},
         [':'] or ['@'] to produce a valid URI
         {{:http://tools.ietf.org/html/rfc3986#section-3.3}[segment].}}
        {- Prepends each segment with a ['/'].}
        {- Concatenate the result.}}

        The empty list is special cased and yields [""]. This is for
        encoding HTTP paths, use {!to_undotted_filepath} to
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

    val decode : string -> (path, string) result
    (** [decode s] decodes an
        {{:https://tools.ietf.org/html/rfc7230#section-2.7}[absolute-path]}
        to its
        {{:http://tools.ietf.org/html/rfc3986#section-2.1}percent-decoded}
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

    (**/**)
    val and_query_of_request_target : string -> string list * string option
    (** raises [Failure] *)
    (**/**)

    val pp : Format.formatter -> path -> unit
    (** [pp] is an unspecified formatter for paths. *)

    val pp_dump : Format.formatter -> path -> unit
    (** [pp_dump] is an unspecified debugging formatter for paths. *)
  end

  type query
  (** The type for queries as key-values maps. Both keys and values
      are properly decoded. Note that keys can map to
      multiple values. *)

  (** Queries and query codecs. *)
  module Query : sig

    (** {1:queries Queries} *)

    type t = query
    (** See {!type-query}. *)

    val empty : query
    (** [empty] is the empty key-values map. *)

    val is_empty : query -> bool
    (** [is_empty q] is true if [q] is {!empty}. *)

    val mem : string -> query -> bool
    (** [mem k q] is true [iff] key [k] is bound in [q]. *)

    val def : string -> string -> query -> query
    (** [def k v q] is [q] with [k] bound only to value [v]. *)

    val add : string -> string -> query -> query
    (** [add k v q] is [q] with value [v] appended to
        [k]'s values (or {!set} if there was no binding for [k]). *)

    val undef : string -> query -> query
    (** [undef k q] is [q] with [k] unbound. *)

    val find : string -> query -> string option
    (** [find k q] is the value of [k]'s first binding in [q], if any. *)

    val find_all : string -> query -> string list
    (** [find_all k q] are all the values bound to [k] or the empty
        list if [k] is unbound. *)

    val fold : (string -> string -> 'a -> 'a) -> query -> 'a -> 'a
    (** [fold f q acc] folds over all the key-value bindings. For keys
        with multiple values folds over them in the same order
        as {!find_all}. *)

    val keep_only_first : query -> query
    (** [keep_only_first q] is [q] with only the first value kept in bindings
        with multiple values. *)

    (** {1:conv Converting} *)

    val decode : string -> query
    (** [decode s] decodes the
        {{:https://url.spec.whatwg.org/#application/x-www-form-urlencoded}
        [application/x-www-form-urlencoded]}
        [s] to a query.  If a key is defined more than once,
        the first definition is returned by {!find} and the
        left-to-right order preserved by {!find_all}'s list. The input
        string is not checked for UTF-8 validity. *)

    val encode : query -> string
    (** [encode q] encodes [q] to an
        {{:https://url.spec.whatwg.org/#application/x-www-form-urlencoded}
        [application/x-www-form-urlencoded]}
        string. *)

    val pp : Format.formatter -> query -> unit
    (** [pp] is an unspecified formatter for queries. *)
  end

  (** {1:header Headers} *)

  type headers
  (** The type for HTTP headers. Maps header names to string values
      such that for:
      {ul
      {- Single valued headers, the value is the string.}
      {- Multi-valued headers, the values are comma [','] separated
         as per specification. Use {!Headers.values_of_string} on the string.}
      {- {!set_cookie} header, must be treated specially since it
         can be repeated but does not follow the syntax of
         multi-valued headers. The values are stored in the string
         separated by ['\x00'] values. Use {!Headers.add_set_cookie} and
         {!Headers.values_of_set_cookie_value} to handle the field.  On
         {!Headers.encode} this results in separate [set-cookie] headers.}} *)

  (** HTTP headers and values. *)
  module Headers : sig

    (** {1:headers Headers} *)

    val name : string -> name
    (** [name n] is {!Http.Header_name.v}. *)

    type t = headers
    (** See {!type-headers}. *)

    val empty : headers
    (** [empty] has no header definition. *)

    val is_empty : headers -> bool
    (** [is_empty hs] is [true] iff [hs] is has no definition. *)

    val mem : name -> headers -> bool
    (** [mem n hs] is [true] iff [n] is defined in [hs]. *)

    val find : ?lowervalue:bool -> name -> headers -> string option
    (** [find n hs] is the value of [n] in [hs] (if any).
        If [lowervalue] is [true] (defaults to [false])
        the US-ASCII uppercase letter are mapped on lowercase.

        If [n] is a multi-valued header use {!decode_multi_value} on
        the result. If [n] is {!set_cookie} you must use
        {!values_of_set_cookie_value}. *)

    val get : ?lowervalue:bool ->name -> headers -> string
    (** [get n hs] is like {!find} but raises [Invalid_argument] if [n]
        is not defined in [hs]. *)

    val def : name -> string -> headers -> headers
    (** [def n v hs] is [hs] with [n] defined to [v]. *)

    val def_if_some : name -> string option -> headers -> headers
    (** [def_some n o hs] is [hs] with [n] defined to [v] if [o]
        is [Some v] and [hs] otherwise. *)

    val def_if_undef : name -> string -> headers -> headers
    (** [def_if_undef n v hs] is [hs] with [n] defined to [v] if [n] is
        not defined in [hs]. *)

    val add : name -> string -> headers -> headers
    (** [add n v hs] appends [v] to the multi-valued header [n] in [hs]. *)

    val add_set_cookie : string -> headers -> headers
    (** [add_set_cookie c hs] adds a {!set_cookie} header with value [c].
        This appends to {!set_cookie}, see {!t}. *)

    val undef : name -> headers -> headers
    (** [undef n hs] is [hs] with [n] undefined. *)

    val override : headers -> by:headers -> headers
    (** [override hs ~by] are the headers of both [hs] and [by]
        with those of [by] taking over. *)

    val fold : (name -> string -> 'a -> 'a) -> headers -> 'a -> 'a
    (** [fold f m acc] folds [f] over the bindings of [hs] starting with
        [acc]. *)

    val pp : Format.formatter -> headers -> unit
    (** [pp ppf hs] prints an unspecified representation of [hs]
        on [ppf]. *)

    (** {1:lookups Header specific lookups} *)

    val request_body_length : headers ->
      ([ `Length of int | `Chunked ], string) result
    (** [request_body_length hs] determines the message body length
        of a request (the rules for responses is a bit different)
        as per {{:https://tools.ietf.org/html/rfc7230#section-3.3.3}
        specification}, by looking at the {!content_type} and
        {!transfer_encoding} in [hs]. *)

    (** {1:values Header values} *)

    val values_of_set_cookie_value : string -> string list
    (** [values_of_set_cookie_value v] decodes [v] as stored in
        by {!set_set_cookie} in the {!t} type to a list of cookies. *)

    val values_of_string : ?sep:char -> string -> string list
    (** [values_of_string s] splits the string [s] at [','] (or [sep]
        if specified) characters and trims the resulting strings from
        {{:https://tools.ietf.org/html/rfc7230#section-3.2.3} optional
        whitespace}, and lowercases the result if [lowercase] is [true].

        Note that by definition the result is never the
        empty list, the function returns [[""]] on [""]. *)

    val values_to_string : ?sep:char -> string list -> string
    (** [values_to_string vs] is [String.concat "," vs] but
        raise [Invalid_argument] if [vs] is [[]]. TODO why ? *)

    val is_token : string -> bool
    (** [is_token s] is [true] iff [s] in an HTTP
        a {{:https://tools.ietf.org/html/rfc7230#section-3.2.6}token}. *)
  end

  (** {2:cookie Cookies} *)

  (** Cookies.

      {b References}
      {ul
      {- A. Barth.
      {{:https://tools.ietf.org/html/rfc6265}
      {e HTTP State Management Mechanism}}.
      2011}} *)
  module Cookie : sig

    type atts
    (** The type for {{:https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie#attributes}cookie attributes}. *)

    val atts_default : atts
    (** [atts_default] are cookie attributes with [secure] set to [true],
        [http_only] set to [true], [same_site] set to ["strict"] and no other
        attribute specified. *)

    val atts :
      ?init:atts ->
      ?domain:string option -> ?http_only:bool -> ?max_age:int option ->
      ?path:path -> ?same_site:string -> ?secure:bool -> unit -> atts
    (** [atts ()] are the given {{:https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie#attributes}cookie attributes}. Those unspecified take
        the value of [init] which defaults to {!atts_default}. *)

    val encode : ?atts:atts -> name:string -> string -> string
    (** [encodes ~atts name s] encodes a cookie
        with attributes [atts] (defaults to {!atts_default}) for
        {!Headers.add_set_cookie}. *)

    val decode_list : string -> ((string * string) list, string) result
    (** [decode_list s] parses the
        {{:https://tools.ietf.org/html/rfc6265#section-4.2.1}cookie string}
        of a {!val-cookie} header value. *)
  end

  (** {2:etags_range Etags and ranges} *)

  (** Entity tags.

      {b References}
      {ul
      {- R. Fielding et al.
      {{:https://tools.ietf.org/html/rfc7232}
      {e Hypertext Transfer Protocol (HTTP/1.1): Conditional requests}}.
      2014}} *)
  module Etag : sig

    (** {1:etags Etags} *)

    type t
    (** The type for
        {{:https://tools.ietf.org/html/rfc7232#section-2.3}etags}. *)

    val v : weak:bool -> string -> t
    (** [v ~weak tag] is the etag [tag]. [weak] indicates if the etag is weak.

        {b Warning.}  The function does not check that the bytes of
        [tag] are valid; each should be one of [0x21], \[[0x23];[0x7E]\]
        or \[[0x80];[0xFF]\]. *)

    val is_weak : t -> bool
    (** [is_weak e] is [true] iff [e] is weak. *)

    val tag : t -> string
    (** [tag e] is the entity tag of [e]. *)

    val weak_match : t -> t -> bool
    (** [weak_match e0 e1] is [true] iff [e0] and [e1]
        {{:https://tools.ietf.org/html/rfc7232#section-2.3.2}weakly match}. *)

    val strong_match : t -> t -> bool
    (** [strong_match e0 e1] is [true] iff [e0] and [e1]
        {{:https://tools.ietf.org/html/rfc7232#section-2.3.2}strongly match}. *)

    val decode : string -> (t, string) result
    (** [decode s] is an
        {{:https://tools.ietf.org/html/rfc7232#section-2.3}etags} from [s]. *)

    val encode : t -> string
    (** [encode etag] is [etag] as an etag. *)

    (** {1:conds Etag conditions} *)

    type cond = [ `Any | `Etags of t list (** *) ]
    (** The type for etags conditions. This represents the value
        of {!H.if_match} or {!H.if_none_match} headers. *)

    val decode_cond : string -> (cond, string) result
    (** [decode_cond s] parses an etag condition from [s]. *)

    val encode_cond : cond -> string
    (** [encode_cond c] serializes condition [c]. *)

    val eval_if_match : cond -> t option -> bool
    (** [eval_if_match c t] evaluates the logic of an
        {!H.if_match} header condition [c] on an entity represented
        by [t] ([None] means the representation does not exist). This is:
        {ul
        {- [true] if [c] is [None] (no condition).}
        {- [true] if [t] is [Some _] and [c] is [Some `Any].}
        {- [true] if [t] is [Some etag], [c] is [Some (`Etags etags)] and
           [etag] {{!strong_match}strongly matches} one of the [etags].}
        {- [false] otherwise.}} *)

    val eval_if_none_match : cond -> t option -> bool
    (** [eval_if_none_match c t] evaluates the logic of an
        {!H.if_none_match} header condition [c] on an entity represented
        by [t] ([None] means the representation does not exist). This is:
        {ul
        {- [true] if [t] is [None] and [c] is [Some `Any].}
        {- [true] if [t] is [Some etag], [c] is [Some (`Etags etags)] and
           [etag] {{!weak_match}weakly matches} none of the [etags]}
        {- [false] otherwise.}} *)

    val eval_if_range : t -> t option -> bool
    (** [eval_if_range req t] evaluates the logic of an {!H.if_range} header
        etag [req] on an entity represented by [t] ([None] means the
        representation does not exist). This is:
        {ul
        {- [true] if [t] is [Some etag] and [etag] {{!strong_match}strongly
            matches} [req]}
        {- [false] otherwise.}} *)
  end

  (** Range requests.

      {b References.}
      {ul
      {- R. Fielding et al.
      {{:https://tools.ietf.org/html/rfc7233}
      {e Hypertext Transfer Protocol (HTTP/1.1): Range Requests}}.
      2014}} *)
  module Range : sig

    (** {1:bytes Byte ranges} *)

    type bytes =
    [ `First of int (** First given offset to last offset *)
    | `Last of int (** At most last given [n] bytes. *)
    | `Range of int * int (** First offset and last offset. *) ]
    (** The type for byte range specifications. Offsets are zero-based. *)

    val eval_bytes : len:int -> bytes -> (int * int) option
    (** [eval_bytes ~len b] given a representation length [len] and
        byte range [b] returns a concrete zero-based byte range or
        [None] if the range cannot be satisfied for [len].  *)

    (** {1:ranges Ranges} *)

    type t =
    [ `Bytes of bytes list (** Byte ranges. *)
    | `Other of string * string (** Range unit and value. *) ]
    (** The type for ranges. *)

    val decode : string -> (t, string) result
    (** [decode s] decodes a
        {{:https://tools.ietf.org/html/rfc7233#section-3.1}range} header
        value. *)

    val encode : t -> string
    (** [encode r] serializes ranges [r] in unit [u].
        It's the client duty to make sure ranges are valid. *)
  end

  (** {2:standard_header_names Standard header names} *)

  val accept : name
  (** {{:https://tools.ietf.org/html/rfc7231#section-5.3.2}[accept]} *)

  val accept_charset : name
  (** {{:https://tools.ietf.org/html/rfc7231#section-5.3.3}
        [accept-charset]} *)

  val accept_encoding : name
  (** {{:https://tools.ietf.org/html/rfc7231#section-5.3.4}
        [accept-encoding]} *)

  val accept_language : name
  (** {{:https://tools.ietf.org/html/rfc7231#section-5.3.5}
        [accept-language]} *)

  val accept_ranges : name
  (** {{:https://tools.ietf.org/html/rfc7233#section-2.3}[accept-ranges]} *)

  val age : name
  (** {{:https://tools.ietf.org/html/rfc7234#section-5.1}[age]} *)

  val allow : name
  (** {{:https://tools.ietf.org/html/rfc7231#section-7.4.1}[allow]} *)

  val authorization : name
  (** {{:https://tools.ietf.org/html/rfc7235#section-4.2}[authorization]} *)

  val cache_control : name
  (** {{:https://tools.ietf.org/html/rfc7234#section-5.2}[cache-control]} *)

  val connection : name
  (** {{:https://tools.ietf.org/html/rfc7230#section-6.1}[connection]} *)

  val content_encoding : name
  (** {{:https://tools.ietf.org/html/rfc7231#section-3.1.2.2}
        [content-encoding]}*)

  val content_language : name
  (** {{:https://tools.ietf.org/html/rfc7231#section-3.1.3.2}
        [content-language]}*)

  val content_length : name
  (** {{:https://tools.ietf.org/html/rfc7230#section-3.3.2}
        [content-length]} *)

  val content_location : name
  (** {{:https://tools.ietf.org/html/rfc7231#section-3.1.4.2}
        [content-location]} *)

  val content_range : name
  (** {{:https://tools.ietf.org/html/rfc7233#section-4.2}[content-range]} *)

  val content_type : name
  (** {{:https://tools.ietf.org/html/rfc7231#section-3.1.1.5}
        [content-type]} *)

  val cookie : name
  (** {{:http://tools.ietf.org/html/rfc6265#section-4.2}[cookie]} *)

  val date : name
  (** {{:https://tools.ietf.org/html/rfc7231#section-7.1.1.2}[date]} *)

  val etag : name
  (** {{:https://tools.ietf.org/html/rfc7232#section-2.3}[etag]} *)

  val expect : name
  (** {{:https://tools.ietf.org/html/rfc7231#section-5.1.1}[expect]} *)

  val expires : name
  (** {{:https://tools.ietf.org/html/rfc7234#section-5.3}[expires]} *)

  val from : name
  (** {{:https://tools.ietf.org/html/rfc7231#section-5.5.1}[from]} *)

  val host : name
  (** {{:https://tools.ietf.org/html/rfc7230#section-5.4}[host]} *)

  val if_match : name
  (** {{:https://tools.ietf.org/html/rfc7232#section-3.1}[if-match]} *)

  val if_modified_since : name
  (** {{:https://tools.ietf.org/html/rfc7232#section-3.3}
        [if-modified-since]} *)

  val if_none_match : name
  (** {{:https://tools.ietf.org/html/rfc7232#section-3.2}[if-none-match]} *)

  val if_range : name
    (** {{:https://tools.ietf.org/html/rfc7233#section-3.2}[if-range]} *)

  val if_unmodified_since : name
  (** {{:https://tools.ietf.org/html/rfc7232#section-3.4}
        [if-unmodified-since]} *)

  val last_modified : name
  (** {{:https://tools.ietf.org/html/rfc7232#section-2.2}[last-modified]} *)

  val location : name
  (** {{:https://tools.ietf.org/html/rfc7231#section-7.1.2}[location]} *)

  val max_forwards : name
  (** {{:https://tools.ietf.org/html/rfc7231#section-5.1.2}[max-forwards]} *)

  val pragma : name
  (** {{:https://tools.ietf.org/html/rfc7234#section-5.4}[pragma]} *)

  val proxy_authenticate : name
  (** {{:https://tools.ietf.org/html/rfc7235#section-4.3}
        [proxy-authenticate]} *)

  val proxy_authorization : name
  (** {{:https://tools.ietf.org/html/rfc7235#section-4.4}
        [proxy-authorization]} *)

  val range : name
  (** {{:https://tools.ietf.org/html/rfc7233#section-3.1}[range]} *)

  val referer : name
  (** {{:https://tools.ietf.org/html/rfc7231#section-5.5.2}[referer]} *)

  val retry_after : name
  (** {{:https://tools.ietf.org/html/rfc7231#section-7.1.3}[retry-after]} *)

  val server : name
  (** {{:https://tools.ietf.org/html/rfc7231#section-7.4.2}[server]} *)

  val set_cookie : name
  (** {{:http://tools.ietf.org/html/rfc6265#section-4.1}[set-cookie]} *)

  val te : name
  (** {{:https://tools.ietf.org/html/rfc7230#section-4.3}[te]} *)

  val trailer : name
  (** {{:https://tools.ietf.org/html/rfc7230#section-4.4}[trailer]} *)

  val transfer_encoding : name
  (** {{:https://tools.ietf.org/html/rfc7230#section-3.3.1}
        [transfer-encoding]} *)

  val upgrade : name
  (** {{:https://tools.ietf.org/html/rfc7230#section-6.7}[upgrade]} *)

  val user_agent : name
  (** {{:https://tools.ietf.org/html/rfc7231#section-5.5.3}[user-agent]} *)

  val vary : name
  (** {{:https://tools.ietf.org/html/rfc7231#section-7.1.4}[vary]} *)

  val via : name
  (** {{:https://tools.ietf.org/html/rfc7230#section-5.7.1}[via]} *)

  val warning : name
  (** {{:https://tools.ietf.org/html/rfc7234#section-5.5}[warning]} *)

  val www_authenticate : name
  (** {{:https://tools.ietf.org/html/rfc7235#section-4.1}
        [www-authenticate]} *)

  (** {1:status_codes Status codes} *)

  type status = int
  (** The type for
      {{:https://tools.ietf.org/html/rfc7231#section-6}HTTP status codes}. *)

  (** Statuses. *)
  module Status : sig
    type t = status
    (** See {!type-status}. *)

    val reason_phrase : status -> string
    (** [reason_phrase s] is [s]'s reason phrase. *)

    val pp : Format.formatter -> status -> unit
    (** [pp] is an unspecified formatter for statuses. *)
  end

  (** {2:informational Informational 1xx} *)

  val continue_100 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.2.1}[100]} *)

  val switching_protocols_101 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.2.2}[101]} *)

  (** {2:sucessful Sucessful 2xx} *)

  val ok_200 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.3.1}[200]} *)

  val created_201 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.3.2}[201]} *)

  val accepted_202 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.3.3}[202]} *)

  val non_authoritative_information_203 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.3.4}[203]} *)

  val no_content_204 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.3.5}[204]} *)

  val reset_content_205 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.3.6}[205]} *)

  val partial_content_206 : status
  (** {{:https://tools.ietf.org/html/rfc7233#section-4.1}[206]} *)

  (** {2:redirection Redirection 3xx} *)

  val multiple_choices_300 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.4.1}[300]} *)

  val moved_permanently_301 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.4.2}[301]} *)

  val found_302 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.4.3}[302]} *)

  val see_other_303 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.4.4}[303]} *)

  val not_modified_304 : status
  (** {{:https://tools.ietf.org/html/rfc7232#section-4.1}[304]} *)

  val use_proxy_305 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.4.5}[305]} *)

  val temporary_redirect_307 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.4.7}[307]} *)

  (** {2:client_error Client Error 4xx} *)

  val bad_request_400 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.1}[400]} *)

  val unauthorized_401 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6}[401]} *)

  val payement_required_402 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.2}[402]} *)

  val forbidden_403 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.3}[403]} *)

  val not_found_404 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.4}[404]} *)

  val method_not_allowed_405 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.5}[405]} *)

  val not_acceptable_406 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.6}[406]} *)

  val proxy_authentication_required_407 : status
  (** {{:https://tools.ietf.org/html/rfc7235#section-3.2}[407]} *)

  val request_time_out_408 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.7}[408]} *)

  val conflict_409 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.8}[409]} *)

  val gone_410 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.9}[410]} *)

  val length_required_411 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.10}[411]} *)

  val precondition_failed_412 : status
  (** {{:https://tools.ietf.org/html/rfc7232#section-4.2}[412]} *)

  val payload_too_large_413 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.11}[413]} *)

  val uri_too_long_414 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.12}[414]} *)

  val unsupported_media_type_415 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.13}[415]} *)

  val range_not_satisfiable_416 : status
  (** {{:https://tools.ietf.org/html/rfc7233#section-4.4}[416]} *)

  val expectation_failed_417 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.14}[417]} *)

  val i'm_a_teapot_418 : status
  (** {{:https://tools.ietf.org/html/rfc2324#section-2.3.2}[418]} *)

  val upgrade_required_426 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.15}[436]} *)

  (** {2:server_error Server Error 5xx} *)

  val server_error_500 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.6.1}[500]} *)

  val not_implemented_501 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.6.2}[501]} *)

  val bad_gateway_502 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.6.3}[502]} *)

  val service_unavailable_503 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.6.4}[503]} *)

  val gateway_time_out_504 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.6.5}[504]} *)

  val http_version_not_supported_505 : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.6.6}[505]} *)

    (** {1:mime_type MIME types} *)

  type mime_type = string
  (** The type for MIME types. *)

  (** MIME type constants and file extensions. *)
  module Mime_type : sig

    (** {1:mime_types MIME types} *)

    type t = mime_type
    (** See {!type-mime_type}. *)

    val application_json : mime_type
    (** [application_json] is ["application/json"], JSON text. *)

    val application_octet_stream : mime_type
    (** [application_octet_stream] is ["application/octet-stream"],
        arbitrary bytes. *)

    val application_x_www_form_urlencoded : mime_type
    (** [application_x_www_form_urlencoded] is
        ["application/x-www-form-urlencoded"]. *)

    val text_css : mime_type
    (** [text_css] is ["text/css"], a CSS stylesheet. *)

    val text_html : mime_type
    (** [text_html] is ["text/html; charset=utf-8"], UTF-8 encoded HTML text. *)

    val text_javascript : mime_type
    (** [text_jvascript] is ["text/javascript"], JavaScript code. *)

    val text_plain : mime_type
    (** [text_plain] is ["text/plain; charset=utf-8"],
        UTF-8 encoded plain text. *)

    val multipart_byteranges : mime_type
    (** [multipart_byteranges] is ["multipart/byteranges"]. *)

    val multipart_form_data : mime_type
    (** [multipart_form_data] is ["multipart/form-data"]. *)

    (** {1:from_exts From file extensions} *)

    type file_ext = string
    (** The type for file extensions, including the [.] character. *)

    type file_ext_map = t Map.Make(String).t
    (** The type for maps from {{!file_ext}file extensions} to MIME types. *)

    val default_file_ext_map : file_ext_map Lazy.t
    (** [default_file_ext_map] is a default extension map. The map is
        documented by its implementation. Non self-describing
        [text/*] MIME types have the parameter [charset=utf-8]. *)

    val of_file_ext : ?map:file_ext_map -> file_ext -> t
    (** [of_file_ext ~map ext] is the value of [ext] in [map] or
        ["application/octet-stream"] if [ext] is unbound in [map].
        [map] defaults to {!default_file_ext_map}. *)

    val of_filepath : ?map:file_ext_map -> fpath -> t
    (** [of_filepath ~map f] is [of_file_ext ~map (Http.filepath_ext f)]. *)
  end

  (** {1:codecs Low-level codecs} *)

  (** Low-level codecs

      {b Warning.} This API is unstable and subject to change between
      minor versions of the library. *)

  module Private : sig

    val trim_ows : string -> string
    (** [trim_ows] trims starting and ending HTTP ows. *)

    val decode_request_line :
      bytes -> first:int -> crlf:int -> meth * string * version
    (** [decode_request_line b ~first ~crlf] decodes a request
        line that starts at [first] and whose ending CRLF starts
        at [crlf]. Raises [Failure] on errors. *)

    val decode_header_field :
      bytes -> first:int -> crlf:int -> name * string
    (** [decode_header_field b ~first ~crlf] decodes a header field
        that starts at [first] and whose ending CRLF starts at
        [crlf]. Raises [Failure] on errors. *)

    val encode_resp_header_section :
      version -> status -> string -> headers -> string
      (** [encode_resp_header_section v st rason_phrase hs] is the header
          section for a response with the given parameters. This has the final
          double CRLF. *)
  end
end

(** HTTP responses. *)
module Resp : sig

  (** {1:body Response bodies} *)

  type connection = ..
  (** The type for direct response connection. This is connector
      dependent. *)

  type consumer = (bytes * int * int) option -> unit
  (** The type for response consumers.

      Services call consumers with [Some (byte, first len)] to
      output the corresponding data and [None] when they are
      finished.

      Response consumers are provided by the connector to pull the
      body produced by a response.  If you are writing a consumer, the
      bytes MUST NOT be modified by the consumer and only read from
      [first] to [first+len]. *)

  type body =
  | Empty
  | Stream of (consumer -> unit)
  | Direct of (connection -> unit) (** *)
  (** The type for response bodies. This is either:
      {ul
      {- An empty body.}
      {- A stream to which a consumer will be passed by the connector.}
      {- A direct connection handler, the connector will pass it's
         connection representation.}} *)

  val empty_body : body
  (** [empty_body s] is an empty body. *)

  val stream_body : (consumer -> unit) -> body
  (** [stream_body producer] is a response body stream produced by
      [producer] on the consumer it will be given to. *)

  val direct_body : (connection -> unit) -> body
  (** [direct_body producer] is a response body produced by
      [producer] on the given (backend specific) [connection]. *)

  val body_of_string : string -> body
  (** [body_of_string s] is a reponse body made of string [s]. *)

  val pp_body : Format.formatter -> body -> unit
  (** [pp_body ppf b] prints an unspecified representation of [b]'s
      specification on [ppf] but guarantees not to consume the body. *)

  (** {1:resp Response} *)

  type t
  (** The type for responses. *)

  val v :
    ?version:Http.version -> ?explain:string -> ?reason:string -> ?body:body ->
    ?headers:Http.headers -> Http.status -> t
  (** [v status headers body] is a response with given
      [status], [headers] (defaults to {!Http.Headers.empty}), [body] (defaults
      to {!empty_body}, [reason] (defaults to {!Http.status_reason_phrase})
      and [version] (defaults to [(1,1)]), [explain] is a server side [reason]
      it is not put on the wire.

      {b FIXME.} Maybe make [body] non-optional to encourage use
      of {!empty} which is clearer in code.

      {b Note.} If [body] is [body_empty] (default) a {!Http.content_length}
      of [0] is automatically added to [headers]. *)

  val version : t -> Http.version
  (** [version r] is [r]'s version. *)

  val status : t -> Http.status
  (** [status r] is [r]'s status. *)

  val reason : t -> string
  (** [reason r] is [r]'s reason phrase. *)

  val headers : t -> Http.headers
  (** [headers r] is [r]'s headers. *)

  val body : t -> body
  (** [body r] is [r]'s body. *)

  val explain : t -> string
  (** [explain r] is [r]'s explanation. In contrast to [reason] this
      remains on the server and can be, for example logged. *)

  val with_status : ?explain:string -> ?reason:string -> Http.status -> t -> t
  (** [with_status st r] is [r] with status [st] and reason phrase
      [reason] (defaults to {!Http.status_reason_phrase}. *)

  val with_headers : Http.headers -> t -> t
  (** [with_headers hs r] is [r] with headers [hs]. *)

  val override_headers : by:Http.headers -> t -> t
  (** [override_headers by r] is [r] with headers
      [H.override (headers r) ~by]. *)

  val with_body : body -> t -> t
  (** [with_body b r] is [r] with body [b]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints an unspecified representation of [r] on [ppf] but
      guarantees not to consume the {!val:body}. *)

  (** {1:pre_canned Pre-canned responses}

      The optional [headers] argument of the functions below always
      {!Http.override} those the function computed.

      See also {{!Req.deconstruct}request deconstruction} combinators.

      {b FIXME.} Do a better compositional design, e.g. easily
      use the error responses with content responses. * *)

  val result : ('a, 'a) result -> 'a
  (** [result r] is [Result.fold ~ok:Fun.id ~error:Fun.id]. *)

  (** {2:pre_canned_content Content responses} *)

  val empty :
    ?explain:string -> ?reason:string -> ?headers:Http.headers -> int -> t
  (** [empty ?explain ?reason ?headers st] is
      [v ?explain ?reason ?headers st]. *)

  val content :
    ?explain:string -> ?headers:Http.headers -> mime_type:Http.mime_type ->
    int -> string -> t
  (** [content ~mime_type st s] responds [s] with content type
      [mime_type] and status [st]. Sets {!Http.content_type} and
      {!Http.content_length} appropriately. *)

  val text : ?explain:string -> ?headers:Http.headers -> int -> string -> t
  (** [text] responds with UTF-8 encoded plain text, i.e.
      {!content} with {!Http.Mime_type.text_plain}. *)

  val html : ?explain:string -> ?headers:Http.headers -> int -> string -> t
  (** [html] responds with UTF-8 encoded HTML text, i.e.
      {!content} with {!Http.Mime_type.text_html}.  *)

  val json : ?explain:string -> ?headers:Http.headers -> int -> string -> t
  (** [json] responds with JSON text, i.e. {!content} with
      {!Http.Mime_type.application_json}. *)

  val octets : ?explain:string -> ?headers:Http.headers -> int -> string -> t
  (** [octets] responds with octets, i.e. {!content} with
      {!Http.Mime_type.application_octet_stream}. *)

  (** {2:pre_redirect Redirect responses} *)

  val redirect : ?explain:string -> ?headers:Http.headers -> int -> string -> t
  (** [redirect status loc] redirects to {{!Http.location}location} [loc]
      with status [status] (defaults to {!Http.found_302}). See also
      {!val:Req.service_redirect}. *)

  (** {2:pre_client_errors Client error responses} *)

  val bad_request_400 :
    ?explain:string -> ?reason:string -> ?headers:Http.headers -> unit ->
    ('a, t) result
  (** [bad_request ?explain ?reason ()] is an {!empty} response with
      status {!Http.bad_request_400}. *)

  val unauthorized_401 :
    ?explain:string -> ?reason:string -> ?headers:Http.headers -> unit ->
    ('a, t) result
  (** [unauthorized ?explain ?reason ()] is an {!empty} response with
      status {!Http.unauthorized_401}. *)

  val forbidden_403 :
    ?explain:string -> ?reason:string -> ?headers:Http.headers -> unit ->
    ('a, t) result
  (** [forbidden ?explain ?reason] is an {!empty} response with
      status {!Http.forbidden_403}. *)

  val not_found_404 :
    ?explain:string -> ?reason:string -> ?headers:Http.headers -> unit ->
    ('a, t) result
  (** [not_found ?explain ?reason] is an {!empty} response with
      status {!Http.not_found_404}. *)

  val method_not_allowed_405 :
    ?explain:string -> ?reason:string -> ?headers:Http.headers ->
    allowed:Http.meth list -> unit -> ('a, t) result
  (** [method_not_allowed ~allowed] is an {!empty} response with status
      {!Http.method_not_allowed_405}. It sets the {!Http.allow} header
      to the [allow]ed methods (which can be empty). *)

  val gone_410 :
    ?explain:string -> ?reason:string -> ?headers:Http.headers -> unit ->
    ('a, t) result
  (** [not_found ?explain ?reason] is an {!empty} response with
      status {!Http.gone_410}. *)

  (** {2:pre_server_errors Server error responses} *)

  val server_error_500 :
    ?explain:string -> ?reason:string -> ?headers:Http.headers -> unit ->
    ('a, t) result
    (** [server_error ?explain ?reason] is an {!empty} response with
        status {!Http.server_error_500}. *)

  val not_implemented_501 :
    ?explain:string -> ?reason:string -> ?headers:Http.headers -> unit ->
    ('a, t) result
    (** [server_error ?explain ?reason] is an {!empty} response with
        status {!Http.not_implemented_501}. *)

  (** {1:error_map Error mapper} *)

  val map_errors : ?only_empty:bool -> (t -> t) -> t -> t
  (** [map_errors f r] maps [r] with [f] if [r]'s status
      is a 4XX or 5XX. If [only_empty] is [true]
      (defaults to [false]) it does so only on empty body responses. *)
end

(** HTTP requests. *)
module Req : sig

  (** {1:body Request bodies} *)

  type body = unit -> (bytes * int * int) option
  (** The type for request bodies.

      Bodies are blocking functions pulled by services to yield byte
      chunks of data of the request body as [Some (bytes, first, len)]
      values. The bytes value must not be modified and is readable
      from [first] to [first+len] until the next call to the
      function. The function returns [None] at the end of stream. *)

  val empty_body : body
  (** [empty_body] is an empty body. *)

  val body_to_string : body -> string
  (** [body_to_string b] accumulates the body to a string. *)

  (** {1:req HTTP Requests} *)

  type t
  (** The type for HTTP requests. *)

  val v :
    ?init:t -> ?body:body -> ?body_length:int option -> ?headers:Http.headers ->
    ?meth:Http.meth -> ?path:Http.path -> ?query:string option ->
    ?request_target:string -> ?service_path:Http.path ->
    ?version:Http.version -> unit -> t
  (** [v ~init ()] is an HTTP request with given attributes and for those
      that are unspecified the ones of [init] (defaults
      to {!default}).

      {b Important.} This is not checked by the module but clients of
      this function, at least connectors, should maintain these
      invariant:
      {ul
      {- [request_target] is the raw request target, still percent
         encoded.}
      {- [Path.concat service_path path] should represent the path of
         [request_target].}
      {- [query] (if any) should correspond to the query of [request_target].}}

      Routing function may tweak paths but it's a good idea to keep
      [request_target] unchanged. *)

  val default : t
  (** [default] is a request whose [body] is
      {!empty_body}, [body_length] is [None], [headers] is
      {!Http.Headers.empty}, [meth] is [`GET], [path] is [[""]],
      [query] is [None], [request_target] is ["/"], [sevice_path] is [[""]],
      version is [(1,1)]. *)

  val body : t -> body
  (** [body r] is [r]'s body. *)

  val body_length : t -> int option
  (** [body_length r] is [r]'s request body length (if known). *)

  val headers : t -> Http.headers
  (** [headers r] is [r]'s HTTP headers. Includes at least
      the {!Http.host} header. *)

  val meth : t -> Http.meth
  (** [meth r] is [r]'s
      {{:https://tools.ietf.org/html/rfc7231#section-4}HTTP method}. *)

  val path : t -> Http.path
  (** [path r] {b should} be (see {!v}) the absolute path of
      {!request_target}, {{!Http.Path.strip_prefix}stripped}
      by {!service_path}. *)

  val query : t -> string option
  (** [query r] {b should} be (see {!v}) the query (without the ['?'])
      of {!request_target}. Note that query string may be the empty string
      which is different from [None] (no ['?'] in the request target).
      To decode the query (and handle those that are [POST]ed)
      see {!to_query}. *)

  val request_target : t -> string
  (** [request_target] is [r]'s
      {{:https://tools.ietf.org/html/rfc7230#section-5.3}request
      target}. This should be the raw request, still percent encoded.
      Note that you usually rather want to use the convenience {!path}
      and {!query} which {b should} (see {!v}) derived from this value. *)

  val service_path : t -> Http.path
  (** [service_path r] is the path on which the root of the service
      is served. This is usually set by the connector. The {!path} value
      of [r] is usually the path mentioned in {!request_target} stripped
      by the service path. *)

  val version : t -> Http.version
  (** [version r] is [r]'s
      {{:https://tools.ietf.org/html/rfc7230#section-2.6}HTTP version}. *)

  (** {b FIXME.} now that we have [init] in [v] consider removing those. *)

  val with_headers : Http.headers -> t -> t
  (** [with_headers hs r] is [r] with headers [hs]. *)

  val with_body : body_length:int option -> body -> t -> t
  (** [with_body blen b r] is [r] with body length [blen] and body [b]. *)

  val with_path : Http.path -> t -> t
  (** [with_path p r] is [r] with path [p]. *)

  val with_service_path : Http.path -> t -> t
  (** [with_service_path p r] is [r] with service path [r]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf req] prints and unspecified representation of [req]
      on [ppf] but guarantees not to consume the {!val:body}. *)

  val echo : ?status:Http.status -> t -> Resp.t
  (** [echo r] returns [r] as a 404 [text/plain] document (and by
      doing so violates numerous HTTP's musts). This includes the
      request body, which is consumed by the service.  *)

  (** {1:deconstruct Request deconstruction and responses}

      Request deconstruction helpers. These functions directly
      error with responses that have the right statuses and empty
      bodies. *)

  (** {2:header_decoding Header decoding} *)

  val decode_header :
    Http.name -> (string -> ('a, string) result) -> t ->
    ('a option, Resp.t) result
  (** [decode_header h dec r] decodes header [h] (if any) in [r].
      Errors with {!Http.bad_request_400} in case of decoding errors. *)

  (** {2:method_constraints Method constraints} *)

  (** Method constraints. *)
  module Allow : sig

    type req = t
    (** See {!Req.t}. *)

    type 'a t = Http.meth * 'a
    (** The type for method constraints. *)

    val meths : 'a t list -> req -> ('a, Resp.t) result
    (** [allow ms r] is:
        {ul
        {- [Ok (Req.meth r)] if [List.mem (Req.meth r, Req.meth r) ms]}
        {- [Error _] with a {!Http.method_not_allowed_405}
           response otherwise.}} *)

    (** {1:constraint Constraints} *)

    val connect : [> `CONNECT] t
    val delete : [> `DELETE] t
    val get : [> `GET] t
    val head : [> `HEAD] t
    val options : [> `OPTIONS] t
    val other : string -> 'a ->  'a t
    val patch : [> `PATCH] t
    val post : [> `POST] t
    val put : [> `PUT] t
    val trace : [> `TRACE] t
  end

  (** {2:cookies Cookies} *)

  val find_cookie : name:string -> t -> (string option, string) result
  (** [find_cookie ~name r] is the value of cookie [name] or [None] if
      undefined in [r]. Errors on header or cookie decoding errors. *)

  (** {2:service_forwarding Service forwarding}

      {b FIXME.}
      {ul
      {- LIKELY REMOVE ALL THAT, leave these things to {!Kurl}}
      {- Not sure this is a good terminology.}
      {- Introduce a variation where you push [n] segments
         on the service path.}} *)

  val service_redirect : ?explain:string -> int -> Http.path -> t -> Resp.t
  (** [service_redirect status p r] redirects [r] to the service path [p] (this
      means [r]'s {!service_path} is prefixed to [p]) with status [status].
      See also {!Resp.redirect}. *)

  val forward_service : strip:Http.path -> t -> (t, Resp.t) result
  (** [forward_service ~strip r] is:
      {ul
      {- [Ok r'] with [r'] the request [r] with a {!path} made
         of [r]'s path stripped by [strip] and a {!service_root}
         made of [r]'s service root concatenated with [strip].}
      {- [Error _] with a {!Http.not_found_404} if
         {{!Http.Path.strip_prefix}stripping} results in [None].}}

      {b FIXME.} Because of the new behaviour of
      {!Http.Path.strip_prefix} on root. This may introduce empty path
      segments that did not exist originally in the request when one
      concatenates the service root and the path. Would that be an
      argument to let [] also represent the root path ? *)

  val to_service : strip:Http.path -> t -> (t, Resp.t) result
  (** [to_service ~strip r] strips [strip] from [r]'s path and
      appends it to the service root. Errors with {!Http.not_found_404}
      if stripping [strip] results in [None]. *)

  (** {2:queries Queries}

      {b Warning.} {!Http.type-query} values are untrusted,
      you need to properly validate their data. *)

  val to_query : t -> (Http.query, Resp.t) result
  (** [to_query r] extracts a query from [r]. This is
      {ul
      {- [Ok q] with [q] parsed from [Req.query r] if [r]'s
         method is [`GET] or [`HEAD].}
      {- [Ok q] with [q] parsed from the request body on
         other methods and the content type is
         {!Http.Mime_type.application_x_www_form_urlencoded} or
         TODO multipart. In this case the {!Req.query} is ignored.}
      {- [Error _] with a:
      {ul
      {- {!Http.unsupported_media_type_415} response if the content-type
         is unsupported}
      {- {!Http.bad_request_400} reponse on decoding errors.}}}} *)

  (** {2:clean Path cleaning}

      There's more than one way to handle empty segments and trailing
      slashes in request paths. The scheme proposed here simply always
      redirects to paths in which all empty segments, and thus
      trailing slashes, are removed; except on the root path. The
      advantage is that no elaborate file extension logic on the final
      segment is needed to route file serving. *)

  val clean_path : t -> (t, Resp.t) result
  (** [clean_path r] is:
      {ul
      {- [Ok r] if [r]'s path is [[]], [[""]] or if it has no empty segment.}
      {- [Error _] with a {!Http.moved_permanently_301} to [r]'s path without
         empty segments or the root if that results in the empty path.}}

      {b Warning.} This cleaning does not touch dot segments or
      percent-encoded directory separators that may be present in the
      path. You should still use
      {{!Http.Path.to_absolute_filepath}that function} or
      {!to_absolute_filepath} for mapping paths to file paths. *)

  (** {2:file_path Absolute file paths} *)

  val to_absolute_filepath :
    ?strip:Http.path -> root:Http.fpath -> t -> (Http.fpath, Resp.t) result
  (** [absolute_filepath ~strip ~root r] determines an absolute
      file path strictly rooted in [root] by
      {{!Http.Path.strip_prefix}stripping} [strip] (defaults to
      [[""]]) from [r]'s {!path},
      {{!Http.Path.to_absolute_filepath} converting} the result
      to an absolute filepath and
      {{!Http.Path.prefix_filepath}prefixing} it with [docroot].

      Errors with {!Http.not_found_404} if stripping [strip]
      results in [None] and {!Http.bad_request_400} if the
      absolute path conversion fails. *)
end

type service = Req.t -> Resp.t
(** The type for services. Maps requests to responses. Note that
    services should not raise exceptions (but connectors should be
    prepared to handle spurious ones). *)

(** {1:connector Connector} *)

(** Connector commonalities. *)
module Connector : sig

  (** {1:log_msg Log messages}

      These message are emited by connector to track activity and
      report unexpected messages. *)

  type dur_ns = int64
  (** The type for integer nanosecond duration. *)

  type log_msg =
  [ `Service_exn of exn * Stdlib.Printexc.raw_backtrace
  | `Connector_exn of exn * Stdlib.Printexc.raw_backtrace
  | `Connection_reset
  | `Trace of dur_ns * Req.t option * Resp.t option ]
  (** The type for connector log messages. These *)

  val no_log : log_msg -> unit
  (** [no_log] is [Fun.const ()]. *)

  val default_log :
    ?ppf:Format.formatter -> trace:bool -> unit -> (log_msg -> unit)
  (** [default_log ~ppf ~trace] logs message on [ppf] (defaults to
      {!Format.err_formatter}) and [`Trace] messages
      iff [trace] is true. *)

  val pp_log_msg : Format.formatter -> log_msg -> unit
  (** [pp_log_msg] is a unspecified formatter for log messages. *)

  val pp_exn_backtrace :
    kind:string ->
    Format.formatter -> exn * Printexc.raw_backtrace -> unit
  (** [pp_exn_backtrace] is a formatter for exception backtraces. *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
