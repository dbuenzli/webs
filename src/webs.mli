(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Web service interface.

    Consult the {{!page-web_service_howto}web service howto} for a quick
    steps to run your first service. A few tools are in are in
    {!Webs_kit} and gateway connectors to run services can be found
    {{!page-index.connectors}here}.

    Open the module to use it. It defines only modules and types in
    your scope. *)

(** {1 Services} *)

(** HTTP nuts and bolts.

    Only types, values and codecs.

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

  (**/**)
  val string_subrange : ?first:int -> ?last:int -> string -> string
  val string_starts_with : prefix:string -> string -> bool
  val string_lowercase : string -> string
  (**/**)

  (** {1:encs Encodings} *)

  (** Base64 codec.

      Codecs Base64 according to
      {{:https://tools.ietf.org/html/rfc4648#section-4}RFC 4684}. *)
  module Base64 : sig
    val encode : ?url:bool -> string -> string
    (** [encode s] is the [base64] (or [base64url] if [url] is [true])
        encoding of [s] . *)

    val decode : ?url:bool -> string -> (string, int) result
    (** [decode s] is the [base64] (or [base64url] if [url] is [true])
        decode of [s]. In case of error the integer indicates:
        {ul
        {- Either the byte index of the error for an invalid
           alphabet character error}
        {- Or the length of the string if the string
           length is not a multiple of [4]}} *)
  end

  (** Percent-encoding codec.

      Codecs percent-encoding according to
      {{:https://tools.ietf.org/html/rfc3986#section-2.1}RFC 3986}.

      {b Note.} This should not be used for URI query strings and
      [application/x-www-form-urlencoded] which is slightly different.
      The {!Query} module handle that. *)
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

    (** {1:names Names}

      In HTTP it is often required to perform US-ASCII case
      insensitive comparisons on HTTP tokens.  Values of type {!name}
      represent US-ASCII lowercased HTTP tokens. *)

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
        [s] is not a header name.  Use {!of_string} if you
        need to handle failures. *)

    val equal : name -> name -> bool
    (** [equal n n'] is [true] iff [n] and [n'] are equal. *)

    val compare : name -> name -> int
    (** [compare] is {!String.compare}. *)

    (** {1:conv Converting} *)

    val of_string : string -> (name, string) result
    (** [of_string s] decodes a name from [s]. *)

    val to_string : name -> string
    (** [to_string n] encodes a name for [s]. *)

    val pp : Format.formatter -> name -> unit
    (** [pp] is an unspecified formatter for header names. *)
  end

  (** {1:mime_type MIME types} *)

  type mime_type = string
  (** The type for MIME types. *)

  (** A few ubiquitous MIME types.

      See also {!Webs_kit.Mime_type}. *)
  module Mime_type : sig

    type t = mime_type
    (** See {!mime_type}. *)

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
  end

  (** {1:methods Methods} *)

  type meth =
  [ `GET (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.1}[GET]} *)
  | `HEAD (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.2}[HEAD]} *)
  | `POST (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.3}[POST]} *)
  | `PUT (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.4}[PUT]} *)
  | `DELETE
  (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.5}[DELETE]} *)
  | `CONNECT
     (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.6}[CONNECT]} *)
  | `OPTIONS
     (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.7}[OPTIONS]} *)
  | `TRACE
     (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.8}[TRACE]} *)
  | `PATCH (** {{:http://tools.ietf.org/html/rfc5789}[PATCH]} '*)
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

  (** {1:header Headers and header values} *)

  type headers
  (** The type for HTTP headers. Maps header names to string values
      such that for:
      {ul
      {- Single valued headers, the value is the string.}
      {- Multi-valued headers, the values are comma [','] separated
         as per specification. Use {!H.values_of_string} on the string.}
      {- {!H.set_cookie} header, must be treated specially since it
         can be repeated but does not follow the syntax of
         multi-valued headers. The values are stored in the string
         separated by ['\x00'] values, use {!H.set_cookie} and
         {!H.values_of_set_cookie_value} to handle the field.  On
         {!H.encode} this results in separate [set-cookie] headers.}} *)

  (** HTTP headers and values. *)
  module H : sig

    (** {1:headers Headers} *)

    val name : string -> name
    (** [name n] is {!Http.Header_name.v}. *)

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

    (** {1:lookups Header lookups} *)

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

    val digits_of_string : string -> (int, string) result
    (** [digits_of_string s] is the non-empty sequence of
        {{:https://tools.ietf.org/html/rfc5234#appendix-B.1}decimal digits}
        [s] as a non-negative integer. *)

    val digits_to_string : int -> string
    (** [digits_to_string n] is the non-negative integer [n] as a sequence
        of decimal digits.

        @raise Invalid_argument if [n] is negative. *)

    val is_token : string -> bool
    (** [is_token s] is [true] iff [s] in an HTTP
        a {{:https://tools.ietf.org/html/rfc7230#section-3.2.6}token}. *)

    (** {1:standard_names Standard header names} *)

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
  end

  (** {1:cookie Cookies} *)

  (** Cookies. *)
  module Cookie : sig

    type atts
    (** The type for cookie attributes. *)

    val atts :
      ?max_age:int -> ?domain:string -> ?path:string -> ?secure:bool ->
      ?http_only:bool -> ?same_site:string -> unit -> atts
    (** [atts] are the given cookie attributes. If an attribute is
        absent it is not mentioned in the cookie sepcification except
        for [same_site] which are always specified and respectively
        defaults to ["strict"] and [http_only] with default to [true].

        {b TODO.} Should we default [path] to ["/"] and use {!path} ?
        Also maybe [secure] should default to [true] it's just if we
        devise a good dev/production configuration story. *)

    val atts_default : atts
    (** [atts_default] is [atts ()]. *)

    val encode : ?atts:atts -> name:string -> string -> string
    (** [encodes ~atts name s] encodes a cookie
        with attributes [atts] (defaults) to {!atts_default} for
        {!H.set_cookie}. *)

    val decode_list : string -> ((string * string) list, string) result
    (** [decode_list s] parses the
        {{:https://tools.ietf.org/html/rfc6265#section-4.2.1}cookie string}
        of a {!H.cookie} header. *)
  end

  (** {1:paths Paths} *)

  type path = string list
  (** The type for {e absolute} URI paths. This is a percent decoded
      list of path segments. Path segments can be empty ([""]). The
      empty list denotes absence of path. The root path ["/"] is
      represented by the list [[""]] while ["/a"] is represented by
      [["a"]].

      {b WARNING.} You should never concatenate these segments with
      a separator to get a filepath, the segments are not guaranteed
      not to contain directory separators. Use the function
      {!Path.to_undotted_filepath}. *)

  (** Paths. *)
  module Path : sig

    type t = path
    (** The type for absolute URI paths. See {!path}. *)

    val drop_prefix : path -> path -> path
    (** [drop_prefix pre p] drops prefix [pre] from [p]. If
        [pre] is not a prefix of [p] this is [p].

        TODO talk about final "". *)

    val undot_and_compress : path -> path
    (** [undot_and_compress p] removes ["."] and [".."]  according to
        the RFC3986
        {{:https://tools.ietf.org/html/rfc3986#section-5.2.4}algorithm}
        and suppresses non final empty [""] segments. *)

    val to_undotted_filepath : path -> (string, string) result
    (** [to_undotted_filepath p] is an absolute filepath for
        [undot_and_compress p]. It returns "/". Errors if any of the
        path segments contains a stray slash or backslash. The result
        always uses [/] as a directory separator regardless of the
        platform. *)

    val prefix_filepath : string -> string -> string
    (** [prefix_filepath p0 p1] prefixes [p1] by [p0] avoiding producing
        empty segs. *)

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

        The empty list is special cased and yields [""].
        {!path_to_undotted_filepath} to convert paths to file paths.

        Here are a few examples:
        {ul
        {- [decode [] = ""]}
        {- [decode [""] = "/"]}
        {- [decode [""; ""] = "//"]}
        {- [decode ["a";"b";"c"] = "/a/b/c"]}
        {- [decode ["a";"b";"";"c";] = "/a/b//c"]}
        {- [decode ["a";"b";"c";""] = "/a/b/c/"]}
        {- [decode ["a";"b";"c";" "] = "/a/b/c/%20"]}
        {- [decode ["a";"b";"c";"";""] = "/a/b/c//"]}
        {- [decode ["a"; "b/"; "c"] = "/a/b%2F/c"]}
        {- [decode ["r\xC3\xC9volte"] = "/r%C3%C9volte"]}
        {- [decode ["a"; "not%20"; "b"] = "/a/not%2520/b"]}} *)

    val decode : string -> (path, string) result
    (** [decode s] decodes an
        {{:https://tools.ietf.org/html/rfc7230#section-2.7}[absolute-path]}
        to its
        {{:http://tools.ietf.org/html/rfc3986#section-2.1}percent-decoded}
        list of segments. By definition of [absolute-path] the list of
        segments is never empty.

        Here are a few examples:
        {ul
        {- [decode "/" = Some [""]]}
        {- [decode "//" = Some ["",""]]}
        {- [decode "/a/b/c" = Some ["a";"b";"c"]]}
        {- [decode "/a/b//c" = Some ["a";"b";"";"c"]]}
        {- [decode "/a/b/c/" = Some ["a";"b";"c";""]]}
        {- [decode "/a/b/c/%20" = Some ["a";"b";"c";" "]]}
        {- [decode "/a/b//c//" = Some ["a";"b";"";"c";"";""]]}
        {- [decode "/a/b%2F/c" = Some ["a"; "b/"; "c"]]}
        {- [decode "/r%C3%C9volte" = Some ["r\xC3\xC9volte"]]}
        {- [decode  "/a/not%2520/b" = Some ["a"; "not%20"; "b"]]}
        {- [decode "" = Error _]}
        {- [decode "a/b/c" = Error _]}} *)


    val pp : Format.formatter -> path -> unit
    (** [pp] is an unspecified formatter for paths. *)
  end

  (** {1:query queries} *)

  (** URI query and [application/x-www-form-urlencoded] codec.

      Encodes and decodes
      according to the
      {{:https://url.spec.whatwg.org/#application/x-www-form-urlencoded}
      whatwg URL standard}. *)
  module Query : sig

    (** {1:queries Queries} *)

    type t
    (** The type for queries as key-values maps. Both keys and values
        are properly decoded. Note that keys can map to
        multiple values. *)

    val empty : t
    (** [empty] is the empty key-values map. *)

    val mem : string -> t -> bool
    (** [mem k q] is true [iff] key [k] is bound in [q]. *)

    val def : string -> string -> t -> t
    (** [def k v q] is [q] with [k] bound only to value [v]. *)

    val add : string -> string -> t -> t
    (** [add k v q] is [q] with value [v] appended to
        [k]'s values (or {!set} if there was no binding for [k]). *)

    val undef : string -> t -> t
    (** [undef k q] is [q] with [k] unbound. *)

    val find : string -> t -> string option
    (** [find k q] is the value of [k]'s first binding in [q], if any. *)

    val find_all : string -> t -> string list
    (** [find_all k q] are all the values bound to [k] or the empty
        list if [k] is unbound. *)

    val fold : (string -> string -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold f q acc] folds over all the key-value bindings. For keys
        with multiple values folds over them in the same order
        as {!find_all}. *)

    val keep_only_first : t -> t
    (** [keep_only_first q] is [q] with only the first value kept in bindings
        with multiple values. *)

    (** {1:conv Converting} *)

    val decode : string -> t
    (** [decode s] decodes the [application/x-www-form-urlencoded]
        [s] to a query.  If a key is defined more than once,
        the first definition is returned by {!find} and the
        left-to-right order preserved by {!find_all}'s list. The input
        string is not checked for UTF-8 validity. *)

    val encode : t -> string
    (** [encode q] encodes [q] to an [application/x-www-form-urlencoded]
        string. *)

    val pp : Format.formatter -> t -> unit
    (** [pp] is an unspecified formatter for queries. *)
  end

  (** {1:status_codes Status codes} *)

  type status = int
  (** The type for
      {{:https://tools.ietf.org/html/rfc7231#section-6}HTTP status codes}. *)

  (** Statuses. *)
  module Status : sig
    type t = status
    (** See {!status}. *)

    val reason_phrase : status -> string
    (** [reason_phrase s] is [s]'s reason phrase. *)

    val pp : Format.formatter -> status -> unit
    (** [pp] is an unspecified formatter for statuses. *)
  end

  (** {2:informational Informational 1xx} *)

  val s100_continue : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.2.1}[100]} *)

  val s101_switching_protocols : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.2.2}[101]} *)

  (** {2:sucessful Sucessful 2xx} *)

  val s200_ok : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.3.1}[200]} *)

  val s201_created : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.3.2}[201]} *)

  val s202_accepted : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.3.3}[202]} *)

  val s203_non_authoritative_information : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.3.4}[203]} *)

  val s204_no_content : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.3.5}[204]} *)

  val s205_reset_content : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.3.6}[205]} *)

  val s206_partial_content : status
  (** {{:https://tools.ietf.org/html/rfc7233#section-4.1}[206]} *)

  (** {2:redirection Redirection 3xx} *)

  val s300_multiple_choices : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.4.1}[300]} *)

  val s301_moved_permanently : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.4.2}[301]} *)

  val s302_found : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.4.3}[302]} *)

  val s303_see_other : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.4.4}[303]} *)

  val s304_not_modified : status
  (** {{:https://tools.ietf.org/html/rfc7232#section-4.1}[304]} *)

  val s305_use_proxy : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.4.5}[305]} *)

  val s307_temporary_redirect : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.4.7}[307]} *)

  (** {2:client_error Client Error 4xx} *)

  val s400_bad_request : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.1}[400]} *)

  val s401_unauthorized : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6}[401]} *)

  val s402_payement_required : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.2}[402]} *)

  val s403_forbidden : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.3}[403]} *)

  val s404_not_found : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.4}[404]} *)

  val s405_not_allowed : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.5}[405]} *)

  val s406_not_acceptable : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.6}[406]} *)

  val s407_proxy_authentication_required : status
  (** {{:https://tools.ietf.org/html/rfc7235#section-3.2}[407]} *)

  val s408_request_time_out : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.7}[408]} *)

  val s409_conflict : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.8}[409]} *)

  val s410_gone : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.9}[410]} *)

  val s411_length_required : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.10}[411]} *)

  val s412_precondition_failed : status
  (** {{:https://tools.ietf.org/html/rfc7232#section-4.2}[412]} *)

  val s413_payload_too_large : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.11}[413]} *)

  val s414_uri_too_long : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.12}[414]} *)

  val s415_unsupported_media_type : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.13}[415]} *)

  val s416_range_not_satisfiable : status
  (** {{:https://tools.ietf.org/html/rfc7233#section-4.4}[416]} *)

  val s417_expectation_failed : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.14}[417]} *)

  val s418_i'm_a_teapot : status
  (** {{:https://tools.ietf.org/html/rfc2324#section-2.3.2}[418]} *)

  val s426_upgrade_required : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.15}[436]} *)

  (** {2:server_error Server Error 5xx} *)

  val s500_server_error : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.6.1}[500]} *)

  val s501_not_implemented : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.6.2}[501]} *)

  val s502_bad_gateway : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.6.3}[502]} *)

  val s503_service_unavailable : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.6.4}[503]} *)

  val s504_gateway_time_out : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.6.5}[504]} *)

  val s505_http_version_not_supported : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.6.6}[505]} *)

  (** {1:versions Versions} *)

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

(** Service environments {b TODO} try to do without.

    Environments are heterogenous key-value maps attached to requests and
    responses. They can be used by services and layers to store and share
    data. *)
module Env : sig

  (** {1:keys Keys} *)

  type 'a key
  (** The type for keys for whose lookup value is of type ['a]. *)

  val key : unit -> 'a key
  (** [key ()] is a new key. *)

  (** {1:env Environments} *)

  type t
  (** The type for environments. *)

  val empty : t
  (** [empty] is the empty environent. *)

  val is_empty : t -> bool
  (** [is_empty e] is [true] iff [e] is empty. *)

  val mem : 'a key -> t -> bool
  (** [mem k e] is [true] iff [k] is bound in [e]. *)

  val add : 'a key -> 'a -> t -> t
  (** [add k v e] is [e] with [k] bound to [v]. *)

  val remove : 'a key -> t -> t
  (** [remove k e] is [e] with [k] unbound. *)

  val find : 'a key -> t -> 'a option
  (** [find k e] is the value of [k]'s binding in [e], if any. *)

  val get : 'a key -> t -> 'a
  (** [get k m] is the value of [k]'s binding in [m].
      @raise Invalid_argument if [k] is not bound in [m]. *)

  val override : t -> by:t -> t
  (** [override m ~by] are the definitions of both [m] and [m'] with
      those of [~by] taking over. *)
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
    ?env:Env.t -> ?version:Http.version -> ?body_length:int option ->
    ?body:body -> ?headers:Http.headers -> Http.meth -> string -> t
  (** [v meth request_target] is an HTTP request with method [meth],
      request target [request_target], [headers] (defaults to
      {!Http.H.empty}), [body] (defaults to {!empty_body}),
      [body_length] (defaults to [None] or [Some 0] if body is
      {!empty_body}) and version (defaults to (1,1)). *)

  val version : t -> Http.version
  (** [version r] is [r]'s
      {{:https://tools.ietf.org/html/rfc7230#section-2.6}HTTP version}. *)

  val meth : t -> Http.meth
  (** [meth r] is [r]'s
      {{:https://tools.ietf.org/html/rfc7231#section-4}HTTP method}. *)

  val request_target : t -> string
  (** [request_target] is [r]'s
      {{:https://tools.ietf.org/html/rfc7230#section-5.3}request
      target}.  This should be the raw request, still percent encoded.
      Note that you usually rather want to use the convenience {!path}
      and {!query} which are derived from this value. *)

  val path : t -> Http.path
  (** [path r] is the absolute path of {!request_target} as a list of
      path segments or the empty list if there is none.
      Note that the path segements are percent decoded, this means they
      {e may} have stray '/' embedded.

      {b TODO} say something about the other request target cases in
      particular should we map "*" on [] or ["/"] ?  Also should we undot
      and compress by default, seems like a good safer default, people who
      want the gory details can use {!request_target}. However that's
      likely not a good idea since this people make people 200 these
      paths. *)

  val query : t -> string option
  (** [query r] is the query
      (without the ['?']) of {!request_target}. Note that query string may
      be the empty string which is different from [None] (no ['?']
      in the request target). *)

  val headers : t -> Http.headers
  (** [headers r] is [r]'s HTTP headers. Includes at least
      the {!Http.H.host} header. *)

  val body_length : t -> int option
  (** [body_length r] is [r]'s request body length (if known). *)

  val body : t -> body
  (** [body r] is [r]'s body. *)

  val with_headers : Http.headers -> t -> t
  (** [with_headers hs r] is [r] with headers [hs]. *)

  val with_body : body_length:int option -> body -> t -> t
  (** [with_body blen b r] is [r] with body length [blen] and body [b]. *)

  val with_path : Http.path -> t -> t
  (** [with_path p r] is [r] with path [p]. *)

  val env : t -> Env.t
  (** [env r] is [r]'s environment. {b TODO} consider removing. *)

  val with_env : Env.t -> t -> t
  (** [with_env e r] is [r] with environment [e]. {b TODO} consider
      removing. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf req] prints and unspecified representation of [req]
      on [ppf] but guarantees not to consume the {!body}. *)
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
    ?env:Env.t -> ?version:Http.version -> ?explain:string -> ?reason:string ->
    ?body:body -> ?headers:Http.headers -> Http.status -> t
  (** [v status headers body] is a response with given
      [status], [headers] (defaults to {!Http.H.empty}), [body] (defaults
      to {!empty_body}, [reason] (defaults to {!Http.status_reason_phrase})
      and [version] (defaults to [(1,1)]), [explain] is a server side [reason]
      it is not put on the wire.

      {b Note.} If [body] is [body_empty] (default) a {!Http.H.content_length}
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

  val env : t -> Env.t
  (** [env r] is [r]'s environment. {b TODO.} Remove. *)

  val with_env : Env.t -> t -> t
  (** [with_env e r] is [r] with environment [e]. {b TODO.} Remove. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints an unspecified representation of [r] on [ppf] but
      guarantees not to consume the {!body}. *)

  (** {1:pre_canned Pre-canned responses}

      The optional [set] argument of the functions below always
      {!Http.H.override} those the function computed. *)

  val result : (t, t) result -> t
  (** [result r] is either response. *)

  (** {2:pre_canned_content Content responses} *)

  val content :
    ?explain:string -> ?set:Http.headers -> mime_type:Http.mime_type ->
    int -> string -> t
  (** [content ~mime_type st s] responds [s] with content type
      [mime_type] and status [st]. Sets {!Http.H.content_type} and
      {!Http.H.content_length} appropriately. *)

  val text : ?explain:string -> ?set:Http.headers -> int -> string -> t
  (** [text] responds with UTF-8 encoded plain text, i.e.
      {!content} with {!Http.Mime_type.text_plain}. *)

  val html : ?explain:string -> ?set:Http.headers -> int -> string -> t
  (** [html] responds with UTF-8 encoded HTML text, i.e.
      {!content} with {!Http.Mime_type.text_html}.  *)

  val json : ?explain:string -> ?set:Http.headers -> int -> string -> t
  (** [json] responds with JSON text, i.e. {!content} with
      {!Http.Mime_type.application_json}. *)

  val octets : ?explain:string -> ?set:Http.headers -> int -> string -> t
  (** [octets] responds with octets, i.e. {!content} with
      {!Http.Mime_type.application_octet_stream}. *)

  (** {2:pre_redirect Redirect responses} *)

  val redirect : ?explain:string -> ?set:Http.headers -> int -> string -> t
  (** [redirect status loc] redirects to {{!Http.H.location}location} [loc]
      with status [status] (defaults to {!Http.s302_found}). *)

  (** {2:pre_canned_errors Errors responses} *)

  val not_allowed :
    ?explain:string -> ?reason:string -> ?set:Http.headers ->
    allow:Http.meth list -> unit -> t
    (** [not_allowed ~allowed] is an {!empty} response with status
        {!Http.s405_not_allowed}. It sets {!Http.H.allow} to the
        [allow]ed methods (which can be empty). *)

  (** {2:echo Echo} *)

  val echo : ?status:Http.status -> Req.t -> t
  (** [echo r] returns [r] as a 404 [text/plain] document (and by
      doing so violates numerous HTTP's musts). This includes the
      request body, which is consumed by the service.  *)
end

type service = Req.t -> Resp.t
(** The type for services. Maps requests to responses. Note that
    services should not raise exceptions. *)

(** {1:connector Connector} *)

(** Connector commonalities. *)
module Connector : sig

  (** {1:log_msg Log messages}

      These message are emited by connector to track activity and
      report unexpected messages. *)

  type log_msg =
  [ `Service_exn of exn * Stdlib.Printexc.raw_backtrace
  | `Connector_exn of exn * Stdlib.Printexc.raw_backtrace
  | `Trace of Req.t option * Resp.t option ]
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
