(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Headers.

    A datatype to handle the quirky HTTP headers.

    @canonical Webs.Http.Headers *)

(** {1:header_names Header names} *)

(** Header names.

    See also {{!Webs.Http.Headers.standard_header_names}standard header names}.

    HTTP header names are US-ASCII case insensitive. Values of type
    {!Name.t} represent US-ASCII lowercased HTTP
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-tokens}tokens}. *)
module Name : sig

  type t = private string
  (** The type for lowercased HTTP header field
      {{:https://www.rfc-editor.org/rfc/rfc9110#name-field-names}
      field-name}s. *)

  val make : string -> t
  (** [make s] is a name from [s]. Raises [Invalid_argument] if
      [s] is not a header name.  Use {!decode} if you
      need to handle failures. *)

  (** {1:converting Converting} *)

  val decode : string -> (t, string) result
  (** [decode s] decodes
      {{:https://www.rfc-editor.org/rfc/rfc9110#name-field-names}HTTP
      [field-name]} from [s]. *)

  val encode : t -> string
  (** [encode n] encodes [n] to an
      {{:https://www.rfc-editor.org/rfc/rfc9110#name-field-names}HTTP
      [field-name]}. *)

  (** {1:predicates Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal] tests names for equality. *)

  val compare : t -> t -> int
  (** [compare] is a total order on names compatible with {!equal}. *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats header names for inspection. *)

  (**/**)
  val unsafe_of_string : string -> t (* FIXME remove that *)
end

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
       separated by ['\x00'] values. Use {!Headers.append_set_cookie} and
       {!Headers.values_of_set_cookie_value} to handle the field. Encoders
       must write the cookies in separate {!set_cookie} headers.}} *)

val empty : t
(** [empty] has no header definition. *)

val define : Name.t -> string -> t -> t
(** [define n v headers] is [headers] with [n] defined to [v]. *)

val define_if_some : Name.t -> string option -> t -> t
(** [define_if_some n o headers] is [headers] with [n] defined to [v] if [o]
    is [Some v] and [headers] otherwise. *)

val define_if_undefined : Name.t -> string -> t -> t
(** [define_if_undefined n v headers] is [headers] with [n] defined to [v]
    if [n] is not defined in [headers]. *)

val undefine : Name.t -> t -> t
(** [undefine n headers] is [headers] with [n] undefined. *)

val append_value : Name.t -> string -> t -> t
(** [append_value n v headers] appends [v] to the multi-valued header [n] in
    [headers]. *)

val append_set_cookie : string -> t -> t
(** [append_set_cookie c headers] adds a {!set_cookie} header with value [c].
    This appends to {!set_cookie}, see {!t}. *)

val override : t -> by:t -> t
(** [override headers ~by] are the headers of both [headers] and [by]
    with those of [by] taking over. *)

(** {1:lookups Lookups} *)

val find : ?lowervalue:bool -> Name.t -> t -> string option
(** [find n headers] is the value of [n] in [headers] (if any).
    If [lowervalue] is [true] (defaults to [false])
    the US-ASCII uppercase letter are mapped on lowercase.

    If [n] is a multi-valued header use {!values_of_string} on
    the result. If [n] is {!set_cookie} you must use
    {!values_of_set_cookie_value}. *)

val find_or_error : ?lowervalue:bool -> Name.t -> t -> (string, string) result
(** [find_or_error] is like {!find}. Except if the header is absent it
    returns an error message of the form ["%s: No such header"]. *)

val get : ?lowervalue:bool -> Name.t -> t -> string
(** [get n headers] is like {!find} but raises [Invalid_argument] if [n]
    is not defined in [headers]. *)

val fold : (Name.t -> string -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f headers acc] folds [f] over the bindings of [headers] starting with
    [acc]. *)

(** {2:lookup_header_specific Header specific}

    {b TODO} Try to get rid of this. *)

val request_body_length : t ->
  ([ `Length of int | `Chunked ], string) result
(** [request_body_length headers] determines the message body length of
    a request (the rules for responses is a bit different) as per
    {{:https://www.rfc-editor.org/rfc/rfc9112#name-message-body-length}
    HTTP/1.1 specification}, by looking at the {!content_type} and
    {!transfer_encoding} in [headers]. *)

val decode_host : Webs__scheme.t -> t -> (string * int, string) result
(** [decode_host scheme headers] decodes the {!host} header into a
    hostname and a port number. If no port number is found in the
    header one is derived from [scheme] with {!Scheme.tcp_port}.
    Errors if the header is missing or on decoding errors. *)

val for_connector : t -> Webs__body.t -> t
(** [for_connector headers body] are the headers of [headers] prepared for
    output by a connector that will write a request or response with body
    [body]. It performs the logic described in the
    {{!page-connector_conventions.service_responses}service responses}
    and {{!page-connector_conventions.client_requests}client
    requests} conventions. *)

(** {1:values Header values} *)

val values_of_set_cookie_value : string -> string list
(** [values_of_set_cookie_value v] decodes [v] as stored in
    by {!append_set_cookie} in the {!t} type to a list of cookies. *)

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

val value_trim_ows : string -> string
(** [value_trim_ows] trims starting and ending
    {{:https://www.rfc-editor.org/rfc/rfc9110#whitespace}optional
    whitespace} (OWS). *)

(** {1:converting Converting} *)

val header_of_string : string -> (Name.t * string, string) result
(** [header_of_string s] parses a header from string [s] assumed
    to be in the form [name: value]. *)

(** {1:predicates Predicates and comparisons} *)

val is_empty : t -> bool
(** [is_empty headers] is [true] iff [headers] is has no definition. *)

val mem : Name.t -> t -> bool
(** [mem n headers] is [true] iff [n] is defined in [headers]. *)

val equal : t -> t -> bool
(** [equal] tests sets of headers for equality. Header values are tested
    using binary equality, for multi-valued headers and
    {!append_set_cookie} order matters. *)

val compare : t -> t -> int
(** [compare] is a total order on headers compatible with {!equal}. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf headers] prints an unspecified representation of [headers]
    on [ppf]. *)

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

val sec_fetch_site : Name.t
(** {{:https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Sec-Fetch-Site}[sec-fetch-site]} *)

val sec_fetch_mode : Name.t
(** {{:https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Sec-Fetch-Mode}[sec-fetch-mode]} *)

val sec_fetch_user : Name.t
(** {{:https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Sec-Fetch-User}[sec-fetch-user]} *)

val sec_fetch_dest : Name.t
(** {{:https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Sec-Fetch-Dest}[sec-fetch-dest]} *)

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
