(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Web service interface.

    [Webs] defines a generic, {e low-level}, interface for web
    {{!service}services} implemented in OCaml. The interaction between
    the service and the HTTP web server that runs it is mediated by a
    connector whose details depend on the web server. [Webs] defines a
    generic connector {{!Connector.t}interface} that connectors are
    encouraged to reuse.

    Consult the {{!basics}basics} and {{!examples}examples} of use.
    Open the module to use it, it defines only modules and types in
    your scope.

    [Webs] is inspired by {{:https://github.com/yesodweb/wai}WAI}.

    {e Release %%VERSION%% - %%MAINTAINER%% } *)

(** {1 Dictionaries} *)

type dict
(** The type for type-safe heterogenous dictionaries. *)

(** Dictionaries.

    A dictionary is a set of {{!keys}keys} mapping to typed values. *)
module Dict : sig

  (** {1:keys Keys} *)

  type 'a key
  (** The type for dictionary keys whose lookup value is ['a]. *)

  val key : unit -> 'a key
  (** [key ()] is a new dictionary key. *)

  (** {1:dict Dictionaries} *)

  type t = dict
  (** The type for dictionaries. *)

  val empty : dict
  (** [empty] is the empty dictionary. *)

  val is_empty : dict -> bool
  (** [is_empty d] is [true] iff [d] is empty. *)

  val mem : 'a key -> dict -> bool
  (** [mem k d] is [true] iff [k] has a mapping in [d]. *)

  val add : 'a key -> 'a -> dict -> dict
  (** [add k v d] is [d] with [k] mapping to [v]. *)

  val rem : 'a key -> dict -> dict
  (** [rem k d] is [d] with [k] unbound. *)

  val find : 'a key -> dict -> 'a option
  (** [find d k] is [k]'s mapping in [d], if any. *)

  val get : 'a key -> dict -> 'a
  (** [get k d] is [k]'s mapping in [d].

      {b Raises.} [Invalid_argument] if [k] is not bound in [d]. *)
end

(** {1 Services} *)

open Rresult

type req
(** The type for HTTP requests. *)

type resp
(** The type for HTTP responses. *)

type service = req -> resp
(** The type for services. Maps HTTP requests to responses. *)

type layer = service -> service
(** The type for layers. Maps services to services. *)

(** HTTP types, fragment codecs and constants.

    [HTTP] provides types, helper functions and constants
    for HTTP {{!versions}versions}, {{!methods}methods}, {{!headers}headers},
    {{!status_codes}status codes} and {{!paths}paths}.

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
module HTTP : sig

  (** {1:versions Versions} *)

  type version = int * int
  (** The type for representing
      {{:https://tools.ietf.org/html/rfc7230#section-2.6}[HTTP-version]}
      fields. Both integers must be in the interval [\[0;9\]]. *)

  val decode_version : string -> version option
  (** [decode_version s] decodes an
      {{:https://tools.ietf.org/html/rfc7230#section-2.6}[HTTP-version]}
      field from [s]. *)

  val encode_version : version -> string
  (** [encode_version v] encodes an
      {{:https://tools.ietf.org/html/rfc7230#section-2.6}[HTTP-version]}
      field value for [v].

      @raise Invalid_argument if [v]'s integer are not in the interval
      [\[0;9\]]  *)

  val pp_version : Format.formatter -> version -> unit
  (** [pp_version ppf v] prints [v] an unspecified representation
      of [v] on [ppf].

      @raise Invalid_argument if [v]'s integer are not in the interval
      [\[0;9\]]  *)

  (** {1:methods Methods} *)

  type meth =
    [ `GET (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.1}[GET]} *)
    | `HEAD (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.2}[HEAD]} *)
    | `POST (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.3}[POST]} *)
    | `PUT (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.4}[PUT]} *)
    | `DELETE (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.5}[PUT]} *)
    | `CONNECT
       (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.6}[CONNECT]} *)
    | `OPTIONS
       (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.7}[OPTIONS]} *)
    | `TRACE
       (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.8}[TRACE]} *)
    | `PATCH (** {{:http://tools.ietf.org/html/rfc5789}[PATCH] *)
    | `Other of string
      (** other {{:https://tools.ietf.org/html/rfc7230#section-3.2.6}token} *)
    ]
  (** The type for HTTP {{:https://tools.ietf.org/html/rfc7231#section-4}
      request methods}. *)

  val decode_meth : string -> meth option
  (** [decode_meth s] decodes a
      {{:https://tools.ietf.org/html/rfc7231#section-4.1}[method]} for
      [s]. *)

  val encode_meth : meth -> string
  (** [encode_meth m] encodes a
      {{:https://tools.ietf.org/html/rfc7231#section-4.1}[method]} for
      [v].

      @raise Invalid_argument if [m] is [`Other t] and [t] is not
      a {{:https://tools.ietf.org/html/rfc7230#section-3.2.6}token}. *)

  val pp_meth : Format.formatter -> meth -> unit
  (** [pp_method ppf m] prints and unspecified representation of [m]
      on [ppf].

      @raise Invalid_argument if [m] is [`Other t] and [t] is not
      a {{:https://tools.ietf.org/html/rfc7230#section-3.2.6}token}. *)

  (** {1:headers Headers} *)

  type headers
  (** The type for header maps.

      A header map represents a list of HTTP headers. It maps
      header field names to their value. *)

(*
  val encode_headers : ?buf:bytes -> ((bytes * int * int) option -> unit) ->
    headers -> unit
  (** [encode_headers ?buf dst hs] encodes the headers [hs] by calling
      [dst]. If [?buf] is specified this buffer is used with the
      destination. *)
*)

  val pp_headers : Format.formatter -> headers -> unit
  (** [pp_headers ppf hs] prints an unspecified representation of [hs] on
      [ppf]. *)

  (** Headers.

      Consult the header {{!hcst}name constants}.

      {b FIXME.} This is not the right way to do this.  Internally we
      are using multi by default, but in fact we should not as we may
      not know if a field is multi or not (because of extensions). The
      decoder should simply combine multi into single by using [',']
      *except* for [Set-Cookie], we then leave it to the user to
      decompose a multi value using {!H.decode_multi_value} on values that
      make sense to her to treat that way (which may not be standard).
      However we then need to provide special functions for [Set-Cookie]. *)
  module H : sig

    (** {1:names Header names} *)

    type name
    (** The type for lowercased HTTP header
        {{:https://tools.ietf.org/html/rfc7230#section-3.2}
        field-name}s. *)

    val name : string -> name
    (** [name s] is a header name from [s]. See also the header
        {{!hcst}name constants}.

        @raise Invalid_argument if [s] is not a
        {{:https://tools.ietf.org/html/rfc7230#section-3.2}field-name}
        Use {!decode_name} if you need to handle failures. *)

    val name_equal : name -> name -> bool
    (** [name_equal n n'] is [true] iff [n] and [n'] are equal. *)

    val decode_name : string -> name option
    (** [decode_name s] decodes and lowercases a
        {{:https://tools.ietf.org/html/rfc7230#section-3.2}[field-name]}
        from [s]. *)

    val encode_name : name -> string
    (** [encode_name n] encodes a
        {{:https://tools.ietf.org/html/rfc7230#section-3.2}[field-name]}
        for [n]. *)

    val pp_name : Format.formatter -> name -> unit
    (** [pp_name ppf n] prints an unspecified representation of [n]
        on [ppf]. *)

    (** {1:map Header maps} *)

    val empty : headers
    (** [empty] is the empty header map. *)

    val is_empty : headers -> bool
    (** [is_empty hs] is [true] iff [hs] is the empty header map. *)

    val is_def : name -> headers -> bool
    (** [is_def n hs] is [true] iff [n] is bound in [hs]. *)

    val undef : name -> headers -> headers
    (** [undef n hs] is [hs] without a binding for [n]. *)

    (** {1:def Single-valued header definition} *)

    val def : name -> string -> headers -> headers
    (** [def n v hs] is [hs] with [n] bound to [v]. *)

    val redef : name -> (string option -> string option) -> headers -> headers
    (** [redef n f hs] is:
        {ul
        {- [def n v] if [f (find name m) = Some v].}
        {- [undef hs] if [f (find name m) = None].}} *)

    val def_if_undef : name -> string -> headers -> headers
    (** [def_if_undef n v hs] is:
        {ul
        {- [hs] if [is_def n hs] is [true].}
        {- [def n v hs] otherwise.}} *)

    val find : name -> headers -> string option
    (** [find n hs] is the value of header [n] in [hs] (if defined). If
        [n] is multi-valued and defined, the result of
        {!encode_multi_value} on the list of values is returned.

        {b Warning.} Do not use this function with {!set_cookie}. *)

    val get : name -> headers -> string
    (** [get n hs] is like {!find} but @raise Invalid_argument if
        [n] is undefined in [hs]. *)

    (** {1:mdef Multi-valued header definition} *)

    val def_multi : name -> string list -> headers -> headers
    (** [def_multi n vs hs] is [hs] with [n] bound to the multi-value [vs].

        @raise Invalid_argument if [vs] is [[]]. *)

    val redef_multi : name -> (string list option -> string list option) ->
      headers -> headers
    (** [redef n f hs] is:
        {ul
        {- [def_multi n vs] if [f (find_multi name m) = Some vs].}
        {- [undef hs] if [f (find_multi name m) = None].}}

        @raise Invalid_argument if [f] returns [Some []]. *)

    val def_if_undef_multi : name -> string list -> headers -> headers
    (** [def_if_undef_multi n vs hs] is:
        {ul
        {- [hs] if [is_def n hs] is [true].}
        {- [def_multi n vs hs] otherwise.}}

        @raise Invalid_argument if [vs] is [[]]. *)

    val find_multi : name -> headers -> string list option
    (** [find_multi n hs] is the value of header [n] in [hs] as a
        multi-valued header (if defined). If [n] is not multi-valued
        a singleton list is returned with the header value. Returned
        lists are {e never} empty. *)

    val get_multi : name -> headers -> string list
    (** [get n hs] is like {!find_multi} but @raise Invalid_argument if
        [n] is undefined in [hs]. *)

    (** {1:fold Map folding, predicates and conversion}

        The following functions only work on the multi valued representation
        as automatic {!encode_multi_value} is not feasable because of
        {!set_cookie}. *)

    val fold : (name -> string list -> 'a -> 'a) -> headers -> 'a -> 'a
    (** [fold f m acc] folds [f] over the bindings of [hs] starting with
        [acc]. *)

    val iter : (name -> string list -> unit) -> headers -> unit
    (** [iter f hs] iters [f] over the bindings of [hs]. *)

    val for_all : (name -> string list -> bool) -> headers -> bool
    (** [for_all p hs] is [true] iff all binding in [hs] satisfy [p]. *)

    val exists : (name -> string list -> bool) -> headers -> bool
    (** [exists p hs] is [true] iff there is a binding in [hs]
        that satisfies [p]. *)

    val keep_if : (name -> string list -> bool) -> headers -> headers
    (** [keep_if p hs] is the bindings of [hs] that satisfiy [p]. *)

    val partition : (name -> string list -> bool) -> headers ->
      headers * headers
    (** [partition p hs] is [(t, f)] where [t] are the bindings
        of [hs] that do satisfy [p] and [f] those that do not satisfy
        [p]. *)

    val bindings : headers -> (name * string list) list
    (** [bindings hs] is [hs]'s list of bindings. *)

    val cardinal : headers -> int
    (** [cardinal hs] is [hs]'s number of bindings in [hs]. *)

    (** {1:values Header values} *)

    val decode_multi_value : string -> string list
    (** [decode_multi_value s] splits the string [s] at [',']
        characters and trims the resulting strings from
        {{:https://tools.ietf.org/html/rfc7230#section-3.2.3} optional
        whitespace}. Note that by definition the result is never the
        empty list, the function returns [[""]] on [""]. *)

    val encode_multi_value : string list -> string
    (** [encode_multi_value vs] is [String.concat "," vs] but
        @raise Invalid_argument if [vs] is [[]]. *)

    (** {1:hcst Header name values} *)

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

  (** {1:status_codes Status codes} *)

  type status = int
  (** The type for
      {{:https://tools.ietf.org/html/rfc7231#section-6}HTTP status codes}. *)

  val status_reason_phrase : status -> string
  (** [status_reason_phrase s] is [s]'s reason phrase. *)

  val pp_status : Format.formatter -> status -> unit
  (** [pp_status ppf s] prints an unspecified representation of
      [s] on [ppf]. *)

  (** {2 {{:https://tools.ietf.org/html/rfc7231#section-6.2}
      Informational 1xx}} *)

  val s100_continue : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.2.1}[100]} *)

  val s101_switching_protocols : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.2.2}[101]} *)

  (** {2 {{:https://tools.ietf.org/html/rfc7231#section-6.3}
      Sucessful 2xx}} *)

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

  (** {2 {{:https://tools.ietf.org/html/rfc7231#section-6.4}
      Redirection 3xx}} *)

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

  (** {2 {{:https://tools.ietf.org/html/rfc7231#section-6.5}
      Client Error 4xx}} *)

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

  val s414_uri_too_long  : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.12}[414]} *)

  val s415_unsupported_media_type : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.13}[415]} *)

  val s416_range_not_satisfiable : status
  (** {{:https://tools.ietf.org/html/rfc7233#section-4.4}[416]} *)

  val s417_expectation_failed : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.14}[417]} *)

  val s426_upgrade_required : status
  (** {{:https://tools.ietf.org/html/rfc7231#section-6.5.15}[436]} *)

  (** {2 {{:https://tools.ietf.org/html/rfc7231#section-6.6}
      Server Error 5xx}} *)

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

  (** {1:paths Paths} *)

  type path = string list
  (** The type for HTTP
      {{:https://tools.ietf.org/html/rfc7230#section-2.7}[absolute-path]}s
      represented by its non-empty list of URI
      {{:https://tools.ietf.org/html/rfc3986#section-3.3}[segment]}s.
      Note that URI segments can be empty; in particular the
      absolute path ["/"] is the list with a single empty segment and
      is thus represented by the list [[""]]. *)

  val decode_path : string -> path option
  (** [decode_path s] decodes an
      {{:https://tools.ietf.org/html/rfc7230#section-2.7}[absolute-path]}
      to its
      {{:http://tools.ietf.org/html/rfc3986#section-2.1}percent-decoded}
      list of segments. By definition of [absolute-path] the list of
      segments is never empty, ["/"] is represented by the singelton
      empty segment [[""]].

      Here are a few examples:
      {ul
      {- [decode_path "/" = Some [""]]}
      {- [decode_path "//" = Some ["",""]]}
      {- [decode_path "/a/b/c" = Some ["a";"b";"c"]]}
      {- [decode_path "/a/b//c" = Some ["a";"b";"";"c"]]}
      {- [decode_path "/a/b/c/" = Some ["a";"b";"c";""]]}
      {- [decode_path "/a/b/c/%20" = Some ["a";"b";"c";" "]]}
      {- [decode_path "/a/b//c//" = Some ["a";"b";"";"c";"";""]]}
      {- [decode_path "/a/b%2F/c" = Some ["a"; "b/"; "c"]]}
      {- [decode_path "/r%C3%C9volte" = Some ["r\xC3\xC9volte"]]}
      {- [decode_path  "/a/not%2520/b" = Some ["a"; "not%20"; "b"]]}
      {- [decode_path "" = None]}
      {- [decode_path "a/b/c" = None]}} *)

  val encode_path : path -> string
  (** [encode_path p] encodes an
      {{:https://tools.ietf.org/html/rfc7230#section-2.7}[absolute-path]}
      for [p] as follows:
      {ol
      {- In each segment {{:http://tools.ietf.org/html/rfc3986#section-2.1}
         percent-encode} any byte that is not
         {{:http://tools.ietf.org/html/rfc3986#section-2.3}[unreserved]},
         {{:http://tools.ietf.org/html/rfc3986#section-2.2}[sub-delims]},
         [':'] or ['@'] and hence producing a valid URI
         {{:http://tools.ietf.org/html/rfc3986#section-3.3}[segment]}}
      {- Prepends each segment with a ['/']}
      {- Concatenate the result.}}

      @raise Invalid_argument if [p] is the empty list.

      Here are a few examples:
      {ul
      {- [encode_path [""] = "/"]}
      {- [encode_path [""; ""] = "//"]}
      {- [encode_path ["a";"b";"c"] = "/a/b/c"]}
      {- [encode_path ["a";"b";"";"c";] = "/a/b//c"]}
      {- [encode_path ["a";"b";"c";""] = "/a/b/c/"]}
      {- [encode_path ["a";"b";"c";" "] = "/a/b/c/%20"]}
      {- [encode_path ["a";"b";"c";"";""] = "/a/b/c//"]}
      {- [encode_path ["a"; "b/"; "c"] = "/a/b%2F/c"]}
      {- [encode_path ["r\xC3\xC9volte"] = "/r%C3%C9volte"]}
      {- [encode_path ["a"; "not%20"; "b"] = "/a/not%2520/b"]}
      {- [encode_path []] raises [Invalid_argument].}} *)

  val pp_path : ?human:bool -> unit -> Format.formatter -> path -> unit
  (** [pp_path human () ppf p] prints an unspecified representation of
      [p] on [ppf] if [human] is [true] (defaults to [false]) the
      is prettified for human presentation rather than debugging. *)

  (** {1:misc Miscenalleous codecs} *)

  val decode_digits : string -> int option
  (** [decode_digits s] decodes a sequence of
      {{:https://tools.ietf.org/html/rfc5234#appendix-B.1}[DIGIT]}s to
      a positive integer. Returns [None] on overflows *)

  val encode_digits : int -> string
  (** [encode_digits i] encodes [i] as a sequence of
      {{:https://tools.ietf.org/html/rfc5234#appendix-B.1}[DIGIT]}s.

      @raise Invalid_argumnet if [i] is negative. *)

  val is_token : string -> bool
  (** [is_token s] is [true] iff [s] in an HTTP
      a {{:https://tools.ietf.org/html/rfc7230#section-3.2.6}token}. *)

(*
  (** {1 Request target} *)

  type request_target =
    | `Origin_form of path * string option
    | `Absolute_form of string
    | `Authority_form of string
    | `Asterisk_form
  (** The type for HTTP
      {{:https://tools.ietf.org/html/rfc7230#section-5.3}[request-target]}. *)
*)

end

(** HTTP requests. *)
module Req : sig

  (** {1 HTTP Requests} *)

  type body = unit -> (bytes * int * int) option
  (** The type for request bodies.

      A body is a function that yields byte chunks of the request body
      as [Some (bytes, pos, len)] values. The bytes value must not be
      modified and is readable from [pos] to [pos+len] until the next
      call to the function. The function returns [None] at the end of
      stream. *)

  type t = req
  (** The type for HTTP requests. *)

  val v : ?dict:dict -> HTTP.version -> HTTP.meth -> path:HTTP.path ->
    ?query:string -> HTTP.headers -> ?body_len:int -> body -> req
  (** [v dict v m p q hs bl b] is an HTTP request with the given components.
      [uri_query] and [body_len] defaults to [None]. [dict] defaults to
      {!Dict.empty}. *)

  val version : req -> HTTP.version
  (** [version r] is [r]'s
      {{:https://tools.ietf.org/html/rfc7230#section-2.6}HTTP version}. *)

  val meth : req -> HTTP.meth
  (** [meth r] is [r]'s
      {{:https://tools.ietf.org/html/rfc7231#section-4}HTTP method}. *)

  val path : req -> HTTP.path
  (** [path r] is [r]'s HTTP request target
      {{:http://tools.ietf.org/html/rfc7230#section-5.3.1}[origin-form]}'s
      [absolute-path]. This is the requested URI's path as decoded by
      {!HTTP.decode_path}.

      {b TODO} say something about the other request target cases. *)

  val query : req -> string option
  (** [query r] is [r]'s HTTP request target
      {{:http://tools.ietf.org/html/rfc7230#section-5.3.1}[origin-form]}'s
      [query] (if any). This is the requested URI query string
      without the ['?']. The query string may be the empty
      string which is different from [None] (no ['?'] in the URI).

      TODO say something about decoding, make something better. *)

  val headers : req -> HTTP.headers
  (** [headers r] is [r]'s HTTP headers. Includes at least
      the {!HTTP.host} header. *)

  val dict : req -> dict
  (** [dict r] is [r]'s dictionary. Initially empty it can be used be
      services and layers to store and share data. *)

  val body_len : req -> int option
  (** [body_len r] is [r]'s request body length (if known). *)

  val body : req -> body
  (** [body r] is [r]'s body. *)

  val with_headers : req -> HTTP.headers -> req
  (** [with_headers req hs] is [req] with headers [hs]. *)

  val with_body : req -> body_len:int option -> body -> req
  (** [with_body req blen b] is [req] with body length [blen] and body [b].
      {!body_length} defaults to [None]. *)

  val with_path : req -> HTTP.path -> req
  (** [with_path req p] is [req] with path [p]. *)

  val with_dict : req -> dict -> req
  (** [with_dict req d] is [req] with dictionary [d]. *)

  val pp : Format.formatter -> req -> unit
  (** [pp ppf req] prints and unspecified representation of [req]
      on [ppf] but guarantees not to consume the {!body}. *)
end

(** HTTP response. *)
module Resp : sig

  (** {1 Response body} *)

  type consumer = (bytes * int * int) option -> unit
  (** The type for response consumers.

      Response consumers are provided by the connector to get the body
      produced by a response. Response bodies call the consumer with
      [Some (byte, pos, len)], to output data. The bytes are not
      modified by the consumer and only read from [pos] to [pos+len].
      The producer signals the end of body by calling the consumer
      with [None]. *)

  (** The type for response bodies. *)
  type body =
    | Stream of (consumer -> unit)
    | File of (int * int) option * string

  val stream_body : (consumer -> unit) -> body
  (** [stream_body producer] is a response body stream produced by
      [producer] on the consumer it will be given to. *)

  val string_body : string -> body
  (** [string_body s] is a reponse body made of string [s]. *)

  val empty_body : body
  (** [empty_body s] is an empty body. *)

  val file_body : ?range:int * int -> string -> body
  (** [file_body name] is a body that will contain the bytes of filename
      [name] as resolved by the connector. If [range] is specified
      as [(pos, len)], only the bytes from [pos] to [pos+len] will
      be transmitted. *)

  val pp_body : Format.formatter -> body -> unit
  (** [pp_body ppf b] prints an unspecified representation of [b]'s
      specification on [ppf]. If body is a stream, does not consume it. *)

  (** {1 Response} *)

  type t = resp
  (** The type for responses. *)

  val v : ?version:HTTP.version -> HTTP.status -> HTTP.headers -> body -> resp
  (** [v ~version status headers body] is a response with the given
      [version] (defaults to [(1,1)]), [status], [headers] and
      [body]. *)

  val version : resp -> HTTP.version
  (** [version r] is [r]'s version. *)

  val status : resp -> HTTP.status
  (** [status r] is [r]'s status. *)

  val headers : resp -> HTTP.headers
  (** [headers r] is [r]'s headers. *)

  val body : resp -> body
  (** [body r] is [r]'s body. *)

  val with_status : resp -> HTTP.status -> resp
  (** [with_status r s] is [r] with status [s]. *)

  val with_headers : resp -> HTTP.headers -> resp
  (** [with_headers r s] is [r] with headers [hs]. *)

  val with_body : resp -> body -> resp
  (** [with_body r b] is [r] with body [b]. *)

  val pp : Format.formatter -> resp -> unit
  (** [pp ppf t] prints an unspecified represntation of [r] on [ppf] but
      guarantees not to consume the {!body}. *)
end

(** {1 Connectors} *)

(** Generic web server connector interface.

    Connectors are not required to follow this interface. They are
    however encouraged to do so, to make it easier for clients to
    be able to use multiple connectors for their service. *)
module Connector : sig

  (** {1 Connector} *)

  type error = [ `Webserver of R.msg | `Connector of R.msg
               | `Service of R.exn_trap ]
  (** The type for connector errors. See {!err}. *)

  type conf = Dict.t
  (** The type for connector configuration. *)

  type t = conf -> service -> (unit, error) result
  (** The type for connectors.

      A call [connect c s] to a connector configures the connection
      to the webserver [c] and installs the service [s].

      {b Implementation duties.} The implementation
      of [connect] should satisfy the following constraints.
      {ul
      {- If the connector uses {{!Conf.std}standard} configuration
          keys it must respect their semantics.}
      {- The [connect] function must never raise an exception.}
      {- If a service raises an exception it must be caught. The
         connector should then try to respond to the web server
         with a {!HTTP.s_server_error}. The connector should
         try to log the uncaught exception, for example by using
         the {!service_exn_log} key.}
      {- If a configuration, connector or web server connection
         error occurs [connect] should return with [Error] with an
         appropriate description.}
      {- If the connector has an implementation dependent way of
         closing the connection with the web server it should return
         from [connect] with [Ok].}
      {- [connect] should not return [Ok] unless it has no
         initiated service requests. TODO should we standarize
         a few daemon error conds ?}} *)

  (** {1:stdconf Standard configuration keys}

      Connectors can define their own keys but should favor
      {{!std}standard} ones if appropriate. See also the {!Webs_unix}
      configuration keys. *)

  val sendfile_header : string Dict.key
  (** [sendfile_header] is key defining a header name.

      {b Purpose.} If a connector supports this key, file response bodies are
      not handled by the connector. The filename is returned to the
      web server in this header. Use for example ["x-accel-redirect"] for
      {{:nginx.org}nginx} or ["x-sendfile"] for Apache with
      {{:https://tn123.org/mod_xsendfile/}mod_xsendfile}
      and {{:http://www.lighttpd.net/}Lightppd}. *)

  val service_exn_log : Format.formatter Dict.key
  (** [error_log] is a key defining a formatter to log service errors.

        {b Purpose.} If a connector supports this key, it must log
      uncaught service exceptions to the given formatter. *)

  (** {1:err Errors} *)

  val pp_error : Format.formatter -> error -> unit
  (** [pp_error ppf e] prints an unspecified representation of [e]
      on [ppf]. *)

  val open_error : ('a, error) result -> ('a, [> error]) Rresult.result
  val error_to_msg : ('a, error) result -> ('a, Rresult.R.msg) Rresult.result
end

type connector = Connector.t
(** The type for generic webserver connectors. *)

(** {1:basics Basics}

    A web service is simply a function mapping HTTP requests to HTTP
    responses. Here is an example of a simple service that entices to
    revolt on any [GET] request and errors on any other method:
{[
open Webs

let revolt r = match Req.meth r with
| `GET ->
    let text = "text/plain; charset=UTF-8" in
    let headers = HTTP.H.(empty |> def content_type text) in
    Resp.v HTTP.s200_ok headers (Resp.string_body "Revolt!\n")
| _ ->
    let headers = HTTP.H.(empty |> def allow (HTTP.encode_meth `GET)) in
    Resp.v HTTP.s405_not_allowed headers Resp.empty_body
]} *)


(** {1:examples Examples}

    {2:cgi Simple service via CGI}


    {2:timing Timing the response}

    Forward req and analyse return headers, if
    content-type = text/html, add comment with time. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
