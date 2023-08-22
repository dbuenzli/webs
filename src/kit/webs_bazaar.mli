(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Unstable service tools.

    {b Warning.} Some of the stuff here remains unconvincing and needs a
    cleanup. It's likely to break in the future. Use at your own risk. *)

open Webs

(** {1:res Ressource helpers} *)

(** Ressource handling tools *)
module Res : sig

  (** Human readable URLs for identifying resources.

      This module provides generic support for handling a
      [/name/id[/path]] URL scheme to identify resources.

      The stable [id] identifier is preceeded by a human readable
      [name] that can change over time without breaking the resource
      resolution. The [name] segment is simply ignored for resolving
      the resource and redirected to the right one if it doesn't match
      the name of the ressource identified by [id] at the time of resolution.

      Let [id] and [name] be the identifier and name of a given ressource.
      The URL response scheme should be:

      {ol
      {- [/name/id[/path]] is determined by the resource specific logic.}
      {- [/name'/id[/path]] with [name'] not an identifier
         and different from [name] is a
         {{!Webs.Http.Status.moved_permanently_301}301} moved permanently
         to [/name/id[/path]].}
      {- [/name] is either a {{!Webs.Http.Status.not_found_404}404} not
         found or, if the corresponding [id] is easy to find,
         a {{!Webs.Http.Status.moved_permanently_301}301} moved permanently
         to [/name/id].}
      {- [/id[/path]] is a {{!Webs.Http.Status.moved_permanently_301}301}
         moved permanently to [/name/id[/path]].}
      {- [/s0/s1[/path]] with both [s0] and [s1] not parsing
         as identifiers can be used freely without interfering with
         the scheme.}}

      Note that for this to work the [name] and [id] syntax must be
      distinguishable unambiguously.

      The {!Named.resolve} function handles point 2 and 4 in a generic manner.
      The {!Named.name_of_string} function provides a (non-injective)
      naming scheme for arbitrary strings that is human readable,
      compatible with URL path segment syntax and unambiguously
      distinguishable from non-negative numerical identifers
      (e.g. {!Res.Id}). *)
  module Named : sig

    (** {1:resolution Resolution} *)

    val resolve :
      ?eq:('name -> 'name -> bool) ->
      get_res:('id -> ('res, Http.Response.t) result) ->
      res_name:('res -> 'name) ->
      res_url:('name -> 'id -> string) ->
      req_name:'name option ->
      req_id:'id -> unit -> ('res, Http.Response.t) result
    (** [resolve ~eq ~get_res ~res_name ~res_url ~req_name ~req_id ()]
        implements the 301 redirection logic spelled out in the preamble of
        this module.

        Given a request's parsed identifier [req_id] and name [req_name]
        (if any):
        {ol
        {- If [get_res req_id] is [Error _] this is returned by the function.}
        {- If [get_res req_id] is [Ok res], let [n] be [res_name res].}
        {- If [req_name] is [None] a
           {{!Webs.Http.Status.moved_permanently_301}301}
           moved permanently [Error _] to [res_url n req_id] is returned.}
        {- If [req_name] is [Some n'] and [eq n n'] is [false] a
           {{!Webs.Http.Status.moved_permanently_301}301} moved permanently
           [Error _] to [res_url n req_id] is returned.}
         {- Otherwise, [Ok res] is returned.}}

        [eq] defaults to {!Stdlib.( = )}. [res_url] must return
        a value suitable for the {!Webs.Http.Headers.location} header. *)

    (** {1:names Names} *)

    type name = string
    (** The type for names. *)

    val name_of_string : string -> name
    (** [name_of_string s] maps [s] to a name that can be used as a human
        readable URL path segment. It transforms [s] to a sequence of ['-']
        separated tokens. A ['-'] is appended to the empty string and to
        sequences of US-ASCII digits only so that they can be distinguished
        from numerical identifiers (e.g. {!Res.Id}). More precisely this:

        {ol
        {- Replaces all US-ASCII characters except letters and digits by ['-'].}
        {- Lowercases upper US-ASCII letters.}
        {- Suppresses initial and final ['-']s and compresses consecutive
           ['-'].}
        {- Appends ["-"] if the result is empty or only has US-ASCII digits.}}

        The function is not injective. It is idempotent and preserves all
        non US-ASCII UTF-8 characters. It is meant to be applied before
        percent encoding. *)
  end

  (** Numerical identifiers.

      This module parses sequences of US-ASCII digits to non-negative
      [int] values. Leading zeros are not allowed, see {!Id.of_string} for
      the full details. *)
  module Id : sig

    (** {1:errors Errors} *)

    type error = [ `Overflow | `Syntax ]
    (** The type for parse errors. See {!of_string}. *)

    val error_message : error -> string
    (** [error_message e] is an english error message for [e]. *)

    val error_to_resp : error -> Http.Response.t
    (** [error_to_resp e] is a {{!Webs.Http.Status.bad_request_400}400} bad
        request for [e]. The response's reason is determined by
        {!error_message}. *)

    (** {1:ids Identifiers} *)

    type t = int
    (** The type for non-negative identifiers.  *)

    val to_string : t -> string
    (** [to_string id] are the US-ASCII digits for [id]. Raises
        [Invalid_argument] if [id] is negative. *)

    val of_string : string -> (t, error) result
    (** [of_string s] is:
        {ul
        {- [Ok id], if [s] is a sequence of US-ASCII digits and [id]
           its non-negative decimal interpretation.}
        {- [Error `Overflow] in case [s] is only made of US-ASCII digits
            but there are too many of them to fit in a non-negative [int].}
        {- [Error `Syntax] in case [s] contains any non US-ASCII digits or
            if [s] has a leading [0] and is not ["0"].}} *)

    val decode : string -> (t, Http.Response.t) result
    (** [decode s] is [Result.map_error error_to_resp (of_string s)]. *)
  end
end

(** {1:url URL management} *)

(** Kinds of URL requests and their formatters.

    This module provides support to describe and handle URL requests
    by multiple client-defined OCaml types in a bidirectional and
    modular manner.

    A {{!Kurl.type-kind}kind} value represents a subset of
    {{!Kurl.type-bare}bare URL requests} by values of a custom request
    OCaml type. Kinds are then used:

    {ul
    {- For request decoding. Kinds are tupled with a
       {{!Kurl.type-service}service} function and {{!Kurl.val-bind}bound} by a
       constant HTTP path in a {{!Kurl.type-tree}service tree}. Given a
       bare URL request, its path is used by the service tree to
       decode the appropriate request value and give it to the
       corresponding service to handle it.}
    {- For response encoding. From the service tree an {{!Kurl.type-fmt}URL
       request formatter} can be derived. Given a custom request value
       and its kind, the formatter can format a well-typed bare URL request
       for your responses without mentioning the URL structure (defined by the
       kind) or binding points (defined by the formatter derived from the
       service tree definition).}}

    {b TODO.}
    {ul
    {- Bare.equal and Kurl.t equal.}
    {- I think the encoding decoding symmetry assumption
       is wrong. Maybe we should get the full request on decode,
       e.g. see next point}
    {- For responding e.g. with 301 we need more context to be
       able to generate urls should we pass an url formatter/service
       to decoder ?}
    {- Integrate fragment part ? If yes, in the [bare] type or on
       formatting functions ? Likely not on [bare], you never get fragments
       to decode so it seems a bit OT.}
    {- Terminology: service or url request tree ?}
    {- The Kurl name doesn't really reflect that this
       is not about URLs but URL {e requests} maybe the module
       name shoud be changed to reflect that. Kreq ? But
       bare doesn't have the headers.}
    {- It's still unclear
       whether we should not push this a bit further and
       integrate all request aspects. This could be
       separated in Kreq and Urlf. But we still need to
       figure out what we want to do with requests that
       are not [URL]able (well with {!Hc} every aspect
       becomes [URL]able). Bare could embody {!Webs.Http.Request.t}.}
    {- Integrate sessions ? Maybe not, here is the reasoning:
       the custom request types should only ever hold what can be specified
       by responses since they need to create these values.}
    {- Better decoding combinators to counter the
       non-dryness/non-tuply/uglyness/granulariy of {!Kurl.type-kind}}} *)
module Kurl : sig

  (** {1:bare Bare URL requests} *)

  type bare
  (** The type for bare URL requests. This embodies an HTTP method, an
      URL path, an URL query and an optional URL path file extension
      used by some URL formatting modes. *)

  val bare :
    ?ext:string -> ?query:Http.Query.t -> Http.Method.t -> Http.Path.t -> bare
  (** [bare m p ~query ~ext] is an URL request to path [p] with method
      [m], query [query] (defaults to {!Webs.Http.Query.empty}) and URL
      path file extension [ext] (defaults to [""]). *)

  (** Bare URL requests. *)
  module Bare : sig

    type t = bare
    (** See {!type-bare}. *)

    val v :
      ?ext:string -> ?query:Http.Query.t -> Http.Method.t -> Http.Path.t -> bare
    (** See {!val-bare}. *)

    val method' : bare -> Http.Method.t
    (** [method' u] is the HTTP method of [u]. *)

    val path : bare -> Http.Path.t
    (** [path u] is the URL path of [u]. *)

    val query : bare -> Http.Query.t
    (** [query u] is the URL query of [u]. *)

    val ext : bare -> string
    (** [ext u] is the optional URL path file extension of [u]. This is
        used by some URL formatting modes. *)

    val with_path : Http.Path.t -> bare -> bare
    (** [with_path p b] is [b] with path [p]. *)

    val of_req : ?ext:string -> Http.Request.t -> bare
    (** [of_req ~ext r] is a bare URL request from [r]. {!Bare.method'} is
        {!Webs.Http.Request.method'}, {!Bare.path} is
        {!Webs.Http.Request.path}, {!Bare.query} is parsed
        {!Webs.Http.Request.query}, [ext] defaults to [""]. *)

    val of_req_referer :
      ?ext:string -> ?method':Http.Method.t -> Http.Request.t ->
      (bare, string) result
    (** [of_req_referer ~ext r] is a bare URL request from [r]. {!Bare.method'}
        is [meth] (defaults to {!Webs.Http.Request.method'}[ r]),
        {!Bare.path} and {!Bare.query} are derived from the
        {!Webs.Http.Headers.referer} header. Errors if [r] has no such
        header or if its parsing fails. *)

    val pp : Format.formatter -> bare -> unit
    (** [pp ppf b] formats an unspecified representation of [b] on
        [ppf]. *)
  end

  (** {2:encoders Encoders} *)

  type 'a enc = 'a -> bare
  (** The type for encoding an URL request value of type ['a] to a bare
      URL request. *)

  (** {2:decoders Decoders } *)

  type 'a dec = bare -> ('a option, Http.Response.t) result
  (** The type for decoding a bare URL request to a request value of
      type ['a]. The path in [bare] url is relative to the service
      tree binding point.

      The function returns:
      {ul
      {- [Ok (Some r)], if the request belongs to the kind of URL requests
         with value [r].}
      {- [Ok None], if the request does not belong to the kind of URL
         requests. This lets other services attached to tree consider the prefix
       or an extension of it.}
      {- [Error r], if the request was meant to belong to the kind of URL
         requests but directly responding with [r] at that point was deemed
         appropriate. Examples are bad request, not allowed, etc. Possibly
         not found but if you want to let other subtrees consider the prefix
         use [Ok None]}}

      {b TODO.}
      {ul
      {- We could generalize over the error. But then we need to carry it
         over everywhere.}} *)

  (** {3:decode_help Decoder helpers}

      {b TODO.} Provide easy redirect. *)

  val allow :
    'a Http.Method.constraint' list -> bare -> ('a, Http.Response.t) result
  (** [meths ms u] is:
        {ul
        {- [Ok (Bare.meth u)] if [List.mem (Bare.meth u, Bare.meth u) ms]}
        {- [Error _] with a {{!Webs.Http.Status.method_not_allowed_405}405} not
         allowed response otherwise.}} *)

  val ok : 'a -> ('a option, 'e) result
  (** [ok v] is [Ok (Some v)]. *)

  val no_match : ('a option, 'e) result
  (** [no_match] is [Ok None]. *)

  (** {1:kinds Kinds} *)

  type 'a kind
  (** The type for kinds of URL request of type ['a]. *)

  val kind :
    ?root_is:[`Dir of string option | `File] ->
    ?name:string -> 'a enc -> 'a dec -> 'a kind
  (** [kind ?root_is ?name enc dec] is a kind using [enc] and [dec] to
      codec bare URL request values. [name] is a name used for debugging
      purposes (defaults to [""]).

      [root_is] defines formatting behaviour of the root path ([[""]])
      relative to the service binding path. With:

      {ul
      {- [`File] the binding path encodes the root. Note that since the
         binding path must be {{!wf_paths}well-formed} it is guaranteed to
         lack a trailing slash. If extension formatting is requested and there
         is one in the encoded bare, it is appended to it. For example
         if the URL kind is bound to path [["mykind"]] and [enc] encodes a
         bare URL with path [[""]] the formatted URL will be ["/mykind"].}
      {- [`Dir seg] the binding path with a trailing slash is the resulting
         path. If extension formatting is requested and [seg] is [Some seg]
         then both segment [seg] (e.g. "index") and the extension are appended,
         otherwise the path is left as is. For example
         if the URL kind is bound to path [["mykind"]] and [enc] encodes a
         bare URL with path [[""]] the formatted URL will be ["/mykind/"]
         or [/mykind/seg] if extension formatting is requested.}}

       {b TODO.} Should [root_is] also affect decoding ?

       {b IMPORTANT.} Kinds have a notion of identity and they
       can only be {{!val-bind}bound} once in a tree.  *)

  (** Kinds of URL requests *)
  module Kind : sig

    type root_is = [`Dir of string option | `File]
    (** See {!val-kind}. *)

    type 'a t = 'a kind
    (** See {!type-kind}. *)

    val v : ?root_is:root_is -> ?name:string -> 'a enc -> 'a dec -> 'a kind
    (** See {!val-kind}. *)

    val name : 'a kind -> string
    (** [kind k] is the name of [k]. *)

    val root_is : 'a kind -> root_is
    (** [root_is k] is root path encoding behaviour for [k], see {!val-kind}. *)

    val enc : 'a kind -> 'a enc
    (** [enc k] is the encoder of [k]. *)

    val dec : 'a kind -> 'a dec
    (** [dec k] is the decoder of [k]. *)

    val equal : 'a kind -> 'a kind -> bool
    (** [equal k0 k1] is [true] iff [k0] and [k1] are the same kind. *)

    (** {1:bare Bare kinds} *)

    val bare : ?root_is:root_is -> ?name:string -> unit -> bare kind
    (** [bare ()] is a bare request kind. Can be used for untyped
        formatting tricks. *)
  end

  val any : bare kind
  (** [any] is [Kind.bare ~name:"any" ~root_is:(`Dir (Some "index")) ()].
      This is a catch-all bare request kind defined. It is
      always added at the root of URL formatters.

      In conjunction with {!Bare.of_req_referer} it allows for example
      to easily format relative URLs for requests performed by HTML
      page components or for request that do not exist in the service
      space like 404.

      You can also bind it at the root of your service tree as the last
      service. It will provide you with a catch all handler. *)

  (** {1:kinded Kinded URL requests} *)

  type t = V : 'a kind * 'a -> t (** *)
  (** The type for an URL request of a given kind. *)

  val v : 'a kind -> 'a ->  t
  (** [v k u] is [V (k, u)]. *)

  (** {1:services Services} *)

  type 'a service
  (** The type for services handling URL requests with a value
      of type ['a], see {!val-service}. *)

  val service : 'b kind -> ('b -> 'a) -> 'a service
  (** [service k f] is a service handling URL requests of kind [k] with
      [f].

      The return type of ['a] is common to all services. Usually this
      is a function which when given arguments common to all services
      effectively handles the request. The service type simply hides
      the first, request kind specific, argument of these
      functions. *)

  val map_service : ('a -> 'b) -> 'a service -> 'b service
  (** [map_service f s] applies [f] to the service of [s]. *)

  (** {1:trees Service trees} *)

  type 'a tree
  (** The type for service trees binding constant HTTP paths to
      {{!type-service}services} of type ['a]. *)

  val empty : unit -> 'a tree
  (** [empty ()] is an empty service tree. *)

  (** {2:req Request handling} *)

  val find_service : 'a tree -> bare -> ('a option, Http.Response.t) result
  (** [find_service t u] finds the service for handling [u]'s
      {!Webs.Http.Request.path}. [Ok None] is returned if no URL request kind
      matched. [Error _] is returned if a URL request kind matched but
      errored. *)

  (** {2:bind Binding services} *)

  val bind : Http.Path.t -> 'a service -> 'a tree -> 'a tree
  (** [bind p s t] is [t] with {{!wf_paths}well-formed} path [p] bound to [s].
      Note shorter bindings to [p] in [t] take precedence, but [s] takes
      precedence over existing bindings at [p]. More precisely:

      {ul
      {- If there is a service [s'] on a prefix of [p] in [t], [s] will
         only be invoked for those requests for which the kind of [s']
         decodes to [Ok None].}
      {- If there is a service [s'] at [p] in [t], [s'] will only be invoked
         for those requests for which the kind of [s] decodes [Ok None].}}

      Raises [Invalid_argument] if [s] is already bound in [t] or
      if [p] is not {{!wf_paths}well-formed}. *)

  val unbind_service : 'a service -> 'a tree -> 'a tree
  (** [unbind s t] is [t] with the binding of [s] removed (if any). *)

  val unbind_path : Http.Path.t -> 'a tree -> 'a tree
  (** [unbind_path p t] is [t] with all services bound to [p] removed
      (if any). *)

  val service_path : 'a service -> 'a tree -> Http.Path.t option
  (** [service_path s t] is the path to which [s] is bound in [t] (if any). *)

  val path_services : Http.Path.t -> 'a tree -> 'a service list
  (** [path_services p t] are the services bound to [p] in [t] (if any). *)

  val fold_paths :
    (Http.Path.t -> 'a service list -> 'b -> 'b) -> 'a tree -> 'b -> 'b
  (** [fold_paths f t acc] folds [f] and [acc] over all paths of [t] in
      lexicographic order. This includes paths on which the service list
      is empty. *)

  (** {1:fmt URL request formatters} *)

  type fmt
  (** The type for URL request formatters. *)

  (** URL request formatters.

      {b TODO}
      {ul
      {- Move out of [Kurl] as [Fmt_url] or [Urlf] or
         [Urlfmt] or [Url_fmt] ?}} *)
  module Fmt : sig

    type kurl = t
    (** See {!Kurl.type-t}. *)

    type t = fmt
    (** See {!type-fmt}. *)

    val empty :
      ?disable_rel:bool -> ?use_exts:bool -> ?scheme:string ->
      ?authority:string -> root:Http.Path.t -> unit -> fmt
    (** [empty ~disable_rel ~use_exts ~scheme ~host ~root ()] is
        an empty URL formatter with:
        {ul
        {- [scheme], a scheme used for full URL formatting, see {!val-req}.}
        {- [host], a host used for full URL formatting, see {!val-req}.}
        {- [root], the root path prepended to all URL request paths.}
        {- [use_exts], appends the {{!Bare.ext}extension} of bare URLs
           to formatted URLs, defaults to [false] and expands directory
           indexes if requested by kind, see {!val-kind}.}
        {- [disable_rel] turns {{!section-rel}relative} formatting functions
           into absolute ones (defaults to [false]). [true] is faster.
           The idea is to disable it for serving over the [http[s]] scheme
           and enable it for serving over the [file] scheme.}}

        This empty URL formatter has {!any} bound on the root. This means
        that untyped URL formatting is readily available. For example via:
{[
        let url uf path = Kurl.Url.url uf Kurl.(V (any, Bare.v `GET path))
]}
    *)

    val with_fmt :
      ?disable_rel:bool -> ?use_exts:bool -> ?scheme:string ->
      ?authority:string -> ?root:Http.Path.t -> fmt -> fmt
    (** [with_fmt uf] is [uf] with formatter parameters changed
        as specified. See {!Fmt.val-empty}. *)

    val scheme : fmt -> string
    (** [scheme uf] is the scheme of [uf], can be empty. *)

    val authority : fmt -> string
    (** [authority uf] is the authority of [uf], can be empty. *)

    val root : fmt -> Http.Path.t
    (** [root uf] is the root path of [uf]. *)

    val disable_rel : fmt -> bool
    (** [disable_rel uf] is [true] if relative formatting is turned into
        absolute formatting. *)

    val use_exts : fmt -> bool
    (** [use_exts uf] is [true] if bare URL extensions are appended to
        formatted URLs. *)

    (** {1:fmt Typed formatting} *)

    (** {2:bind Binding} *)

    val bind : Http.Path.t -> 'a kind -> fmt -> fmt
    (** [bind p k uf] is [uf] with URL requests of kind [k] bound to
        {{!Kurl.wf_paths}well-formed} path [p].

        Raises [Invalid_argument] if [k] is already bound in [uf] or
        if [p] not {{!Kurl.wf_paths}well-formed}. *)

    val bind_tree : 'a tree -> fmt -> fmt
    (** [bind_tree t uf] is [uf] with URL request kinds in [t] bound to their
        path in [t]. *)

    (** {2:abs Absolute} *)

    val bare : fmt -> kurl -> bare
    (** [bare uf (V (k, u))] is [k]'s bare absolute URL request for [u]. Note
        that this differs from [k]'s {{!enc}encoding} only in the URL path.

        Raises [Invalid_arg] if [k] is not part of [fmt]'s tree. *)

    val req : ?full:bool -> fmt -> kurl -> Http.Method.t * string
    (** [req] is like {!Fmt.val-bare}' but returns an encoded URL path
        and query (if any).

        If [full] is [true] (defaults to [false]) a complete URL is
        returned:
        {ol
        {- [scheme:] is prepended iff {!scheme} is non empty.}
        {- [//] follows iff both {!scheme} and {!authority} are non empty or
           if {!authority} is empty and {!scheme} is ["file"].}
        {- [authority] follows iff {!authority} is non empty.}
        {- The URL path and query follow}}

        In general it's better to avoid full URLs if you can, but it can
        be useful e.g. for sitemap generation. *)

    val url : ?full:bool -> fmt -> kurl -> string
    (** [url] is {!val-req} without the method. *)

    (** {2:rel Relative} *)

    val rel_bare : fmt -> src:kurl -> dst:kurl -> bare
    (** [rel_bare rf ~src ~dst s] is a relative raw request from [src]
        to [dst]. Compared to {!Fmt.val-bare} this only changes the
        path of the result which is the relative path from [src] to [dst].

        Note this assumes both paths do not have dot or non-final empty
        segments.

        Raises [Invalid_arg] if [root] or [sub] is not part of [fmt]'s tree. *)

    val rel_req : fmt -> src:kurl -> dst:kurl -> Http.Method.t * string
    (** [rel_req] is like {!rel_bare} but returns an encoded URL {e path},
        including the query (if any). *)

    val rel_url : fmt -> src:kurl -> dst:kurl -> string
    (** [rel_url] is {!rel_req} without the method. *)
  end

  (** {1:path Path shenanigans}

      HTTP path handling is a mess.

      {2:wf_paths Well-formed paths}

      For this module a {e well-formed path} is either the root path
      [[""]] or a non-empty {!Webs.Http.Request.path} devoid of empty segments
      (including trailing ones).

      To lessen the mess, in [Kurl] you can only bind to well-formed
      paths in service trees and URL formatters. Note that this doesn't
      mean that your kinds can't produce non well-formed paths.

      It's not mandatory but also a good idea to use
      {!Webs.Http.Request.clean_path}
      before handing your requests to {!find_service}.

      {2:root_paths Root paths}

      HTTP clients makes no difference between:
      {[
        https://example.org = https://example.org/
      ]}
      but they do between:
      {[
        https://example.org/a ≠ https://example.org/a/
      ]}
      This leads to all sorts of difficulties for abitrarily rebinding
      services, since the root is treated differently.

      For this reason in tree services we never give an empty path to
      decode. That is if a service is bound on [/a/b] both requests with
      path [/a/b/] and [/a/b] will result in path [/] ("root" of the service)
      in the bare URL to {{!dec}decode}. For this reason it's a good idea to
      canonicalize requests before with {!Webs.Http.Request.clean_path}.

      Todo describe {!Kind.type-root_is}. *)
end

(** {1:websocket Websocket} *)

(** Websocket upgrade logic.

    {b References.}
    {ul
    {- I. Fette et al.
    {{:https://www.rfc-editor.org/rfc/rfc6455}
    {e The WebSocket Protocol}}. 2011}} *)
module Websocket : sig

  open Webs

  (** {1:upgrade Upgrading} *)

  val upgradable : Http.Headers.t -> bool
  (** [upgradable hs] is [true] iff [hs] has a header
      {!Webs.Http.Headers.connection}
      with an ["upgrade"] value and a header {!Webs.Http.Headers.upgrade}
      with a ["websocket"] value. *)

  val upgrade : Http.Request.t -> (Http.Response.t, Http.Response.t) result
  (** [upgrade] responds to upgrade the request to a websocket *)

  (** {1:header_names Headers names} *)

  val sec_websocket_accept : Http.Headers.Name.t
  (** {{:https://www.rfc-editor.org/rfc/rfc6455#section-11.3.3}
      [sec-websocket-accept]} *)

  val sec_websocket_extensions : Http.Headers.Name.t
  (** {{:https://www.rfc-editor.org/rfc/rfc6455#section-11.3.2}
      [sec-websocket-extensions]} *)

  val sec_websocket_key : Http.Headers.Name.t
  (** {{:https://www.rfc-editor.org/rfc/rfc6455#section-11.3.1}
      [sec-websocket-key]} *)

  val sec_websocket_protocol : Http.Headers.Name.t
  (** {{:https://www.rfc-editor.org/rfc/rfc6455#section-11.3.4}
      [sec-websocket-protocol]} *)

  val sec_websocket_version : Http.Headers.Name.t
  (** {{:https://www.rfc-editor.org/rfc/rfc6455#section-11.3.5}
      [sec-websocket-version]} *)

  (** {1:handshake Handshake} *)

  val accept_key : string -> string
  (** [accept_key k] is a value for the [sec-websocket-accept] header
      from the [k] value of the [sec-websocket-key] header. *)
end
