(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Webs service tools. *)

open Webs

(** {1:gateway Gateway interaction} *)

(** Gateway interaction. *)
module Gateway : sig

  (** {1:sending_file File serving handoff}

      See {{!page-web_service_howto.serving_files}this section} of the
      web service howto. *)

  val send_file : header:Http.name -> Req.t -> Http.fpath -> (Resp.t, 'e) result
  (** [send_file ~header r file] lets {e the gateway} respond to [r]
      with file [file], use {!Req_to.absolute_filepath} to determine one
      from [r] safely. More precisely this a {!Webs.Http.ok_200} empty
      response with:

      {ul
      {- Header [header] set to [file]. The actual [header] to use
         depends on your gateway.}}

      {b Note.} The gateway docs of this mechanism are unclear whether we
      should also transfer some of [r]'s headers in the response
      e.g. the etag and conditional headers but at least with Nginx
      that doesn't seem to be the case. *)

  val x_accel_redirect : Http.name
  (** [x_accel_redirect] is the header name
      {{:https://www.nginx.com/resources/wiki/start/topics/examples/x-accel/}
      ["x-accel-redirect"]} used
      by Nginx for internal redirection. *)

  val x_sendfile : Http.name
  (** [x_sendfile] is the header name ["x-sendfile"]. Used by Apache and
      Lighttpd for file serving. *)
end

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
         {{!Webs.Http.moved_permanently_301}301} moved permanently
         to [/name/id[/path]].}
      {- [/name] is either a {{!Webs.Http.not_found_404}404} not found or,
         if the corresponding [id] is easy to find,
         a {{!Webs.Http.moved_permanently_301}301} moved permanently
         to [/name/id].}
      {- [/id[/path]] is a {{!Webs.Http.moved_permanently_301}301}
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
      get_res:('id -> ('res, Webs.Resp.t) result) ->
      res_name:('res -> 'name) ->
      res_url:('name -> 'id -> string) ->
      req_name:'name option ->
      req_id:'id -> unit -> ('res, Webs.Resp.t) result
    (** [resolve ~eq ~get_res ~res_name ~res_url ~req_name ~req_id ()]
        implements the 301 redirection logic spelled out in the preamble of
        this module.

        Given a request's parsed identifier [req_id] and name [req_name]
        (if any):
        {ol
        {- If [get_res req_id] is [Error _] this is returned by the function.}
        {- If [get_res req_id] is [Ok res], let [n] be [res_name res].}
        {- If [req_name] is [None] a {{!Webs.Http.moved_permanently_301}301}
           moved permanently [Error _] to [res_url n req_id] is returned.}
        {- If [req_name] is [Some n'] and [eq n n'] is [false] a
           {{!Webs.Http.moved_permanently_301}301} moved permanently [Error _]
           to [res_url n req_id] is returned.}
         {- Otherwise, [Ok res] is returned.}}

        [eq] defaults to {!Stdlib.( = )}. [res_url] must return
        a value suitable for the {!Webs.Http.location} header. *)

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
        URL encoding. *)
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

    val error_to_resp : error -> Resp.t
    (** [error_to_resp e] is a {{!Webs.Http.bad_request_400}400} bad request
        for [e]. The response's reason is determined by {!error_to_string}. *)

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

    val decode : string -> (t, Resp.t) result
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
    {- Bare.equal}
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
       becomes [URL]able). Bare could embody {!Req.t}.}
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

  val bare : ?ext:string -> ?query:Http.query -> Http.meth -> Http.path -> bare
  (** [bare m p ~query ~ext] is an URL request to path [p] with method
      [m], query [query] (defaults to {!Webs.Http.Query.empty}) and URL
      path file extension [ext] (defaults to [""]). *)

  (** Bare URL requests. *)
  module Bare : sig

    type t = bare
    (** See {!type-bare}. *)

    val v : ?ext:string -> ?query:Http.query -> Http.meth -> Http.path -> bare
    (** See {!val-bare}. *)

    val meth : bare -> Http.meth
    (** [meth u] is the HTTP method of [u]. *)

    val path : bare -> Http.path
    (** [path u] is the URL path of [u]. *)

    val query : bare -> Http.query
    (** [query u] is the URL query of [u]. *)

    val ext : bare -> string
    (** [ext u] is the optional URL path file extension of [u]. This is
        used by some URL formatting modes. *)

    val with_path : Http.path -> bare -> bare
    (** [with_path p b] is [b] with path [p]. *)

    val of_req : ?ext:string -> Req.t -> bare
    (** [of_req ~ext r] is a bare URL request from [r]. {!Bare.meth} is
        {!Req.meth}, {!Bare.path} is {!Req.path}, {!Bare.query} is parsed
        {!Req.query}, [ext] defaults to [""]. *)

    val pp : Format.formatter -> bare -> unit
    (** [pp ppf b] formats an unspecified representation of [b] on
        [ppf]. *)
  end

  (** {2:encoders Encoders} *)

  type 'a enc = 'a -> bare
  (** The type for encoding an URL request value of type ['a] to a bare
      URL request. *)

  (** {2:decoders Decoders } *)

  type 'a dec = bare -> ('a option, Resp.t) result
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

  (** {3:decode_help Decoder helpers} *)

  (** Method constraints. *)
  module Allow : sig

    type 'a t = Http.meth * 'a
    (** The type for method constraints. *)

    val meths : 'a t list -> bare -> ('a, Resp.t) result
    (** [meths ms u] is:
        {ul
        {- [Ok (Bare.meth u)] if [List.mem (Bare.meth u, Bare.meth u) ms]}
        {- [Error _] with a {{!Webs.Http.method_not_allowed_405}405} not
         allowed response otherwise.}} *)

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

      [root_is] defines the root path ([[""]]) encoding behaviour relative
      to the service binding path. With:

      {ul
      {- [`File] the binding path encodes the root. Note that since the
         binding path must be {{!wf_paths}well-formed} it is guaranteed to
         lack a trailing slash. If extension formatting is requested and there
         is one in the encoded bare, it is appended to it.}
      {- [`Dir seg] the binding path with a trailing slash is the resulting
         path. If extension formatting is requested and [seg] is [Some seg]
         then both segment [seg] (e.g. "index") and the extension are appended,
         otherwise the path is left as is.}}

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

      The return type of ['a] is common to all services. Usually this is a
      function which when given arguments common to all services
      effectively handles the request. The service type simply hides the
      first, request kind specific, argument of these functions. *)

  (** {1:trees Service trees} *)

  type 'a tree
  (** The type for service trees binding constant HTTP paths to
      {{!type-service}services} of type ['a]. *)

  val empty : unit -> 'a tree
  (** [empty ()] is an empty service tree. *)

  (** {2:req Request handling} *)

  val find_service : 'a tree -> bare -> ('a option, Resp.t) result
  (** [find_service t u] finds the service for handling [u]'s
      {!Webs.Req.path}. [Ok None] is returned if no URL request kind
      matched. [Error _] is returned if a URL request kind
      matched but errored. *)

  (** {2:bind Binding services} *)

  val bind : Http.path -> 'a service -> 'a tree -> 'a tree
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

  val unbind_path : Http.path -> 'a tree -> 'a tree
  (** [unbind_path p t] is [t] with all services bound to [p] removed
      (if any). *)

  val service_path : 'a service -> 'a tree -> Http.path option
  (** [service_path s t] is the path to which [s] is bound in [t] (if any). *)

  val path_services : Http.path -> 'a tree -> 'a service list
  (** [path_services p t] are the services bound to [p] in [t] (if any). *)

  val fold_paths :
    (Http.path -> 'a service list -> 'b -> 'b) -> 'a tree -> 'b -> 'b
  (** [fold_paths f t acc] folds [f] and [acc] over all paths of [t] in
      lexicographic order. This includes paths on which the service list
      is empty. *)

  (** {1:fmt URL request formatters} *)

  type fmt
  (** The type for URL request formatters. *)

  (** URL request formatters. *)
  module Fmt : sig

    type kurl = t
    (** See {!Kurl.type-t}. *)

    type t = fmt
    (** See {!type-fmt}. *)

    val empty :
      ?disable_rel:bool -> ?use_exts:bool -> ?scheme:string ->
      ?authority:string -> root:Http.path -> unit -> fmt
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
           and enable it for serving over the [file] scheme.}} *)

    val with_fmt :
      ?disable_rel:bool -> ?use_exts:bool -> ?scheme:string ->
      ?authority:string -> ?root:Http.path -> fmt -> fmt
    (** [with_fmt uf] is [uf] with formatter parameters changed
        as specified. See {!Fmt.val-empty}. *)

    val scheme : fmt -> string
    (** [scheme uf] is the scheme of [uf], can be empty. *)

    val authority : fmt -> string
    (** [authority uf] is the authority of [uf], can be empty. *)

    val root : fmt -> Http.path
    (** [root uf] is the root path of [uf]. *)

    val disable_rel : fmt -> bool
    (** [disable_rel uf] is [true] if relative formatting is turned into
        absolute formatting. *)

    val use_exts : fmt -> bool
    (** [use_exts uf] is [true] if bare URL extensions are appended to
        formatted URLs. *)

    (** {1:bind Binding} *)

    val bind : Http.path -> 'a kind -> fmt -> fmt
    (** [bind p k uf] is [uf] with URL requests of kind [k] bound to
        {{!Kurl.wf_paths}well-formed} path [p].

        Raises [Invalid_argument] if [k] is already bound in [uf] or
        if [p] not {{!Kurl.wf_paths}well-formed}. *)

    val bind_tree : 'a tree -> fmt -> fmt
    (** [bind_tree t uf] is [uf] with URL request kinds in [t] bound to their
        path in [t]. *)

    (** {1:fmt Formatting} *)

    (** {2:abs Absolute} *)

    val bare : fmt -> kurl -> bare
    (** [bare uf (V (k, u))] is [k]'s bare absolute URL request for [u]. Note
        that this differs from [k]'s {{!enc}encoding} only in the URL path.

        Raises [Invalid_arg] if [k] is not part of [fmt]'s tree. *)

    val req : ?full:bool -> fmt -> kurl -> Http.meth * string
    (** [req] is like {!Fmt.val-bare}' but returns an encoded URL path
        and query (if any).

        If [full] is [true] (defaults to [false]) a complete URL is
        returned:
        {ol
        {- [scheme:] is prepended iff {!scheme} is non empty.}
        {- [//] follows iff both {!scheme} and {!host} are non empty or
           if {!authority} is empty and {!scheme} is ["file"].}
        {- [authority] follows iff {!authority} is non empty.}
        {- The URL path and query follow}}

        In general it's better to avoid full URLs if you can, but it can
        be useful e.g. for sitemap generation. *)

    val url : ?full:bool -> fmt -> kurl -> string
    (** [url] is {!url_req} without the method. *)

    (** {2:rel Relative} *)

    val rel_bare : fmt -> root:kurl -> kurl -> bare
    (** [rel_bare rf ~root sub s] is [sub]'s relative raw request for [s]
        to the root path of [root]'s request. Compared to {!Fmt.val-bare} this
        only changes the path of the result.

        Note this assumes both paths do not have dot or non-final empty
        segments.

        Raises [Invalid_arg] if [root] or [sub] is not part of [fmt]'s tree. *)

    val rel_req : fmt -> root:kurl -> kurl -> Http.meth * string
    (** [rel_req] is like {!rel_bare} but returns an encoded URL {e path},
        including the query (if any). *)

    val rel_url : fmt -> root:kurl -> kurl -> string
    (** [rel_url] is {!rel_req} without the method. *)
  end

  (** {1:path Path shenanigans}

      HTTP path handling is a mess.

      {2:wf_paths Well-formed paths}

      For this module a {e well-formed path} is either the root path
      [[""]] or a non-empty {!Http.path} devoid of empty segments
      (including trailing ones).

      To lessen the mess, in [Kurl] you can only bind to well-formed
      paths in service trees and URL formatters. Note that this doesn't
      mean that your kinds can't produce non well-formed paths.

      It's not mandatory but also a good idea to use {!Webs.Req.clean_path}
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
      canonicalize requests before with {!Webs.Req.clean_path}.

      Todo describe {!Kind.type-root_is}. *)
end

(** {1:auth Being authentic} *)

(** SHA-256 hashes, HMAC-SHA-256 and PBKDF2-HMAC-SHA-256. *)
module Sha_256 : sig

  (** {1:values Hash values} *)

  type t
  (** The type for SHA-256 hashes. *)

  val length : t -> int
  (** [length h] is the length of [h] in bytes (i.e. 32). *)

  val hash : string -> t
  (** [hash s] is the SHA-256 hash of [s]. *)

  (** {1:hmac HMAC-SHA-256} *)

  val hmac : key:string -> string -> t
  (** [hmac ~key msg] is the {{:https://tools.ietf.org/html/rfc2104}RFC 2104}
      HMAC-SHA-256 for key [key] and message [msg].
      [key] should not be less than 32 bytes. *)

  (** {1:pbkdf2 PBKDF2-HMAC-SHA-256} *)

  val pbkdf2_hmac :
    key_len:int -> iterations:int -> pass:string -> salt:string -> unit ->
    string
  (** [pbkdf2_hmac ~key_len ~iterations ~pass ~salt ()] derives a key
      for password [pass] with a salt [salt] and
      iterations [iterations] iterations (use at least [100_000]) wto
      generate a key of length [key_len] using
      {{:https://tools.ietf.org/html/rfc8018}RFC 8018}'s
      PBKFD2-HMAC-SHA-256.

      {b Warning.} Use {!equal_key} to compare derived keys,
      not {!String.equal} or [( = )].

      In 2021, here is a good baseline of parameters:

      {ul
      {- A [key_len] of [32] bytes.}
      {- A number of [iterations] of [400_000].}
      {- A [salt] length of [8] bytes.}}

      {b Warning.} Use {!equal_key} to compare derived keys,
      not {!String.equal} or [( = )].

      Raises [Invalid_argument] if [key_len] or [iterations] are smaller or
      equal to [0] or if [key_len] is greater than 2{^32} - 1 * 32
      or [max_int]. *)

  val equal_key : string -> string -> bool
  (** [equal_key k0 k1] is a constant time string equality for [k0]
      and [k1] of the same length. Do not use to compare string of
      different lengths this raises [Invalid_argument] in that
      case. *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal h0 h1] is a constant time equality comparison function
      between [h0] and [h1]. *)

  (** {1:convert Converting} *)

  val to_bytes : t -> string
  (** [to_bytes h] is the sequence of bytes of [h]. *)

  val of_bytes : string -> (t, unit) result
  (** [of_bytes s] is the sequence of bytes of [s] as a hash value.
      An error is returned if the length of [s] in not 32. *)

  val to_hex : t -> string
  (** [to_ascii_hex h] is the sequence of bytes of [h] as US-ASCII lowercase
      hexadecimal digits. *)

  val of_hex : string -> (t, int) result
  (** [of_hex s] parses a sequence of US-ASCII (lower or upper cased)
      hexadecimal digits to its hash value. Errors with an offending
      index or the length of the string in case [s] was
      not exactly made of 64 US-ASCII hex digits. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats digests with {!to_hex}. *)
end

(** Authenticatable data.

    This module defines a simple US-ASCII compatible
    {{!Authenticatable.auth}encoding scheme} to publish {b
    non-encrypted}, expirable data bytes authenticatable with a
    private key. Human readability and secrecy is a non-goal, storing
    state in non-trusted environments is.

    {b The data is not encrypted}.

    {b Note.} The scheme and module is designed with extensibility
    in mind. But for now only an {{!Sha_256.val-hmac}HMAC-SHA-256} based
    scheme is defined. *)
module Authenticatable : sig

  (** {1:time Time} *)

  type time = int
  (** The type for some notion of time to expire data. The semantics is left
      to the client, for example you can use a logical notion of time or the
      number of seconds since the epoch. *)

  (** {1:private_keys Private keys} *)

  type private_key = [
    | `Hs256 of string (** Used with
      {{!Sha_256.val-hmac}[HMAC-SHA-256]}, hence should be at least 32
      bytes. *) ]
  (** The type for private keys.  *)

  val random_private_key_hs256 : unit -> private_key
  (** [random_private_key_hs256 ()] are 64 random bytes sourced from
      a {!Stdlib.Random.make_self_init} initialized PRNG. *)

  val private_key_to_ascii_string : private_key -> string
  (** [private_key_to_ascii_string k] encodes [k] to an URL safe US-ASCII
      scheme that can be read back by {!private_key_of_string}. *)

  val private_key_of_ascii_string : string -> (private_key, string) result
  (** [private_key_of_ascii_string s] reads back the encoding of
      {!private_key_to_string}. *)

  (** {1:auth Authenticatable} *)

  type t = string
  (** The type for authenticatable bytes. The encoding scheme for bytes
      [data] and an optional expiration timestamp [expire] and private key
      [private_key] is defined by:
{[
exp = match expire with None -> "" | Some e -> string_of_int e
msg = exp ^ ":" ^ data
hs256 = "HS256:" ^ (hmac_sha_256 private_key msg)
auth = base64url (hs256 ^ msg)
]}
  *)

  (** {1:enc Encode} *)

  val encode : private_key:private_key -> expire:time option -> string -> t
  (** [encode ~private_key ~expire data] makes data [data]
      expire at [expire] (if any) and authenticatable via the private
      key and scheme defined by [private_key]. *)

  (** {1:dec Decode} *)

  type format_error =
  [ `Base64url of Http.Base64.error (** [base64url] decode error. *)
  | `Scheme of string option (** Scheme name, if one was found. *) ]
  (** The type for decode format errors. *)

  val format_error_message : format_error -> string
  (** [format_error_message e] is an english error message for [e]. *)

  type error =
  [ `Authentication (** Authentication error. *)
  | `Expired of time (** Expiration time. *)
  | `Missing_now_for of time (** Expiration time. *)
  | `Format of format_error ]
  (** The type for decode and authentication errors. See {!decode}. *)

  val error_message : error -> string
  (** [error_message e] is an english error message for [e]. *)

  val error_string : ('a, error) result -> ('a, string) result
  (** [error_string r] is [Result.map_error error_message r]. *)

  val decode :
    private_key:private_key -> now:time option -> t ->
    (time option * string, error) result
  (** [decode ~private_key ~now s] authenticates data [s] with
      the private key and scheme defined by [private_key] and expires it
      (if applicable) according to [now]. If [now] is [None] and [s] has
      an expiration timestamp, the result errors. More precisely the
      the result is:
      {ul
      {- [Ok (expire, data)] with [data] the authenticated bytes and [expire]
         the expiration timestamp iff [s] is
         authenticated by [private_key] and,
         if [expire] is [Some t], [now] is [Some n] with [n < t].}
      {- [Error `Authentication] if [s] cannot be authenticated by
         [private_key].}
      {- [Error (`Expired t)], if [s] is authenticated by [private_key]
         and expires at [t] but [now] is [Some n] with [n >= t].}
      {- [Error (`Missing_now_for t)], if [s] is authenticated by
         [private_key] and expires at [t] but [now] is [None].}
      {- [Error (`Format _)] if any other decoding error occurs.}} *)

  (** {1:untrusted Untrusted decode} *)

  type untrusted = [`Untrusted_hs256 of Sha_256.t * time option * string ]
  (** The type for untrusted decode result. *)

  val untrusted_decode : t -> (untrusted, format_error) result
  (** [untrusted_decode s] decodes the encoding structure of [s] but
      neither authenticates nor expires the data. *)
end

(** Authenticated cookies.

    An authenticated cookie uses the {!Authenticatable} scheme to let
    your service store expirable state on the client with the
    guarantee that it cannot tamper with it.

    {b The data is not encrypted.} *)
module Authenticated_cookie : sig

  (** {1:setclear Setting and clearing} *)

  val set :
    private_key:Authenticatable.private_key ->
    expire:Authenticatable.time option -> ?atts:Http.Cookie.atts ->
    name:string -> string -> Resp.t -> Resp.t
  (** [set ~private_key ~expire ~atts ~name data resp] sets in [resp] the
      cookie [name] to [data] authenticated by [private_key] and expiring at
      [expire] (see {!Authenticatable.encode}). [atts] are the cookie's
      attributes, they default to {!Webs.Http.Cookie.atts_default}.

      {b Note.} The expiration [expire], if provided, expires the
      authenticated data, it does not affect HTTP cookie expiration. Use the
      [max_age] attribute of {!Webs.Http.Cookie.val-atts} for that.  *)

  val clear : ?atts:Http.Cookie.atts -> name:string -> Resp.t -> Resp.t
  (** [clear ~atts ~name resp] clears the cookie named [name] in
      [resp] by setting its [max-age] to [-1] and value to
      [""]. [atts] should be the same value as the one given to
      [set], its [max_age] attribute gets overriden with a [-1] by
      the function. *)

  (** {1:get Getting} *)

  type error =
  [ Authenticatable.error (** Authenticatable data errors. *)
  | `Cookie of string (** Cookie decoding errors. *) ]
  (** The type for authenticated cookie decode and authentication
      errors. *)

  val error_message : error -> string
  (** [error_message e] is an english error message for [e]. *)

  val error_string : ('a, error) result -> ('a, string) result
  (** [error_string r] is [Result.map_error error_message r]. *)

  val find :
    private_key:Authenticatable.private_key ->
    now:Authenticatable.time option -> name:string -> Req.t ->
    ((Authenticatable.time option * string) option, error) result
  (** [find ~private_key ~now ~name req] is the cookie of [req] named
      [name] authenticated and expired by [private_key] and
      [now]. This is [Ok None] if no cookie named [name] could be
      found or if its value is [""]. If the cookie was {!set} with
      an [expire] you need to provide a [now] otherwise it will never
      authenticate, see {!Authenticatable.decode} for more details. *)
end

(** Session handling.

    Sessions maintain state across request-response cycles. This
    module provides a basic mechanism to abstract session handlers.

    A handler {{!Session.section-client_stored}implementation} using
    {!Authenticated_cookie}s for storing session state on the clients is
    also provided.  *)
module Session : sig

  (** {1:state_descr State descriptors} *)

  type 'a state
  (** The type for describing session state of type ['a]. Values of this type
      describe to {{!type-handler}session handlers} how to assert values of
      type ['a] for equality and how to codec them with bytes. *)

  (** State descriptors.

      {b FIXME.} encode allow errors ? [pp] ? *)
  module State : sig

    type 'a t = 'a state
    (** See {!Session.type-state}. *)

    val v :
      eq:('a -> 'a -> bool) -> encode:('a -> string) ->
      decode:(string -> ('a, string) result) -> unit -> 'a state
  (** [v ~eq ~encode ~decode ()] tests state for equality with
      [eq], encodes it to bytes with [encode] and decodes from bytes
      with [decode]. *)

    val eq : 'a state -> ('a -> 'a -> bool)
    (** [eq sd] is the equality function of [sd]. *)

    val encode : 'a state -> ('a -> string)
    (** [encode sd] is the encoding function of [sd]. *)

    val decode : 'a state -> (string -> ('a, string) result)
    (** [decode sd] is the decoding function of [sd]. *)

    val option_eq : 'a state -> 'a option -> 'a option -> bool
    (** [option_eq sd] is [eq] trivially extended to optional state. *)
  end

  (** {1:handler Session handler} *)

  type ('a, 'e) handler
  (** The type for session handler with state of type ['a] and state
      loading errors of type ['e]. Values of this type are in charge of
      loading the state when a request is received and saving it when
      a response is sent back and the state has changed. *)

  (** Session handlers. *)
  module Handler : sig

    type ('a, 'e) t = ('a, 'e) handler
    (** See {!Session.type-handler}. *)

    val v :
      load:('a state -> Req.t -> ('a option, 'e) result) ->
      save:('a state -> 'a option -> Resp.t -> Resp.t) -> unit ->
      ('a, 'e) handler
    (** [handler ~load ~save ()] is a session handler using [load]
        to setup the session state and [save] to save it before responding.

        Sessions as are represented by ['a option] values. On [load] a [None]
        indicates there is no session. On [save] a [None] indicates to
        drop the session.

        {b Warning.} [save] gets invoked [iff] the state to save is different
        from the one that was loaded. *)

    val load :
      ('a, 'e) handler -> ('a state -> Req.t -> ('a option, 'e) result)
    (** [load h] is the state loading function of [h]. *)

    val save :
      ('a, 'e) handler -> ('a state -> 'a option -> Resp.t -> Resp.t)
    (** [save h] is the state saving function of [h]. *)
  end

  type 'a resp = 'a option * Resp.t
  (** The type for session responses. *)

  val setup :
    'a state -> ('a, 'e) handler ->
    (('a option, 'e) result -> Req.t -> 'a resp) -> Webs.service
  (** [setup sd h service] handles loading and saving state described
      by [sd] with handler [h] for service [service].

      The resulting service behaves as follows. On a request [r],
      [service] is invoked with the state loaded by [h] for [r]. The
      new state returned by [service] is saved by [h] if different from
      the loaded state. *)

  (** {1:client_stored Client stored sessions}

      This handler provides sessions by storing them on the client.
      The session data is {b unencrypted} but authenticated, the
      client cannot tamper with it – unless it finds out about your
      private key.

      This is convenient for small user state like login status as no
      state needs to be maintained on the server. However be mindful
      about the size of your state as it travels in each request and,
      on state changes, in responses. *)

  type client_stored_error =
  [ Authenticated_cookie.error (** Authenticated cookie errors. *)
  | `State_decode of string (** Session state decode errors. *) ]
  (** The type for client stored load state errors. *)

  val client_stored_error_message : client_stored_error -> string
  (** [client_stored_error_message e] is an english error
      message for [e]. *)

  val client_stored_error_string :
    ('a, client_stored_error) Stdlib.result -> ('a, string) Stdlib.result
  (** [client_stored_error_string r] is
      [Result.map_error client_stored_error_message r]. *)

  val client_stored :
    private_key:Authenticatable.private_key -> ?atts:Http.Cookie.atts ->
    name:string -> unit -> ('a, client_stored_error) handler
  (** [client_stored ~private_key ~atts ~name] stores
      {{:expiring}non-expirable} state on the client in an
      {!Authenticated_cookie} authenticated with the private key
      [private_key], named [name] and with attributes [atts]
      [atts] (defaults to {!Webs.Http.Cookie.atts_default}).

      {b Important.} The default [atts] has no path defined. This sets
      the cookie (and thus the session) only for the requested URI prefix
      which is unlikely to be what you want. FIXME cross-check that.

      {b Warning.} The state is not encrypted and stored on the client's
      matchine, make sure no service secrets are part of the state.

      {b Warning.} Cookies are set in the response iff the state changes
      in a request-response cycle. In particular this means that using
      a [max_age] in the cookie attributes, will only refresh the deadline
      whenever the state changes; but {{!expiring}we argue} this should not
      be used anyways. *)

  (** {1:result [result] state injection}

      Convenience result combinators. *)

  val for_result :
    's option -> ('a, 'b) result -> ('s option * 'a, 's option * 'b) result
  (** [for_result st r] injects [st] in either case of [r]. *)

  val for_ok : 's option -> ('a, 'b) result -> ('s option * 'a, 'b) result
  (** [for_ok st r] injects [st] into the error case of [r]. *)

  val for_error : 's option -> ('a, 'b) result -> ('a, 's option * 'b) result
  (** [for_error st r] injects [st] into the error case of [r]. *)

  type nonrec 'a result = ('a resp, 'a resp) result


  (** {1:design_notes Design notes}

      {b TODO.} Decide whether they makes sense.

      {2:expiring Expiring sessions}

      {b FIXME.} Revise this now that we have state load errors
      in the service, it brings new ways around this.

      The session mechanism has no provision to automatically expire
      sessions. In general expiring sessions is not very user
      friendly, especially on mobile. It seems better to let the user
      have its state indefinitely if it wishes and have a [sudo]-like
      mechanism that asks and provides elevated authentication for a
      limited amount of time to peform more sensitive operations.

      This expiration mechanism can be devised on top of the mechanism
      of this module using session state itself – for example by
      having a [sudo_until : Ptime.t option] field in the state of an
      authenticated cookie and dedicated logic to handle it.

      Somehow it feels like a good separation of concerns, or not.

      {2:state State data structure}

      The {!type-state} type could be a heterogenous dictionary with
      serializable values. This makes state highly composable, but {e
      implicitely} composable and thus harder to understand and audit.

      For now we would rather try to explore how inconvenient explicit
      state design and composition is or becomes in practice. *)
end

(** HTTP basic authentication

    {b WARNING.} Only for quick hacks {b over HTTPS}. Nothing serious
    should be protected by that, the user name and password travel in
    plain text on each request. Without prevention it is easily
    amenable to cross-site request forgery attacks. Finally it is not
    possible for users to log out.

    {b References.}
    {ul
    {- R. Fielding et al.
    {{:https://tools.ietf.org/html/rfc7235}
    {e Hypertext Transfer Protocol (HTTP/1.1): Authentication}}. 2014}
    {- J. Reschke.
    {{:https://tools.ietf.org/html/rfc7617}
    {e The 'Basic' HTTP Authentication Scheme}}. 2015}} *)
module Basic_auth : sig

  type user = string
  (** The type for users. Note that since all this is utterly broken
      the user should not contain [':'] (U+003A) characters. *)

  type check =
    user:user -> pass:string ->
    (unit, [`User_unknown | `Wrong_password]) result
  (** The type for basic authentication password check. Really, don't use
      that. If this sources from storage at least hash your passwords. *)

  val enticate :
    ?cancel:(Req.t -> Resp.t) -> (* TODO we want to specify a body. *)
    check:check -> realm:string -> Req.t -> (user * Req.t, Resp.t) result
  (** [enticate ~check ~realm ~forbidden_body ~cancel req] is:
      {ul
      {- [Ok (user, req)] if the basic {{!Webs.Http.authorization}
         authorization header} in [req] passes [check].}
      {- A {{!Webs.Http.unauthorized_401}401} response
         [Error (cancel req)] with a challenge for [realm] if there
         is no authorization header or if [check] failed. The page is only
         shown if the user cancels, defaults to an english HTML page
         that entices the  user to try again via a link to self.}
      {- A {{!Webs.Http.bad_request_400}400} bad request [Error resp]
         if the basic authentication failed to parse.}} *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers

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
