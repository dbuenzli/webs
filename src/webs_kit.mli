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

      {b Note.} The gateway docs of this mecanism are unclear whether we
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
        a value suitable for the {!Webs.Http.H.location} header. *)

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

    val error_to_string : error -> string
    (** [error_to_string e] is an english error message for [e]. *)

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
    {- Integrate fragment part ? If yes, in the [bare] type or on
       formatting functions ? Likely not on [bare], you never get fragments
       to decode so it seems a bit OT.}
    {- Terminology: service or url request tree ?}
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

    type 'a t
    (** See {!type-kind}. *)

    val v : ?root_is:root_is -> ?name:string -> 'a enc -> 'a dec -> 'a kind
    (** See {!val-kind}. *)

    val name : 'a t -> string
    (** [kind k] is the name of [k]. *)

    val root_is : 'a t -> root_is
    (** [root_is k] is root path encoding behaviour for [k], see {!val-kind}. *)

    val enc : 'a t -> 'a enc
    (** [enc k] is the encoder of [k]. *)

    val dec : 'a t -> 'a dec
    (** [dec k] is the decoder of [k]. *)

    val equal : 'a t -> 'a t -> bool
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

      In 2021, here is a good baseline of parameters:
      {ul
      {- A [key_len] of [32] bytes.}
      {- A number of [iterations] of [400_000].}
      {- A [salt] length of [8] bytes.}}

      Raises [Invalid_argument] if [key_len] or [iterations] are smaller or
      equal to [0] or if [key_len] is greater than 2{^32} - 1 * 32
      or [max_int]. *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal h0 h1] is true iff [h0] and [h1] are equal. *)

  val compare : t -> t -> int
  (** [compare h0 h1] is a total order on hashes compatible with
      {!equal}. *)

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
    {{!Authenticatable.t}encoding
    scheme} to publish {b non-secret}, expirable data bytes
    authenticatable via a secret private key. Human readability is a
    non-goal, storing your state in non-trusted environments is.

    {b The data is not encrypted}.

    {b TODOs.}
    {ul
    {- {!decode}, are we happy about the erroring structure ?}
    {- {!encode}/{!decode}, provide primed version which compose
       with [{to,of}_string] funs ?}} *)
module Authenticatable : sig

  (** {1:time Time} *)

  type time = int
  (** The type for some notion of time to expire data. For example you
      can use a logical notion of time or the number of seconds since
      the epoch. *)

  (** {1:keys Keys} *)

  type private_key = string
  (** The type for private keys. This is used with
      {{!val:Sha_256.hmac}[HMAC-SHA-256]}, so it should be at least 32
      bytes long. *)

  val random_private_key : unit -> private_key
  (** [random_private_key ()] are 64 bytes random bytes sourced after having
      called {!Stdlib.Random.self_init}. *)

  (** {1:auth Authenticatable} *)

  type t = string
  (** The type for authenticatable bytes. The encoding scheme for bytes
      [data] and an optional expiration timestamp [expire] and private key
      [private_key] is:
{[
expire = match expire with None -> "" | Some e -> string_of_int e
msg = expire + ":" + data
authenticatable = (base64|base64url)(hmac-sha-256(private_key, msg) + msg)
]}
  *)

  val encode :
    ?base64url:bool -> private_key:private_key -> expire:time option ->
    string -> t
  (** [encode ~private_key ~expire data] makes data [data] expire at [expire]
      (if any) and authenticatable via the private key [private_key]. If
      [base64url] is [true] (defaults to [false]) the [base64url]
      encoding scheme is used instead of [base64] (see {!Webs.Http.Base64}). *)

  val decode :
    ?base64url:bool -> private_key:private_key -> now:time -> t ->
    (time option * string, [`Expired | `Decode | `Authentication]) result
  (** [decode ~key ~now s] authenticates data [s] with the private
      key [key] and makes it expire (if applicable) according to [now].
      The result is:
      {ul
      {- [Ok (expire, data)] with [data] authenticated by [private_key]
         and [now] stricly smaller than [expire] (if any).}
      {- [Error `Expired] if the data could be authenticated but
         [now] is larger or equal to [expire] (if any).}
      {- [Error `Authentication] if [s] cannot be authenticated by
         [key].}
      {- [Error `Decode] if any other decoding error occurs.}}

      If [base64url] is [true] (defaults to [false]) the [base64url] encoding
      scheme is used instead of [base64]. *)

  (** {1:untrusted Untrusted decode} *)

  val untrusted_decode :
    ?base64url:bool -> t ->
    ([`Untrusted of Sha_256.t * time option * string], [`Decode]) result
    (** [untrusted_decode s] decodes the encoding structure of [s] but
        neither authenticates nor expires the data. *)
end

(** Authenticated cookies.

    An authenticated cookie lets your service store expirable state on
    the client with the guarantee that it cannot tamper with it. The
    data is {b not encrypted}, this is not made to store service
    secrets on the client.

    In order to use this you need a private key in your service. An
    easy way to handle this is to generate one randomly with
    {!Authenticatable.random_key} when you start your service. Note
    however that this invalidates any data currently stored on your
    clients whenever you restart your service – that may be okay, or
    not. *)
module Authenticated_cookie : sig

  val get :
    private_key:Authenticatable.private_key -> now:Authenticatable.time ->
    name:string -> Req.t -> string option
  (** [get ~private_key ~now ~name req] is the cookie of [req] named [name]
      authenticated and expired by [private_key] and [now] (see
      {!Authenticatable.decode}).

      {b TODO.} Any kind of error leads to [None]. *)

  val set :
    private_key:Authenticatable.private_key ->
    expire:Authenticatable.time option -> ?atts:Http.Cookie.atts ->
    name:string -> string -> Resp.t -> Resp.t
  (** [set ~private_key ~expire ~atts ~name data resp] sets in [resp] the
      cookie [name] to [data] authenticated
      by [private_key] and expiring at [expire] (see
      {!Authenticatable.encode}). [atts] are the cookie's attribute
      they default to {!Webs.Http.Cookie.atts_default}. *)
end

(** Sessions.

    Sessions maintain state across request/response cycles. This module
    provides a basic infrastructure to abstract the mecanism handling
    sessions.

    One built-in mecanism is offered for {b unencrypted} but authenticated
    client-side sessions via {!Authenticated_cookie}s.

    {b TODO.}
    {ul
    {- Error paths. In particular on load.}
    {- Expiry.}
    {- Provide a reasonably efficient and convenient (Tpf ?) binary
       solution for {!state} codec.}
    {- Authenticated cookie, fix the None path. Use expiration, option
       to refresh on each request. Understand caching issues. Global salty
       invalidation ?}
    {- Add session handler init hooks and expiry cleanup}
    {- Should {!state} be as a heterogenous dict with
       serialisation ? That makes state composable, but implicitely
       composable. We should rather go for
       explicit compositionality and state design.}} *)
module Session : sig

  (** {1:session Session state} *)

  type 'a state
  (** The type for session state of type ['a]. Values of this type
      describe how to test ['a] for equality and codec it with
      bytes. *)

  val state :
    eq:('a -> 'a -> bool) -> encode:('a -> string) ->
    decode:(string -> ('a, string) result) -> unit -> 'a state
  (** [state ~eq ~encode ~decode ()] tests state for equality with
      [eq], encodes it with [encode] and decodes it with [decode]. *)

  (** Built-in state values. {b TODO.} Call Tpf to the rescue. *)
  module State : sig
    val int : int state
    val string : string state
    val pair : 'a state -> 'b state -> ('a * 'b) state
  end

  (** {1:handler Session handler} *)

  type 'a handler
  (** The type for session handler of state of type ['a]. Values
      of this type are in charge of loading and saving the state. *)

  val handler :
    load:('a state -> Req.t -> 'a option) ->
    save:('a state -> 'a option -> Resp.t -> Resp.t) -> unit ->
    'a handler
  (** [handler ~load ~save ()] is a session handler using [load]
      to setup the session state and [save] to save before responding.

      {b TODO} do we want to give the original [Req.t] to save
      aswell ? *)

  type 'a resp = 'a option * Resp.t
  (** The type for session responses. *)

  val setup :
    'a state -> 'a handler -> ('a option -> Req.t -> 'a resp) -> Webs.service
  (** [setup st handler service] handles loading and saving state [st]
      with handler [handler] for service [service] which gets current
      session state as argument and should tuple the new state with
      the resulting request. *)

  (** {1:result Injecting session state in [result]} *)

  val for_result :
    's option -> ('a, 'b) result -> ('s option * 'a, 's option * 'b) result
  (** [for_result st r] injects [st] in either case of [r]. *)

  val for_ok : 's option -> ('a, 'b) result -> ('s option * 'a, 'b) result
  (** [for_ok st r] injects [st] into the error case of [r]. *)

  val for_error : 's option -> ('a, 'b) result -> ('a, 's option * 'b) result
  (** [for_error st r] injects [st] into the error case of [r]. *)

  type nonrec 'a result = ('a resp, 'a resp) result

  (** {1:built-in Built-in session handlers} *)

  val with_authenticated_cookie :
    private_key:Authenticatable.private_key -> ?atts:Http.Cookie.atts ->
    name:string -> unit -> 'a handler
  (** [with_authenticated_cookie ~private_key ~atts ~name] stores
      state on the client with an {!Authenticated_cookie} that can be
      authenticated with the private key [private_key].  [name] is the
      name of the cookie.  [atts] are the attributes of the cookie,
      they default to {!Webs.Http.Cookie.atts_default}. *)
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
        {- [Ok (user, req)] if the basic {{!Webs.Http.H.authorization}
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
