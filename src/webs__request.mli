(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** HTTP requests.

    These values are:
    {ul
    {- Constructed by client users and interpreted by client connectors
       according to
       {{!page-connector_conventions.client_requests}these conventions}.}
    {- Constructed by service connectors according to these
       {{!page-connector_conventions.service_requests}these conventions}
       and handed over to service implementations for intepretation.}}

    @canonical Webs.Http.Request *)

(** {1:requests Requests} *)

type t
(** The type for HTTP requests. *)

val make :
  ?headers:Webs__headers.t -> ?log:string -> ?path:Webs__path.t ->
  ?query:string option -> ?scheme:Webs__scheme.t ->
  ?service_path:Webs__path.t ->
  ?version:Webs__version.t -> Webs__method.t -> raw_path:string ->
  Webs__body.t -> t
(** [make method' ~raw_path body] is a request with given [method'],
    [raw_path] and [body] and:

    {ul
    {- [headers], the request headers. Defaults to {!Http.Headers.empty}.
       In general it is better to let the {{!Http.Body.make}body} define
       content type and content length headers, see the
       {{!page-connector_conventions.client_requests}client requests} and
       {{!page-connector_conventions.service_requests}service requests}
       conventions.}
    {- [log], the connector side log message, see {!log}. Defaults to [""].}
    {- [path], see {!path}. Defaults to {!Path.none}.}
    {- [query], see {!query}. Defaults to [None].}
    {- [scheme] is the scheme of the request, see {!scheme}. Defaults to
       [`Https].}
    {- [service_path], see {!service_path}. Defaults to {!Path.root}.}
    {- [version], the HTTP version, see {!version}. Defaults to
       {!Http.Version.none}. Client connectors generally ignore this, they
       decide how they want to send the request to the server.}}

    Usually you should rather use {!for_service_connector}
    or {!of_url}. This ensures that derived data like {!path} and {!query}
    are computed correctly. *)

val for_service_connector :
  ?log:string -> ?scheme:Webs__scheme.t -> service_path:Webs__path.t ->
  version:Webs__version.t -> Webs__method.t -> raw_path:string ->
  headers:Webs__headers.t -> Webs__body.t -> (t, Webs__response.t) result
(** [for_service_connector ~service_path ~version method' ~raw_path
    ~headers body] is a request that satisfies the
    {{!page-connector_conventions.service_requests} service requests
    conventions}.

    In case the response cannot satisfy them an error
    response is returned according to the
    {{!page-connector_conventions.service_connector_responses}
    connector responses conventions} and [body] is {!Http.Body.dismiss}ed. *)

val of_url :
  ?body:Webs__body.t -> ?headers:Webs__headers.t -> ?log:string ->
  Webs__method.t -> url:Webs__url.t -> (t, string) result
(** [of_url method' ~url] is a [method'] request on [url] ensuring that
    the request satsifies the
    {{!page-connector_conventions.client_requests}client request
    conventions}. If [url] is {!Url.is_likely_percent_decoded},
    it is percent-encoded before constructing the request.

    [headers] defaults to {!Headers.empty} and a suitable
    {!Headers.host} derived from [url] is added to it. [body] defaults to
    {!Body.empty}. Both {!query} and {!path} properties are
    derived from [url] and the
    {!service_path} is {!Path.root}. The {!scheme} property is determined
    from [url].

    An error is returned if the scheme is neither [http] or
    [https] or if a decoding error occurs. In this case [body]
    is {!Http.Body.dismiss}ed. *)

val to_url : t -> (Webs__url.t, string) result
(** [to_url request] is an URL for [request]. This can be seen as the
    inverse of {!of_url}. This only errors if no {!Headers.host}
    header can be found in the headers of [request]. The resulting
    URL should be percent encoded (but that depends if [request]
    was well formed). *)

val with_body : Webs__body.t -> t -> t
(** [with_body b request] is [request] with body [b]. The body of [request]
    is {!Http.Body.dismiss}ed. *)

val with_headers : Webs__headers.t -> t -> t
(** [with_headers headers request] is [request] with headers [headers]. *)

val override_headers : by:Webs__headers.t -> t -> t
(** [override_headers ~by request] is [request] with headers
    [Http.Headers.override (headers response) ~by]. *)

(** {1:properties Properties} *)

val body : t -> Webs__body.t
(** [body request] is the body of [request]. *)

val headers : t -> Webs__headers.t
(** [headers request] are the HTTP headers of [request]. Should always
    at least includes at the {!Http.Headers.host} header. *)

val log : t -> string
(** [log request] is the log of [request]. The log is a
    client-side explanatation {b not meant to be sent to the
    server}. It can be used to log further details or explanations
    about the request. *)

val method' : t -> Webs__method.t
(** [method' request] is the
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-methods}HTTP method}
    of [request]. *)

val path : t -> Webs__path.t
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

val scheme : t -> Webs__scheme.t
(** [scheme request] is the scheme of the request. {b Note that
    this is indicative}. In particular services should not rely on
    this as this may depend on the way you are proxied or not be
    set by the connector. It it is mainly used by client
    connectors to be able to reconstruct the requested URL for
    example to follow redirections. *)

val service_path : t -> Webs__path.t
(** [service_path request] is service path of [request].
    {ul
    {- For client connectors this is irrelevant and set to
       {!Path.root}.}
    {- For service connectors this is the path on which the root
       of the service is attached. This is usually defined by the service
       connector. The {!val-path} value of [r] is the path mentioned in
       {!raw_path} stripped by this path.}} *)

val version : t -> Webs__version.t
(** [version request] is the HTTP version of [request].
    {ul
    {- For client connectors this is mostly irrelevant: the connectors
       decides how they want to talk to the client. But it can
       be used as hint for which HTTP version to use.}
    {- For service connectors this should be the HTTP version of the
       request made on the connector. Note that if the service connector
       interfaces with a gateway this may be different from the actual
       version used by the gateway with the client.}} *)

(** {1:deconstruct Deconstructing and responding}

    Request deconstruction and response helpers. When they error
    these functions directly do with responses that have the
    right statuses and, unless otherwise noted, empty bodies. They
    also generally dismiss the body of the erroring request, see
    the individual combinators for details. *)

(** {2:redirect Redirection} *)

val redirect_to_path :
  ?body:Webs__body.t -> ?headers:Webs__headers.t -> ?log:string ->
  ?reason:string -> t -> Webs__status.t -> Webs__path.t ->
  Webs__response.t
(** [redirect_to_path request status path] redirects to [path] in
    the service of [request]. This is {!Response.redirect}[ status
    loc] with [loc] the result of {{!Path.encode}encoding} the
    path:
    {[Path.concat (service_path request) path]}
    and the body of [request] is {!Http.Body.dismiss}ed. *)

(** {2:header_decoding Header decoding} *)

val decode_header :
  Webs__headers.Name.t -> (string -> ('a, string) result) -> t ->
  ('a option, Webs__response.t) result
(** [decode_header h dec rrequest] decodes header [h] (if any) in [request].
    Errors with {!Http.Status.bad_request_400} in case of decoding
    errors, transfers the error message to {!Response.reason} and
    the body of [request] is {!Http.Body.dismiss}ed. *)

(** {2:method_constraints Method constraints} *)

val allow :
  'a Webs__method.constraint' list -> t -> ('a, Webs__response.t) result
(** [allow ms request] is:
    {ul
    {- [Ok (Request.method' request)] is in the allowed methods [ms].}
    {- [Error _] with a {!Http.Status.method_not_allowed_405}
       response otherwise and the body of [request] is
       {!Http.Body.dismiss}ed.}} *)

(** {2:cookies Cookies} *)

val find_cookie : name:string -> t -> (string option, string) result
(** [find_cookie ~name request] is the value of cookie [name] or [None] if
    undefined in [request]. Errors on header or cookie decoding
    errors.

    {b FIXME.} Why is this a string error ? *)

(** {2:queries Queries} *)

val to_query : t -> (Webs__query.t, Webs__response.t) result
(** [to_query request] extracts a query from [request]. This is:

    {ul
    {- [Ok q] with [q] parsed from [Request.query request] if [request]'s
       method is [`GET] or [`HEAD].}
    {- [Ok q] with [q] parsed from the request body on
       other methods and the content type is
       {!Media_type.application_x_www_form_urlencoded}.
       In this case the {!Webs.Http.Request.query} is ignored.}
    {- [Error _] with a:
    {ul
    {- {!Http.Status.unsupported_media_type_415} response if the
       content type is unsupported}
    {- {!Http.Status.bad_request_400} reponse on decoding errors.}}
    and the body of [request] is {!Http.Body.dismiss}ed}}

    {b Warning.} {!Http.Query.t} values are untrusted, you need to properly
    validate their data. *)

(** {2:clean Path cleaning} *)

val clean_path : t -> (unit, Webs__response.t) result
(** [clean_path request] is:
    {ul
    {- [Ok ()] if [request]'s path is [[]], [[""]] or if it has no empty
       segment.}
    {- [Error _] with a {!Http.Status.moved_permanently_301} to [request]'s
       path without empty segments or the root if that results in the empty
       path and the body of [request] is {!Http.Body.dismiss}ed}}

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
  ?strip:Webs__path.t -> file_root:Webs__path.fpath -> t ->
  (Webs__path.fpath, Webs__response.t) result
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
       fails. The body of [request] is {!Http.Body.dismiss}ed.}} *)

(** {2:etag Etags} *)

val eval_if_none_match :
  t -> Webs__etag.t -> headers:Webs__headers.t ->
  (Webs__headers.t, Webs__response.t) result
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
       {!Etag.eval_if_none_match} returns [false].
       The body of [request] is {!Http.Body.dismiss}ed}
    {- [Error r] with [r] an empty {!Http.Status.bad_request_400} response
       if the {!Headers.if_none_match} decoding errors.
       The body of [request] is {!Http.Body.dismiss}ed}}

    {b Design note.} This slightly abuses the idea of the [result] design
    idea which was rather to carry error responses in the [Error _]
    case. But… it's convenient. *)

(** {2:echo Echo} *)

val echo : ?status:Webs__status.t -> t -> Webs__response.t
(** [echo request] is a response with status [status] (defaults to
    {!Http.Status.ok_200}) and a [text/plain] body that has all the
    properties of [request], including the request body, which is consumed
    by the function with {!Body.to_string}.

    {b Note.} In general using [echo] violates numerous HTTP MUSTs. *)

(** {1:predicates Predicates and comparisons} *)

val is_body_empty : t -> bool
(** [is_body_empty request] is {!Body.is_empty}[ (body request)]. *)

val equal : t -> t -> bool
(** [equal] tests requests for equality. This consumes the body. *)

val compare : t -> t -> int
(** [compare] is a total order on requests compatible with {!equal}. This
    consumes the body. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats requests for inspection. Guarantees not to
    consume the {!val-body}. *)
