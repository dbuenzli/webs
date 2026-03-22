(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** HTTP clients.

    See the {{!page-index.quick_start_fetch}quick start}
    and the {{!page-cookbook.fetching}cookbook}.

    @canonical Webs.Http.Client *)

(** {1:clients Clients} *)

val default_max_redirection : int
(** [default_max_redirection] is [10], the default maximal number of
    redirections when they are followed, see {!val-request}. *)

(** Cookie persistence and request pre-configuration. *)
module Session : sig
  type t
  (** The type for sessions. TODO *)
end

type t
(** The type for HTTP clients.  *)

val id : t -> string
(** [id httpc] identifies the underlying implementation of [httpc]. *)

val session : t -> Session.t option
(** [session httpc] is the session of [httpc] (if any). *)

val request :
  ?max_redirections:int -> t -> follow:bool ->
  Webs__request.t -> (Webs__response.t, string) result
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

    The maximal number of redirection is given by [max_redirection] and
    defaults to {!default_max_redirection}.

    If, and only if, there was a follow, the final requested URL can
    be found in the response in the {!x_follow_location} header. *)

val get : t -> follow:bool -> url:Webs__url.t -> (string, string) result
(** [get c ~follow ~url] is the body of a successful [GET] request
    on [url]. For the semantics of [follow] see {!request}.

    {b Note.} This is voluntarily kept bare bones (e.g. no headers
    can be specified). Anything more complex should use {!request}. *)

val x_follow_location : Webs__headers.Name.t
(** [x_follow_location] is the final location that was requested
    on [~follow:true]. Only added if there was a redirection. *)

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

  val request : t -> Webs__request.t -> (Webs__response.t, string) result
  (** [request httpc request] perform request [request] with [httpc].

      This function should follow the
      {{!page-connector_conventions.client_connectors}client
      connector conventions}. *)
end

val make : (module T with type t = 'a) -> 'a -> Session.t option -> t
(** [make impl httpc session] packs an HTTP client implementation [impl] and
    its specific implementation [httpc] along with an optional [session]. *)
