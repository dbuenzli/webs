(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Session handling.

    Sessions maintain state across request-response cycles. This
    module provides a basic mechanism to abstract session handlers.

    A handler {{!section-client_stored}implementation} using
    {!Webs_authenticated_cookie}s for storing session state on the
    clients is also provided.  *)

open Webs

(** State descriptors.

    {b FIXME.} encode allow errors ? [pp] ? Ideally this
    would simply take a typegist. *)
module State : sig

  type 'a t
  (** The type for describing session state of type ['a]. Values of this type
      describe to {{!Webs_session.Handler.t}session handlers} how codec
      values of type ['a] with bytes and assert them for equality. *)

  val make :
    encode:('a -> string) -> decode:(string -> ('a, string) result) ->
    equal:('a -> 'a -> bool) -> unit -> 'a t
  (** [make ~encode ~decode ~equal ()] tests state for equality with
      [eq], encodes states to bytes with [encode], decodes states from bytes
      with [decode] and tests state for equality with [equal] *)

  val encode : 'a t -> ('a -> string)
  (** [encode sd] is the encoding function of [sd]. *)

  val decode : 'a t -> (string -> ('a, string) result)
  (** [decode sd] is the decoding function of [sd]. *)

  val equal : 'a t -> ('a -> 'a -> bool)
  (** [equal sd] is the equality function of [sd]. *)

  val option_equal : 'a t -> 'a option -> 'a option -> bool
  (** [option_equal sd] is {!equal} trivially extended to optional state. *)
end

(** Session handlers. *)
module Handler : sig

  type ('a, 'e) t
    (** The type for session handler with state of type ['a] and state
        loading errors of type ['e]. Values of this type are in charge of
        loading the state when a request is received and saving it when
        a response is sent back and the state has changed. *)

  val v :
    load:('a State.t -> Http.Request.t -> ('a option, 'e) result) ->
    save:('a State.t -> 'a option -> Http.Response.t -> Http.Response.t) ->
    unit -> ('a, 'e) t
  (** [handler ~load ~save ()] is a session handler using [load]
        to setup the session state and [save] to save it before responding.

        Sessions as are represented by ['a option] values. On [load] a [None]
        indicates there is no session. On [save] a [None] indicates to
        drop the session.

        {b Warning.} [save] gets invoked [iff] the state to save is different
        from the one that was loaded. *)

  val load :
    ('a, 'e) t -> ('a State.t -> Http.Request.t -> ('a option, 'e) result)
  (** [load h] is the state loading function of [h]. *)

  val save :
    ('a, 'e) t ->
    ('a State.t -> 'a option -> Http.Response.t -> Http.Response.t)

    (** [save h] is the state saving function of [h]. *)
end

type 'a response = 'a option * Http.Response.t
(** The type for session responses. *)

val setup :
  'a State.t -> ('a, 'e) Handler.t ->
  (('a option, 'e) result -> Http.Request.t -> 'a response) ->
  (Http.Request.t -> Http.Response.t)
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
[ Webs_authenticated_cookie.error (** Authenticated cookie errors. *)
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
  private_key:Webs_authenticatable.Private_key.t ->
  ?attributes:Http.Cookie.attributes ->
  name:string -> unit -> ('a, client_stored_error) Handler.t
(** [client_stored ~private_key ~atts ~name] stores
    {{:expiring}non-expirable} state on the client in an
    {!Webs_authenticated_cookie} authenticated with the private key
    [private_key], named [name] and with attributes [atts]
    [atts] (defaults to {!Webs.Http.Cookie.default_attributes}).

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

    The {!State.t} type could be a heterogenous dictionary with
    serializable values. This makes state highly composable, but {e
    implicitely} composable and thus harder to understand and audit.

    For now we would rather try to explore how inconvenient explicit
    state design and composition is or becomes in practice. *)
