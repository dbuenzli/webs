(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Authenticated cookies.

    An authenticated cookie uses the {!Webs_authenticatable} scheme to let
    your service store expirable state on the client with the
    guarantee that it cannot tamper with it.

    {b The data is not encrypted.} *)

open Webs

(** {1:setclear Setting and clearing} *)

val set :
  private_key:Webs_authenticatable.Private_key.t ->
  expire:Webs_authenticatable.time option ->
  ?attributes:Http.Cookie.attributes ->
  name:Http.Cookie.name -> string -> Http.Response.t -> Http.Response.t
(** [set ~private_key ~expire ~attributes ~name data response] sets in
    response [response] the cookie [name] to [data] authenticated by
    [private_key] and expiring at [expire]. [attributes] are the
    cookie's attributes, they default to {!Webs.Http.Cookie.default_attributes}.

    {b Note.} The expiration [expire], if provided, expires the
    authenticated data (see {!Webs_authenticatable.encode}).  It does
    not affect HTTP cookie expiration. Use the [max_age] attribute of
    {!Webs.Http.Cookie.val-attributes} for that.  *)

val clear :
  ?attributes:Http.Cookie.attributes -> name:Http.Cookie.name ->
  Http.Response.t -> Http.Response.t
(** [clear ~attributes ~name response] clears the cookie named [name]
    in [response] by setting its [max-age] to [-1] and value to
    [""]. [attributes] should be the same value as the one given to
    {!set}, its [max_age] attribute gets overriden with a [-1] by the
    function, they default to {!Webs.Http.Cookie.default_attributes}. *)

(** {1:get Getting} *)

type error =
[ Webs_authenticatable.error (** Authenticatable data errors. *)
| `Cookie of string (** Cookie decoding errors. *) ]
(** The type for authenticated cookie decode and authentication
    errors. *)

val error_message : error -> string
(** [error_message e] is an english error message for [e]. *)

val error_string : ('a, error) result -> ('a, string) result
(** [error_string r] is [Result.map_error error_message r]. *)

val find :
  private_key:Webs_authenticatable.Private_key.t ->
  now:Webs_authenticatable.time option ->
  name:Http.Cookie.name -> Http.Request.t ->
  ((Webs_authenticatable.time option * string) option, error) result
(** [find ~private_key ~now ~name request] is the cookie of [request] named
    [name] authenticated and expired by [private_key] and [now]. This
    is [Ok None] if no cookie named [name] could be found or if its
    value is [""].

    {b Note.} If the cookie was {!set} with an [expire] you need
    to provide a [now] otherwise it will never authenticate, see
    {!Webs_authenticatable.decode} for more details. *)
