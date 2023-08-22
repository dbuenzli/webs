(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** HTTP basic authentication.

    {b Warning.} HTTP basic authentication should not be used:
    {ul
    {- The username and password travel in plain text in the headers of
       each request.}
    {- It is easily amenable to cross-site request forgery attacks.}
    {- It is not possible for end-users to easily log out.}}

    That being said it remains useful as a lightweight protection
    mecanism for demo and testing web sites or to urgently lock down
    part of a website.

    {b If you still decide to use it make sure that everything
    only ever happens over HTTPS.}

    {b References.}
    {ul
    {- R. Fielding et al.
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-http-authentication}
    {e HTTP authentication}}. In RFC 9110. 2022}
    {- J. Reschke.
    {{:https://www.rfc-editor.org/rfc/rfc7617}
    {e The 'Basic' HTTP Authentication Scheme}}. 2015}} *)

open Webs

(** {1:checks Credentials checks} *)

type error =
[ `Unknown_username (** The provided username is unknown. *)
| `Wrong_password (** The provided password is wrong. *) ]
(** The type for credentials check errors. *)

type username = string
(** The type for usernames. Note that since all this is utterly broken
    the user name should not contain [':'] (U+003A) characters. *)

type password = string
(** The type for password. *)

type check = username:username -> password:password -> (unit, error) result
(** The type for credentials check functions.

    {b Warning}. Make sure passwords are tested for equality in
    constant time and that they are stored hashed and salted. *)

(** {1:auth Authenticate requests} *)

val enticate :
  check:check -> realm:string -> Http.Request.t ->
  (username, Http.Response.t) result
(** [enticate ~check ~realm request] is:
    {ul
    {- [Ok username] if a
       {{:https://www.rfc-editor.org/rfc/rfc7617}basic authentication}
       [username] and [password] is found in the
       {!Webs.Http.Headers.authorization} header of [request] and they
       pass the [check] function.}
    {- [Error r] with [r] an empty
       {!Webs.Http.Status.unauthorized_401} response with a challenge for {{:https://www.rfc-editor.org/rfc/rfc9110#name-establishing-a-protection-s}realm} [realm] if there is no {!Webs.Http.Headers.authorization}
       header or if there was one but the credentials [check] failed.
       The exact condition is stored in {!Webs.Http.Response.explain}[ r] for
       your service log. If you add a body to this response, e.g. via
       {!Webs.Http.Response.map_errors}, it
       {e may} be shown by the browser when the user hits cancel on the password
       prompt.}
    {- [Error r] with [r] an empty {!Webs.Http.Status.bad_request_400}
       response if there is a {!Webs.Http.Headers.authorization} header
       but no basic authentication could parsed from it.}} *)
