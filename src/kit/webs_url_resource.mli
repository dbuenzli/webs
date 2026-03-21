(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** URL resolution helpers. *)

open Webs

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

      Given a request's parsed identifier [req_id] and name [req_name] (if any):
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
      {- Suppresses initial and final ['-']s and compresses consecutive ['-'].}
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
