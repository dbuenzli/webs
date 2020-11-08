(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** [Webs] tools. *)

open Webs

(** Request deconstruction. *)
module Req_to : sig

  val query : Req.t -> (Http.Query.t, Resp.t) result
  (** [query req] extracts a query from [req]. This is
      {ul
      {- [Ok q] with [q] parsed from [Req.query req] if [req]'s
         method is [`GET].}
      {- [Ok q] with [q] parsed from the request body if [req] is
         [`POST] with a content type of
         {!Webs.Http.Mime_type.application_x_www_form_urlencoded} or
         TODO multipart.}
      {- [Error _] with a
      {!Webs.Http.s405_not_allowed} on other methods,
      {!Webs.Http.s415_unsupported_media_type} if the content-type
      is unsupported or
      {!Webs.Http.s400_bad_request} on decoding errors.}}

      {b TODO.} In POST should we merge with [Req.query] ?
      E.g. for a login form redirect param in url login creds in POST.
      Or maybe alternate combinators
      [(Http.Query.t * Http.Query.t, Resp.t) result]. *)

  (** {1:file Files} *)

  type file_req =
    { path : string;
      if_none_match : string list;
      if_match : string list;
      byte_ranges : (int * int) list; }
  (** The type for file requests. *)

  val file_req : Req.t -> strip:Http.path -> (file_req, Resp.t) result
  (** [file_req r ~strip] *)
end

(** Gateway interaction. *)
module Gateway : sig

  (** {1:sending_file File serving handoff} *)

  val send_file :
    ?prefix:string -> header:Http.name -> Req.t -> (Resp.t, Resp.t) result
  (** [send_file ~prefix ~header r] is:
      {ul
      {- [Ok resp]. An empty {!Webs.Http.s200_ok} response with
         [header] set to [r]'s request target as transformed by
         {!Webs.Http.Path.to_undotted_filepath} and prefixed by [prefix]
         (defaults to ["/"]).}
      {- [Error resp] is a {!Webs.Http.s400_bad_request} if the
         the {!Webs.Http.Path.to_undotted_filepath} conversion fails.}}
      Used with the appropriate [header] this allows to
      handoff the request back to the gateway for it to serve the
      file.  See {{!page-web_service_howto.gateway_files}this section}
      of the web service howto manual. *)

  val x_accel_redirect : Http.name
  (** [x_accel_redirect] is the header name
      {{:https://www.nginx.com/resources/wiki/start/topics/examples/x-accel/}
      ["x-accel-redirect"]} used
      by nginx for internal redirection. *)

  val x_sendfile : Http.name
  (** [x_sendfile] is the header name ["x-sendfile"]. Used by Apache and
      Lightppd for file serving. *)

  (** {1:nginx Nginx specific} *)
end

(** Resource oriented combinators. *)
module Res : sig
  val allow : Http.meth list -> Req.t -> (Req.t, Resp.t) result
  (** [allow ms req] is:
      {ul
      {- [Ok req] if [List.mem (Req.meth r) ms]}
      {- An empty {{:Http.s405_not_allowed}405} [Error _] not allowed
         response, otherwise.}} *)
end

(** MIME type tools. *)
module Mime_type : sig

  val default_of_ext : Http.mime_type Map.Make(String).t Lazy.t
  (** [default_of_ext] is a default file extension (including the [.])
      to MIME type map.

      It is documented by its implementation. Non self-describing
      [text/*] MIME types have a parameter [charset=utf-8] added. *)

  val of_ext :
    ?map:Http.mime_type Map.Make(String).t -> string -> Http.mime_type
  (** [of_ext ~map ext] finds [ext] in [map] (defaults to
      {!default_ext_to_mime}) or defaults to ["application/octet-stream"] *)
end

(** {1:auth Being authentic} *)

(** SHA-256 hashes and HMAC-SHA-256. *)
module Sha_256 : sig

  (** {1:values Hash values} *)

  type t
  (** The type for SHA-256 hashes. *)

  val length : t -> int
  (** [length h] is the length of [h] in bytes (i.e. 32) *)

  val hash : string -> t
  (** [hash s] is the SHA-256 hash of [s]. *)

  val hmac : key:string -> string -> t
  (** [hmac ~key msg] is the {{:https://tools.ietf.org/html/rfc2104}RFC 2104}
      HMAC-SHA-256 for key [key] and message [msg].
      [key] should not be less than 32 bytes. *)

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

    This module defines a simple US-ASCII compatible {{!t}encoding
    scheme} to publish {b non-secret}, expirable data bytes
    authenticatable via a secret private key. Human readability is a
    non-goal, storing your state in non-trusted environments is.

    {b The data is not encrypted}.

    {b TODOs.}
    {ul
    {- {!ptime}, maybe use positive ints in the API and be more stringent about
      data checking. Note that fundamentally we don't need that to be
      POSIX time, a client could use it's own definition of a
      clock. We could also leave the expiration mechanism out but it feels
      convenient.}
    {- {!decode}, are we happy about the erroring structure ?}
    {- {!encode}/{!decode}, provide primed version which compose
       with [{to,of}_string] funs ?}} *)
module Authenticatable : sig

  (** {1:time Time} *)

  type ptime = float
  (** The type for POSIX time as a number of seconds since the epoch
      1970-01-01 00:00:00 UTC. *)

  (** {1:keys Keys} *)

  type key = string
  (** The type for keys. This is used with {{!Sha_256.hmac}[HMAC-SHA-256]},
      so it should be at least 32 bytes long. *)

  val random_key : unit -> key
  (** [random_key ()] are 64 bytes random bytes sourced after having
      called {!Random.self_init}. *)

  (** {1:auth Authenticatable} *)

  type t = string
  (** The type for authenticatable bytes. The encoding scheme for bytes
      [data] and an optional expiration timestamp [expire] is:
{[
expire = match expire with None -> "" | Some e -> string_of_int e
msg = expire + ":" + data
authenticatable = (base64|base64url)(hmac-sha-256(key, msg) + msg)
]}
  *)

  val encode : ?base64url:bool -> key:key -> expire:ptime option -> string -> t
  (** [encode ~key ~expire data] makes data [data] expire at [expire]
      (if any and truncated to seconds) and authenticatable via the
      private key [key]. If [base64url] is [true] (defaults to [false])
      the [base64url] encoding scheme is used instead of [base64]. *)

  val decode :
    ?base64url:bool -> key:key -> now:ptime -> t ->
    (ptime option * string, [`Expired | `Decode | `Authentication]) result
  (** [decode ~key ~now s] authenticates data [s] with the private
      key [key] and makes it expire (if applicable) according to [now].
      The result is:
      {ul
      {- [Ok (expire, data)] with [data] authenticated by [key]
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
    ([`Untrusted of Sha_256.t * ptime option * string], [`Decode]) result
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
    {!Authenticatable.random_key} when you start your service. Note however
    that this invalidates any data currently stored on your clients whenever you
    restart your service – depending on your use case that may be okay,
    or not. *)
module Authenticated_cookie : sig

  val get :
    key:Authenticatable.key -> now:Authenticatable.ptime ->
    name:string -> Req.t -> string option
  (** [get ~key ~now ~name req] is the cookie of [req] named [name]
      authenticated and expired by [key] and [now] (see
      {!Authenticatable.decode}).

      {b TODO.} Any kind of error leads to [None]. *)

  val set :
    ?atts:Http.Cookie.atts -> key:Authenticatable.key ->
    expire:Authenticatable.ptime option -> name:string -> string ->
    Resp.t -> Resp.t
  (** [set ~atts ~key ~expire ~name data resp] sets in [resp] the
      cookie [name] to [data] authenticated
      by [key] and expiring at [expire] (see
      {!Authenticatable.encode}). [atts] are the cookie's attribute
      they default to {!Webs.Http.Cookie.atts_default}. *)
end

(** Sessions.

    Sessions maintain state across request/response cycles. This module
    provides a basic infrastructure to abstract the mecanism handling
    sessions.

    One built-in mecanism is ofered for client-side sessions via
    {!Authenticated_cookie}s.

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

  val setup :
    'a state -> 'a handler ->
    (Req.t -> 'a option -> ('a option * Resp.t)) -> Webs.service
   (** [setup st handler service] handles loading and saving state [st]
       with handler [handler] for service [service] which gets current
       state as argument and should tuple the new state with the request. *)

  val setup' :
    'a state -> 'a handler ->
    (Req.t -> 'a option -> ('a option * (Resp.t, Resp.t) result)) ->
    (Req.t -> (Resp.t, Resp.t) result)
  (** {b TODO.} Add that for now until we settle on something. *)

  (** {1:built-in Built-in session handlers} *)

  val with_authenticated_cookie :
    ?atts:Http.Cookie.atts -> ?name:string -> ?key:Authenticatable.key ->
    unit -> 'a handler
  (** [with_authenticated_cookie ~key] stores state on the client with
      an {!Authenticated_cookie} that can be authenticated with the
      private key [key] (defaults to {!Authenticatable.random_key}).
      [name] is the name of the cookie, it defaults to ["webss"].
      [atts] are the attributes of the cookie, they default to
      {!Webs.Http.Cookie.atts_default}. *)
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
        {- A {{:Webs.Http.s401_unauthorized}401} response
           [Error (cancel req)] with a challenge for [realm] if there
           is no authorization header or if [check] failed. The page is only
           shown if the user cancels, defaults to an english HTML page
           that entices the  user to try again via a link to self.}
        {- A {{:Webs.Http.s400_bad_request}400} bad request [Error resp]
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
