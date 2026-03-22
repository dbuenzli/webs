(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Cookies.

    Support for {{:https://www.rfc-editor.org/rfc/rfc6265}cookies}.

    @canonical Webs.Http.Cookie *)

type name = string
(** The type for cookie names. *)

type attributes
(** The type for
    {{:https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie#attributes}
    cookie attributes}. *)

val default_attributes : attributes
(** [default_attributes] are cookie attributes with [secure] set
    to [true], [http_only] set to [true], [same_site] set to
    ["strict"] and no other attribute specified. *)

val attributes :
  ?init:attributes ->
  ?domain:string option -> ?http_only:bool -> ?max_age:int option ->
  ?path:Webs__path.t -> ?same_site:string -> ?secure:bool -> unit ->
  attributes
(** [atts ()] are the given {{:https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie#attributes}cookie attributes}. Those unspecified take
    the value of [init] which defaults to {!default_attributes}. *)

(** {1:converting Converting} *)

val encode : ?attributes:attributes -> name:name -> string -> string
(** [encodes ~atts name value] encodes a cookie named [name] with
    value [value] and attributes [atts] (defaults to
    {!default_attributes}) for {!Headers.add_set_cookie}. *)

val decode_list : string -> ((name * string) list, string) result
(** [decode_list s] parses the
    {{:https://www.rfc-editor.org/rfc/rfc6265#section-4.2.1}cookie string}
    of a {!Headers.cookie} header value. *)
