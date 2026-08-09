(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module String_map : Map.S with type key = string

module Js_int : sig
  type t = int
  val jsont : t Jsont.t
end

module Js_uint : sig
  type t = int
  val jsont : t Jsont.t
end

(** WebDriver extensions (unknown object members).

    @canonical Webs_webdriver.Wd.Exts *)
module Exts : sig
  type t = Jsont.json Map.Make(Stdlib.String).t
  (** The type for
      {{:https://www.w3.org/TR/webdriver-bidi/#cddl-type-extensible}
      [Extensible]}. Maps unknown object members to their JSON value. *)

  val none : t
  (** No extensions (empty map). *)

  val jsont : (t, Jsont.json, t) Jsont.Object.Mems.map
  (** [jsont] is a representation to use with {!Jsont.Object.keep_unknown}. *)
end

(** Empty parameters.

    @canonical Webs_webdriver.Wd.Empty_params *)
module Empty_params : sig
  type t
  (** The type for
      {{:https://www.w3.org/TR/webdriver-bidi/#cddl-type-emptyparams}
      [EmptyParams]}.

      Formally this is [unit] but drivers can add their own fields. *)

  val make : ?exts:Exts.t -> unit -> t
  val empty : t
  val exts : t -> Exts.t
  val jsont : t Jsont.t
end

(** Empty method results.

    @canonical Webs_webdriver.Wd.Empty_result *)
module Empty_result : sig
  type t
  (** The type for
      {{:https://www.w3.org/TR/webdriver-bidi/#cddl-type-emptyresult}
      [EmptyResult]}.

      Formally this is [unit] but drivers can add their own fields. *)

  val make : ?exts:Exts.t -> unit -> t
  val empty : t
  val exts : t -> Exts.t
  val jsont : t Jsont.t
end

(** Commands. *)
module Command : sig
  type 'params instance
  type ('params, 'result) t

  val name : ('params, 'result) t -> string
  val encode_jsont : ('params, 'result) t -> 'params Jsont.t
  val result_jsont : ('params, 'result) t -> 'result Jsont.t

  val make : ?exts:Exts.t -> int -> string -> 'params -> 'params instance
  val define : string ->
    params_jsont:'params Jsont.t -> result_jsont:'result Jsont.t ->
    ('params instance, 'result) t
end

(** Command responses. *)
module Command_response : sig
  type t
  val id : t -> int
  val result : t -> Jsont.json
  val exts : t -> Exts.t
end

(** Error responses.

    @canonical Webs_webdriver.Wd.Error.Response *)
module Error_response : sig
  type t
  (** The type for
      {{:https://www.w3.org/TR/webdriver-bidi/#cddl-type-errorresponse}
      [ErrorResponse]}. *)

  val make :
    ?exts:Exts.t -> ?stacktrace:string -> ?id:int ->
    error:string -> message:string -> unit -> t

  val id : t -> int option
  val error : t -> string
  val message : t -> string
  val stacktrace : t -> string option
  val exts : t -> Exts.t
  val jsont : t Jsont.t
end


module Event : sig
  type 'a t (** @canonical Webs_webdriver.Wd.Event.t *)


  val define : string -> params_jsont:'a Jsont.t -> 'a t
  val name : 'a t -> string
  val params_jsont : 'a t -> 'a Jsont.t

  type instance

  val method' : instance -> string
  val params : instance -> Jsont.json
  val exts : instance -> Exts.t
end

(** Messages. *)
module Message : sig
  type t =
  | Command_response : Command_response.t -> t
  | Error_response : Error_response.t -> t
  | Event : Event.instance -> t

  val jsont : t Jsont.t
end

(** @canonical Webs_webdriver.Wd.User_context *)
module User_context : sig
  type t = string
  (** The type for
      {{:https://www.w3.org/TR/webdriver-bidi/#type-browser-UserContext}
      [browser.UserContext]}. *)

  val jsont : t Jsont.t
end

(** @canonical Webs_webdriver.Wd.Browsing_context *)
module Browsing_context : sig
  type t = string
  (** The type for
      {{:https://www.w3.org/TR/webdriver-bidi/#type-browsingContext-Browsingcontext}[browsingContext.BrowsingContext]}. *)

  val jsont : string Jsont.t
end
