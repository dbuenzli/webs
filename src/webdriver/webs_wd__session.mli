(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/session}Session} module.

    @canonical Webs_webdriver.Wd_session *)

val name : string
(** [name] is ["session"]. *)

(** {1:types Types} *)

open Webs_wd__types
open Webs_wd__protocol


module User_prompt_handler_type : sig
  type t = Accept | Dismiss | Ignore (** *)
  val jsont : t Jsont.t
end

module User_prompt_handler : sig
  type t
  val make :
    ?alert:User_prompt_handler_type.t ->
    ?before_unload:User_prompt_handler_type.t ->
    ?confirm:User_prompt_handler_type.t ->
    ?default:User_prompt_handler_type.t ->
    ?file:User_prompt_handler_type.t ->
    ?prompt:User_prompt_handler_type.t -> unit -> t
  val alert : t -> User_prompt_handler_type.t option
  val before_unload : t -> User_prompt_handler_type.t option
  val confirm : t -> User_prompt_handler_type.t option
  val default : t -> User_prompt_handler_type.t option
  val file : t -> User_prompt_handler_type.t option
  val prompt : t -> User_prompt_handler_type.t option
  val jsont : t Jsont.t
end

module Proxy_configuration : sig
  module Autodetect : sig
    type t
    val make : ?exts:Exts.t -> unit -> t
    val exts : t -> Exts.t
    val jsont : t Jsont.t
  end
  module Direct : sig
    type t
    val make : ?exts:Exts.t -> unit -> t
    val exts : t -> Exts.t
    val jsont : t Jsont.t
  end
  module Manual : sig
    type t
    val make :
      ?exts:Exts.t -> ?http_proxy:string -> ?ssl_proxy:string ->
      ?socks_proxy:string -> ?socks_version:int -> ?no_proxy:string list ->
      unit -> t

    val http_proxy : t -> string option
    val ssl_proxy : t -> string option
    val socks_proxy : t -> string option
    val socks_version : t -> int option
    val no_proxy : t -> string list option
    val exts : t -> Exts.t
    val jsont : t Jsont.t
  end

  module Pac : sig
    type t
    val make : ?exts:Exts.t -> proxy_autoconfig_url:string -> unit -> t
    val proxy_autoconfig_url : t -> string
    val exts : t -> Exts.t
    val jsont : t Jsont.t
  end

  module System : sig
    type t
    val make : ?exts:Exts.t -> unit -> t
    val exts : t -> Exts.t
    val jsont : t Jsont.t
  end

  type t =
  | Autodetect of Autodetect.t
  | Direct of Direct.t
  | Manual of Manual.t
  | Pac of Pac.t
  | System of System.t
  | Empty of unit (** Workaround Firefox empty object *)

  val jsont : t Jsont.t
end

module Capability_request : sig
  type t
  val make :
    ?exts:Exts.t -> ?accept_insecure_certs:bool -> ?browser_name:string ->
    ?browser_version:string -> ?platform_name:string ->
    ?proxy:Proxy_configuration.t ->
    ?unhandled_prompt_behaviour:User_prompt_handler.t -> unit -> t

  val jsont : t Jsont.t
end

module Capabilities_request : sig
  type t
  val make :
    ?always_match:Capability_request.t ->
    ?first_match:Capability_request.t list -> unit -> t

  val jsont : t Jsont.t
end

module Capabilities : sig
  type t
  val accept_insecure_certs : t -> bool
  val browser_name : t -> string
  val browser_version : t -> string
  val platform_name : t -> string
  val set_window_rect : t -> bool
  val user_agent : t -> string
  val proxy : t -> Proxy_configuration.t option
  val unhandled_prompt_behaviour : t -> User_prompt_handler.t option
  val websocket_url : t -> string option
  val exts : t -> Exts.t
  val jsont : t Jsont.t
end

module New_result : sig
  type t
  val session_id : t -> string
  val capabilities : t -> Capabilities.t
  val jsont : t Jsont.t
end

module Status_result : sig
  type t
  val ready : t -> bool
  val message : t -> string
  val jsont : t Jsont.t
end

module Subscription : sig
  type t = string
  val jsont : t Jsont.t
end

module Subscribe_result : sig
  type t
  val subscription : t -> Subscription.t
  val jsont : t Jsont.t
end

(** {1:commands Commands} *)

val status : Connection.t -> ?exts:Exts.t -> unit -> Status_result.t
(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/session/new}[session.status]} *)

val new' :
  Connection.t -> ?exts:Exts.t -> ?capabilities:Capabilities_request.t ->
  unit -> New_result.t
(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/session/new}[session.new]} *)

val end' : Connection.t -> ?exts:Exts.t -> unit -> Empty_result.t
(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/session/end}[session.end]} *)

val subscribe :
  Connection.t -> ?exts:Exts.t -> events:string list ->
  ?contexts:Browsing_context.t list -> ?user_contexts:User_context.t list ->
  unit -> Subscribe_result.t
(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/session/subscribe}[session.subscribe]}.
    {ul
    {- For the [events] argument use
    {!Wd.Event.name} on an event like {!Wd_log.entry_added}
    or a module name like {!Wd_browsing_context.name}}}  *)

val unsubscribe :
  Connection.t -> ?exts:Exts.t ->
  [ `Events of string list | `Subscriptions of Subscription.t list ] ->
  unit -> Empty_result.t
(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/session/unsubscribe}[session.unsubscribe]} *)
