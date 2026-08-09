(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/browser}Browser} module.

    @canonical Webs_webdriver.Wd_browser *)

val name : string
(** [name] is ["browser"] *)

(** {1:types Types} *)

open Webs_wd__types
open Webs_wd__protocol

module Client_window : sig
  type t = string
  val jsont : t Jsont.t
end

module Client_window_info : sig
  type state = Fullscreen | Maximized | Minimized | Normal
  type t
  val active : t -> bool
  val client_window : t -> Client_window.t
  val height : t -> int
  val state : t -> state
  val width : t -> int
  val x : t -> int
  val y : t -> int
  val jsont : t Jsont.t
  val state_jsont : state Jsont.t
end

module Download_behavior : sig
  type destination_folder = string
  type t = [`Allowed of destination_folder | `Denied ]
end

module Get_client_windows_result : sig
  type t
  val client_windows : t -> Client_window_info.t list
  val jsont : t Jsont.t
end

module User_context_info : sig
  type t
  val make : user_context:User_context.t -> unit -> t
  val user_context : t -> User_context.t
  val jsont : t Jsont.t
end

module Get_user_contexts_result : sig
  type t
  val user_contexts : t -> User_context_info.t list
  val jsont : t Jsont.t
end

(** {1:commands Commands} *)

val close : Connection.t -> ?exts:Exts.t -> unit -> Empty_result.t
(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/browser/close}[browser.close]} *)

val create_user_context :
  Connection.t -> ?exts:Exts.t -> ?accept_insecure_certs:bool ->
  ?proxy:Webs_wd__session.Proxy_configuration.t ->
  ?unhandled_prompt_behavior:Webs_wd__session.User_prompt_handler.t -> unit ->
  User_context_info.t
(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/browser/createUserContext}[browser.createUserContext]} *)

val get_client_windows :
  Connection.t -> ?exts:Exts.t -> unit -> Get_client_windows_result.t
(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/browser/getClientWindows}[browser.getClientWindows]} *)

val get_user_contexts :
  Connection.t -> ?exts:Exts.t -> unit -> Get_user_contexts_result.t
(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/browser/getUserContexts}[browser.getUserContexts]} *)

val remove_user_context :
  Connection.t -> ?exts:Exts.t -> user_context:User_context.t -> unit ->
  Empty_result.t
(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/browser/removeUserContext}[browser.removeUserContext]} *)

val set_client_window_state :
  Connection.t -> ?exts:Exts.t -> client_window:Client_window.t ->
  state:Client_window_info.state -> ?width:int -> ?height:int -> ?x:int ->
  ?y:int -> unit -> Client_window_info.t
(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/browser/setClientWindowState}[browser.setClientWindowState]} *)

val set_download_behavior :
  Connection.t -> ?exts:Exts.t ->
  download_behavior:Download_behavior.t option ->
  ?user_contexts:User_context.t list -> unit -> Empty_result.t
(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/browser/setDownloadBehavior}[browser.setDownloadBehavior]} *)
