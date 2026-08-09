(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/browsingContext}BrowsingContext} module.

    @canonical Webs_webdriver.Wd_browsing_context *)

val name : string
(** [name] is ["browsingContext"] *)

(** {1:types Types} *)

open Webs_wd__types
open Webs_wd__protocol

module Navigation : sig
  type t = string
  val jsont : t Jsont.t
end

module Box_clip_rectangle : sig
  type t
  val make : x:float -> y:float -> width:float -> height:float -> unit -> t
  val x : t -> float
  val y : t -> float
  val width : t -> float
  val height : t -> float
  val jsont : t Jsont.t
end

module Capture_screenshot_result : sig
  type t
  val data : t -> string
  val jsont : t Jsont.t
end

module Clip_rectangle : sig
  type t =
  [ `Box_clip of Box_clip_rectangle.t
  | `Element_clip of Webs_wd__script.Shared_reference.t ]
  val jsont : t Jsont.t
end

module Create_result : sig
  type t
  val context : t -> Browsing_context.t
  val user_context : t -> User_context.t option
  val jsont : t Jsont.t
end

module Download_will_begin_params : sig
  type t
  val context : t -> Browsing_context.t
  val navigation : t -> Navigation.t option
  val timestamp : t -> int
  val url : t -> string
  val user_context : t -> Webs_wd__types.User_context.t option
  val suggested_filename : t -> string
  val jsont : t Jsont.t
end

module Download_end_params : sig
  type t
  val context : t -> Browsing_context.t
  val navigation : t -> Navigation.t option
  val timestamp : t -> int
  val url : t -> string
  val user_context : t -> Webs_wd__types.User_context.t option
  val status : t -> [ `Canceled | `Complete of string option ]
  val jsont : t Jsont.t
end

module History_updated_params : sig
  type t
  val context : t -> Browsing_context.t
  val timestamp : t -> int
  val url : t -> string
  val user_context : t -> User_context.t option
  val jsont : t Jsont.t
end

module Image_format : sig
  type t
  val make : ?quality:float -> type':string -> unit -> t
  val type' : t -> string
  val quality : t -> float option
  val jsont : t Jsont.t
end

module Info : sig
  type t
  val children : t -> t list option
  val client_window : t -> Webs_wd__browser.Client_window.t
  val context : t -> Browsing_context.t
  val original_opener : t -> Browsing_context.t option
  val url : t -> string
  val user_context : t -> User_context.t
  val parent : t -> Browsing_context.t option
  val jsont : t Jsont.t
end

module Get_tree_result : sig
  type t
  val contexts : t -> Info.t list
  val jsont : t Jsont.t
end

module Locator : sig
  module Accessibility : sig
    type t
    val make : ?name:string -> ?role:string -> unit -> t
    val name : t -> string option
    val role : t -> string option
  end
  module Css : sig
    type t = string
  end
  module Context : sig
    type t = Browsing_context.t
  end
  module Inner_text : sig
    type match_type = Full | Partial
    type t
    val make :
      value:string  -> ?ignore_case:bool -> ?match_type:match_type ->
      ?max_depth:int -> unit -> t

    val value : t -> string
    val ignore_case : t ->  bool option
    val match_type : t -> match_type option
    val max_depth : t -> int option
  end
  module Xpath : sig
    type t = string
  end
  type t =
  [ `Accessibility of Accessibility.t
  | `Css of Css.t
  | `Context of Context.t
  | `Inner_text of Inner_text.t
  | `Xpath of Xpath.t ]

  val jsont : t Jsont.t
end

module Locate_nodes_result : sig
  type t
  val nodes : t -> Webs_wd__script.Node_remote_value.t list
  val jsont : t Jsont.t
end

module Navigate_result : sig
  type t
  val navigation : t -> Navigation.t option
  val url : t -> string
  val jsont : t Jsont.t
end

module Navigation_info : sig
  type t
  val context : t -> Browsing_context.t
  val navigation : t -> Navigation.t option
  val timestamp : t -> int
  val url : t -> string
  val user_context : t -> User_context.t option
  val jsont : t Jsont.t
end

module Print_result : sig
  type t
  val data : t -> string
  (** [data r] is a Base64 encoded PDF file. *)

  val jsont : t Jsont.t
end

module Print_margin : sig
  type t
  val make :
    ?bottom:float -> ?left:float -> ?right:float -> ?top:float -> unit -> t
  val bottom : t -> float option
  val left : t -> float option
  val right : t -> float option
  val top : t -> float option
  val jsont : t Jsont.t
end

module Print_orientation : sig
  type t = [ `Landscape | `Portrait ]
end

module Print_page : sig
  type t
  val make : ?width:float -> ?height:float -> unit -> t
  val width : t -> float option
  val height : t -> float option
  val a4 : t
  val jsont : t Jsont.t
end

module Readiness_state : sig
  type t = [ `None | `Interactive | `Complete ]
  val jsont : t Jsont.t
end

module User_prompt_type : sig
  type t = [`Alert | `Beforeunload | `Confirm | `Prompt ]
  val jsont : t Jsont.t
end

module User_prompt_closed_params : sig
  type t
  val context : t -> Browsing_context.t
  val accepted : t -> bool
  val type' : t -> User_prompt_type.t
  val user_context : t -> User_context.t option
  val user_text : t -> string option
  val jsont : t Jsont.t
end

module User_prompt_opened_params : sig
  type t
  val context : t -> Webs_wd__types.Browsing_context.t
  val handler : t -> Webs_wd__session.User_prompt_handler_type.t
  val message : t -> string
  val type' : t -> User_prompt_type.t
  val user_context : t -> Webs_wd__types.User_context.t option
  val default_value : t -> string option
  val jsont : t Jsont.t
end

module Viewport : sig
  type t
  val make : width:int -> height:int -> unit -> t
  val width : t -> int
  val height : t -> int
  val jsont : t Jsont.t
end

(** {1:commands Commands} *)

val activate :
  Connection.t -> ?exts:Exts.t -> context:Browsing_context.t -> unit ->
  Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-browsingContext-activate}
    [browsingContext.activate]} *)

val capture_screenshot :
  Connection.t -> ?exts:Exts.t -> context:Browsing_context.t ->
  ?origin:[`Viewport | `Document] -> ?format:Image_format.t ->
  ?clip:Clip_rectangle.t -> unit -> Capture_screenshot_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-browsingContext-captureScreenshot}[browsingContext.captureScreenshot]}, [format] is [image/png] if unspecified. *)

val close :
  Connection.t -> ?exts:Exts.t -> context:Browsing_context.t ->
  ?prompt_unload:bool -> unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-browsingContext-close}
    [browsingContext.close]} *)

val create :
  Connection.t ->
  ?exts:Exts.t ->  type':[ `Tab | `Window ] -> ?user_context:User_context.t ->
  ?background:bool -> ?reference_context:Browsing_context.t ->unit ->
  Create_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-browsingContext-create}
    [browsingContext.create]} *)

val get_tree :
  Connection.t -> ?exts:Exts.t -> ?max_depth:int -> ?root:Browsing_context.t ->
  unit -> Get_tree_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-browsingContext-getTree}
    [browsingContext.getTree]} *)

val handle_user_prompt :
  Connection.t -> ?exts:Exts.t -> context:Browsing_context.t ->
  ?accept:bool -> ?user_text:string -> unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-browsingContext-handleUserPrompt}[browsingContext.handleUserPrompt]} *)

val locate_nodes :
  Connection.t -> ?exts:Exts.t -> context:Browsing_context.t ->
  locator:Locator.t -> ?max_node_count:int ->
  ?serialization_options:Webs_wd__script.Serialization_options.t ->
  ?start_nodes:Webs_wd__script.Shared_reference.t list ->
  unit -> Locate_nodes_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-browsingContext-locateNodes}
    [browsingContext.locateNodes]} *)

val navigate :
  Connection.t -> ?exts:Exts.t -> context:Browsing_context.t -> url:string ->
  ?wait:Readiness_state.t -> unit -> Navigate_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-browsingContext-navigate}
    [browsingContext.navigate]} *)

val print :
  Connection.t -> ?exts:Exts.t -> context:Browsing_context.t ->
  ?background:bool -> ?margin:Print_margin.t ->
  ?orientation:Print_orientation.t -> ?page:Print_page.t ->
  ?page_ranges:(int * int) list -> ?scale:float -> ?shrink_to_fit:bool ->
  unit -> Print_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-browsingContext-print}
    [browsingContext.print]} *)

val reload :
  Connection.t -> ?exts:Exts.t -> context:Browsing_context.t ->
  ?ignore_cache:bool -> ?wait:Readiness_state.t -> unit -> Navigate_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-browsingContext-reload}
    [browsingContext.reload]} *)

val set_bypass_csp :
  Connection.t -> ?exts:Exts.t -> bypass:bool ->
  ?contexts:Browsing_context.t list -> ?user_contexts:User_context.t list ->
  unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-browsingContext-setBypassCSP}[browsingContext.setBypassCSP]} *)

val set_viewport :
  Connection.t -> ?exts:Exts.t -> ?context:Browsing_context.t ->
  ?viewport:Viewport.t option -> ?device_pixel_ratio:float option ->
  ?user_contexts:User_context.t list -> unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-browsingContext-setViewport}[browsingContext.setViewport]} *)

val traverse_history :
  Connection.t -> ?exts:Exts.t -> context:Browsing_context.t ->
  delta:int -> unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-browsingContext-traverseHistory}[browsingContext.traverseHistory]} *)

(** {1:events Events} *)

val context_created : Info.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-browsingContext-contextCreated}[browsingContext.contextCreated]} *)

val context_destroyed : Info.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-browsingContext-contextDestroyed}[browsingContext.contextDestroyed]} *)

val dom_content_loaded : Navigation_info.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-browsingContext-domContentLoaded}[browsingContext.domContentLoaded]} *)

val download_will_begin : Download_will_begin_params.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-browsingContext-downloadWillBegin}[browsingContext.downloadWillBegin]} *)

val download_end : Download_end_params.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-browsingContext-downloadEnd}[browsingContext.downloadEnd]} *)

val fragment_navigated : Navigation_info.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-browsingContext-fragmentNavigated}[browsingContext.fragmentNavigated]} *)

val history_updated : History_updated_params.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-browsingContext-historyUpdated}[browsingContext.historyUpdated]} *)

val load : Navigation_info.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-browsingContext-load}
    [browsingContext.load]} *)

val navigation_started : Navigation_info.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-browsingContext-navigationStarted}[browsingContext.navigationStarted]} *)

val navigation_aborted : Navigation_info.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-browsingContext-navigationAborted}[browsingContext.navigationAborted]} *)

val navigation_committed : Navigation_info.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-browsingContext-navigationCommitted}[browsingContext.navigationCommitted]} *)

val navigation_failed : Navigation_info.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-browsingContext-navigationFailed}[browsingContext.navigationFailed]} *)

val user_prompt_closed : User_prompt_closed_params.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-browsingContext-userPromptClosed}[browsingContext.userPromptClosed]} *)

val user_prompt_opened : User_prompt_opened_params.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-browsingContext-userPromptOpened}[browsingContext.userPromptOpened]} *)
