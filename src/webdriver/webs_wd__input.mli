(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/input}Input} module.

    @canonical Webs_webdriver.Wd_input *)

open Webs_wd__types
open Webs_wd__protocol

val name : string
(** [name] is ["input"]. *)

(** {1:types Types} *)

module File_dialog_info : sig
  type t
  val context : t -> Browsing_context.t
  val user_context : t -> User_context.t option
  val element : t -> Webs_wd__script.Shared_reference.t option
  val multiple : t -> bool
  val jsont : t Jsont.t
end

module Element_origin : sig
  type t
  val make : element:Webs_wd__script.Shared_reference.t -> unit -> t
  val element : t -> Webs_wd__script.Shared_reference.t
  val jsont : t Jsont.t
end

module Origin : sig
  type t = [ `Element of Element_origin.t | `Pointer | `Viewport ]
  val jsont : t Jsont.t
end

module Pause_action : sig
  type t
  val make : ?duration:int -> unit -> t
  val duration : t -> int option
  val jsont : t Jsont.t
end

module Key_down_action : sig
  type t
  val make : value:string -> unit -> t
  val value : t -> string
  val jsont : t Jsont.t
end

module Key_up_action : sig
  type t
  val make : value:string -> unit -> t
  val value : t -> string
  val jsont : t Jsont.t
end

module Pointer_up_action : sig
  type t
  val make : button:int -> unit -> t
  val button : t -> int
  val jsont : t Jsont.t
end

module Pointer_down_action : sig
  type t
  val make :
    ?width:int -> ?height:int -> ?pressure:float ->
    ?tangential_pressure:float -> ?twist:float -> ?altitude_angle:float ->
    ?azimuth_angle:float -> button:int -> unit -> t
  val button : t -> int
  val width : t -> int option
  val height : t -> int option
  val pressure : t -> float option
  val tangential_pressure : t -> float option
  val twist : t -> float option
  val altitude_angle : t -> float option
  val azimuth_angle : t -> float option
  val jsont : t Jsont.t
end

module Pointer_move_action : sig
  type t
  val make :
    ?width:int -> ?height:int -> ?pressure:float ->
    ?tangential_pressure:float -> ?twist:float -> ?altitude_angle:float ->
    ?azimuth_angle:float -> ?origin:Origin.t -> ?duration:int ->
    x:float -> y:float -> unit -> t
  val x : t -> float
  val y : t -> float
  val duration : t -> int option
  val origin : t -> Origin.t option
  val width : t -> int option
  val height : t -> int option
  val pressure : t -> float option
  val tangential_pressure : t -> float option
  val twist : t -> float option
  val altitude_angle : t -> float option
  val azimuth_angle : t -> float option
  val jsont : t Jsont.t
end

module Wheel_scroll_action : sig
  type t
  val make :
    ?origin:Origin.t -> ?duration:int -> x:int -> y:int ->
    delta_x:int -> delta_y:int -> unit -> t
  val x : t -> int
  val y : t -> int
  val delta_x : t -> int
  val delta_y : t -> int
  val duration : t -> int option
  val origin : t -> Origin.t option
  val jsont : t Jsont.t
end

module None_source_action : sig
  type t = [ `Pause of Pause_action.t ]
  val jsont : t Jsont.t
end

module None_source_actions : sig
  type t
  val make : id:string -> actions:None_source_action.t list -> unit -> t
  val id : t -> string
  val actions : t -> None_source_action.t list
  val jsont : t Jsont.t
end

module Key_source_action : sig
  type t =
  [ `Key_down of Key_down_action.t
  | `Key_up of Key_up_action.t
  | `Pause of Pause_action.t ]
  val jsont : t Jsont.t
end

module Key_source_actions : sig
  type t
  val make : id:string -> actions:Key_source_action.t list -> unit -> t
  val id : t -> string
  val actions : t -> Key_source_action.t list
  val jsont : t Jsont.t
end

module Pointer_type : sig
  type t = [ `Mouse | `Pen | `Touch ]
  val jsont : t Jsont.t
end

module Pointer_parameters : sig
  type t
  val make : ?pointer_type:Pointer_type.t -> unit -> t
  val pointer_type : t -> Pointer_type.t option
  val jsont : t Jsont.t
end

module Pointer_source_action : sig
  type t =
  [ `Pause of Pause_action.t
  | `Pointer_down of Pointer_down_action.t
  | `Pointer_move of Pointer_move_action.t
  | `Pointer_up of Pointer_up_action.t ]
  val jsont : t Jsont.t
end

module Pointer_source_actions : sig
  type t
  val make :
    ?parameters:Pointer_parameters.t ->
    id:string -> actions:Pointer_source_action.t list -> unit -> t
  val id : t -> string
  val parameters : t -> Pointer_parameters.t option
  val actions : t -> Pointer_source_action.t list
  val jsont : t Jsont.t
end

module Wheel_source_action : sig
  type t =
  [ `Pause of Pause_action.t
  | `Wheel_scroll of Wheel_scroll_action.t ]
  val jsont :t Jsont.t
end

module Wheel_source_actions : sig
  type t
  val make : id:string -> actions:Wheel_source_action.t list -> unit -> t
  val id : t -> string
  val actions : t -> Wheel_source_action.t list
  val jsont : t Jsont.t
end

module Source_actions : sig
  type t =
  [ `Key of Key_source_actions.t
  | `None of None_source_actions.t
  | `Pointer of Pointer_source_actions.t
  | `Wheel of Wheel_source_actions.t ]
  val jsont : t Jsont.t
end

(** {1:commands Commands} *)

val perform_actions :
  Connection.t -> ?exts:Exts.t -> context:Browsing_context.t ->
  actions:Source_actions.t list -> unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-input-performActions}
    [input.performActions]} *)

val release_actions :
  Connection.t -> ?exts:Exts.t -> context:Browsing_context.t ->
  unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-input-releaseActions}
    [input.releaseActions]} *)

val set_files :
  Connection.t -> ?exts:Exts.t -> context:Browsing_context.t ->
  element:Webs_wd__script.Shared_reference.t -> files:string list -> unit ->
  Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-input-setFiles}
    [input.setFiles]} *)

(** {1:events Events} *)

val file_dialog_opened : File_dialog_info.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-input-fileDialogOpened}
    [input.fileDialogOpened]} *)
