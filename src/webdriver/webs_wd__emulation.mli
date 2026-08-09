(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/emulation}Emulation} module.

    @canonical Webs_webdriver.Wd_emulation *)

open Webs_wd__types
open Webs_wd__protocol

val name : string
(** [name] is ["emulation"]. *)

(** {1:types Types} *)

module Geolocation_coordinates : sig
  type t
  (** The type for
      {{:https://www.w3.org/TR/webdriver-bidi/#cddl-type-emulationgeolocationcoordinates}[emulation.GeolocationCoordinates]}. *)

  val make :
    ?speed:float -> ?heading:float -> ?altitude_accuracy:float ->
    ?altitude:float -> ?accuracy:float -> latitude:float -> longitude:float ->
    unit -> t

  val jsont : t Jsont.t
end

module Screen_orientation : sig
  type natural = [ `Landscape | `Portrait ]
  type type' =
  [ `Landscape_primary
  | `Landscape_secondary
  | `Portrait_primary
  | `Portrait_secondary ]
  type t
  val make : natural:natural -> type':type' -> unit -> t
  val jsont : t Jsont.t
end

module Screen_area : sig
  type t
  val make : width:int -> height:int -> t
  val width : t -> int
  val height : t -> int
  val jsont : t Jsont.t
end

(** {1:commands Commands} *)

val set_forced_colors_mode_theme_override :
  Connection.t -> ?exts:Exts.t ->
  theme:[ `Dark | `Light ] option ->
  ?contexts:Browsing_context.t list -> ?user_contexts:User_context.t list ->
  unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-emulation-setForcedColorsModeThemeOverride}
    [emulation.setForcedColorsModeThemeOverride]} *)

val set_geolocation_override :
  Connection.t -> ?exts:Exts.t ->
  [ `Coordinates of Geolocation_coordinates.t option
  | `Position_unavailable ] ->
  ?contexts:Browsing_context.t list -> ?user_contexts:User_context.t list ->
  unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-emulation-setGeolocationOverride}[emulation.setGeolocationOverride]} *)

val set_locale_override :
  Connection.t -> ?exts:Exts.t -> locale:string option ->
  ?contexts:Browsing_context.t list -> ?user_contexts:User_context.t list ->
  unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-emulation-setLocaleOverride}[emulation.setLocaleOverride]} *)

val set_network_conditions :
  Connection.t -> ?exts:Exts.t -> network_conditions:[`Offline] option ->
  ?contexts:Browsing_context.t list -> ?user_contexts:User_context.t list ->
  unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-emulation-setNetworkConditions}[emulation.setNetworkConditions]} *)

val set_screen_orientation_override :
  Connection.t -> ?exts:Exts.t ->
  screen_orientation:Screen_orientation.t option ->
  ?contexts:Browsing_context.t list -> ?user_contexts:User_context.t list ->
  unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-emulation-setScreenOrientationOverride}
    [emulation.setScreenOrientationOverride]} *)

val set_screen_settings_override :
  Connection.t -> ?exts:Exts.t -> screen_area:Screen_area.t option ->
  ?contexts:Browsing_context.t list -> ?user_contexts:User_context.t list ->
  unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-emulation-setScreenSettingsOverride}[emulation.setScreenSettingsOverride]} *)

val set_scripting_enabled :
  Connection.t -> ?exts:Exts.t -> enabled:bool option ->
  ?contexts:Browsing_context.t list -> ?user_contexts:User_context.t list ->
   unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-emulation-setScriptingEnabled}[emulation.setScriptingEnabled]} *)

val set_scrollbar_type_override :
  Connection.t -> ?exts:Exts.t ->
  scrollbar_type:[ `Classic | `Overlay ] option ->
  ?contexts:Browsing_context.t list -> ?user_contexts:User_context.t list ->
  unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-emulation-setScrollbarTypeOverride}[emulation.setScrollbarTypeOverride]} *)

val set_timezone_override :
  Connection.t -> ?exts:Exts.t -> timezone:string option ->
  ?contexts:Browsing_context.t list -> ?user_contexts:User_context.t list ->
  unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-emulation-setTimezoneOverride}[emulation.setTimezoneOverride]} *)

val set_touch_override :
  Connection.t -> ?exts:Exts.t -> max_touch_points:int option ->
  ?contexts:Browsing_context.t list -> ?user_contexts:User_context.t list ->
  unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-emulation-setTouchOverride}
    [emulation.setTouchOverride]} *)

val set_user_agent_override :
  Connection.t -> ?exts:Exts.t -> user_agent:string option ->
  ?contexts:Browsing_context.t list -> ?user_contexts:User_context.t list ->
  unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-emulation-setUserAgentOverride}[emulation.setUserAgentOverride]} *)
