(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs_wd__types
open Webs_wd__protocol

let name = "emulation"

(* Types *)

module Geolocation_coordinates = struct
  type t =
    { latitude : float;
      longitude : float;
      accuracy : float option;
      altitude : float option;
      altitude_accuracy : float option;
      heading : float option;
      speed : float option }

  let make
      ?speed ?heading ?altitude_accuracy ?altitude ?accuracy ~latitude
      ~longitude () =
    { latitude; longitude; accuracy; altitude; altitude_accuracy;
      heading; speed }

  let jsont =
    let make
        latitude longitude accuracy altitude altitude_accuracy heading speed
      =
      { latitude; longitude; accuracy; altitude; altitude_accuracy; heading;
        speed; }
    in
    let longitude c = c.longitude and latitude c = c.latitude
    and accuracy c = c.accuracy and altitude c = c.altitude
    and altitude_accuracy c = c.altitude_accuracy and heading c = c.heading
    and speed c = c.speed in
    Jsont.Object.map ~kind:"emulation.GeolocationCoordinates" make
    |> Jsont.Object.mem "longitude" Jsont.number ~enc:longitude
    |> Jsont.Object.mem "latitude" Jsont.number ~enc:latitude
    |> Jsont.Object.opt_mem "accuracy" Jsont.number ~enc:accuracy
    |> Jsont.Object.opt_mem "altitude" Jsont.number ~enc:altitude
    |> Jsont.Object.opt_mem
      "altitude_accuracy" Jsont.number ~enc:altitude_accuracy
    |> Jsont.Object.opt_mem "heading" Jsont.number ~enc:heading
    |> Jsont.Object.opt_mem "speed" Jsont.number ~enc:speed
    |> Jsont.Object.finish
end

module Screen_orientation = struct
  type natural = [`Portrait | `Landscape ]
  type type' =
    [ `Portrait_primary | `Portrait_secondary | `Landscape_primary
    | `Landscape_secondary ]

  type t = { natural : string; type' : string }
  let make ~natural ~type' () =
    let natural = match natural with
    | `Portrait -> "portrait" | `Landscape -> "landscape"
    in
    let type' = match type' with
    | `Portrait_primary -> "portrait-primary"
    | `Portrait_secondary -> "portrait-secondary"
    | `Landscape_primary -> "landscape-primary"
    | `Landscape_secondary -> "landscape-secondary"
    in
    { natural; type' }

  let jsont =
    let make natural type' = { natural; type' } in
    let natural s = s.natural and type' s = s.type' in
    Jsont.Object.map ~kind:"ScreenOrientation" make
    |> Jsont.Object.mem "natural" Jsont.string ~enc:natural
    |> Jsont.Object.mem "type" Jsont.string ~enc:type'
    |> Jsont.Object.finish
end

module Screen_area = struct
  type t = { width : int; height : int }
  let make ~width ~height = { width; height }
  let width s = s.width
  let height s = s.height
  let jsont =
    let make width height = { width; height } in
    Jsont.Object.map ~kind:"ScreenArea" make
    |> Jsont.Object.mem "width" Js_uint.jsont ~enc:width
    |> Jsont.Object.mem "height" Js_uint.jsont ~enc:height
    |> Jsont.Object.finish
end

(* Most override have a single additional field and all other fields in
   commmon. This abstracts that. *)

type 'a override =
  { override : 'a; (* override specific field *)
    contexts : Browsing_context.t list option;
    user_contexts : User_context.t list option }

let override_jsont ~kind ~mem:name jsont =
  let make override contexts user_contexts =
    { override; contexts; user_contexts}
  in
  let contexts o = o.contexts and user_contexts o = o.user_contexts in
  let override o = o.override in
  Jsont.Object.map ~kind make
  |> Jsont.Object.mem name jsont ~enc:override
  |> Jsont.Object.opt_mem
    "contexts" (Jsont.list Browsing_context.jsont) ~enc:contexts
  |> Jsont.Object.opt_mem
    "userContexts" (Jsont.list User_context.jsont) ~enc:user_contexts
  |> Jsont.Object.finish

(* emulation.setForcedColorsModeThemeOverride *)

let set_forced_colors_mode_theme_override_params_jsont = override_jsont
    ~kind:"emulation.SetForcedColorsModeThemeOverrideParameters"
    ~mem:"theme"
    (Jsont.option Jsont.string)

let set_forced_colors_mode_theme_override_command =
  let params_jsont = set_forced_colors_mode_theme_override_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "emulation.setForcedColorsModeThemeOverride"
    ~params_jsont ~result_jsont

let set_forced_colors_mode_theme_override
    c ?exts ~theme ?contexts ?user_contexts () =
  let override = match theme with
  | None -> None | Some `Light -> Some "light" | Some `Dark -> Some "dark"
  in
  let params = { override; contexts; user_contexts } in
  call c ?exts set_forced_colors_mode_theme_override_command params

(* emulation.setGeolocationOverride *)

type geolocation_error = { type' : string }
let geolocation_error_jsont =
  let make type' = { type' } in
  let type' p = p.type' in
  Jsont.Object.map ~kind:"emulation.GeolocationError" make
  |> Jsont.Object.mem "type" Jsont.string ~enc:type'
  |> Jsont.Object.finish

type set_geolocation_override_params =
  { coordinates : Geolocation_coordinates.t option option;
    error : geolocation_error option;
    contexts : Browsing_context.t list option;
    user_contexts : User_context.t list option; }

let set_geolocation_override_params_jsont =
  let make coordinates error contexts user_contexts =
    { coordinates; error; contexts; user_contexts }
  in
  let coordinates p = p.coordinates and error p = p.error
  and contexts p = p.contexts and user_contexts p = p.user_contexts in
  Jsont.Object.map ~kind:"emulation.SetGeolocationOverrideParameters"
    make
  |> Jsont.Object.opt_mem
    "coordinates" (Jsont.option Geolocation_coordinates.jsont) ~enc:coordinates
  |> Jsont.Object.opt_mem
    "error" geolocation_error_jsont ~enc:error
  |> Jsont.Object.opt_mem
    "contexts" (Jsont.list Browsing_context.jsont) ~enc:contexts
  |> Jsont.Object.opt_mem
    "userContexts" (Jsont.list User_context.jsont) ~enc:user_contexts
  |> Jsont.Object.finish

let set_geolocation_override_command =
  let params_jsont = set_geolocation_override_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "browser.setGeolocationOverride" ~params_jsont ~result_jsont

let set_geolocation_override c ?exts status ?contexts ?user_contexts () =
  let coordinates, error = match status with
  | `Coordinates coords -> Some coords, None
  | `Position_unavailable -> None, Some { type' = "positionUnavailable" }
  in
  let params = { coordinates; error; contexts; user_contexts } in
  call c ?exts set_geolocation_override_command params

(* emulation.setLocaleOverride *)

let set_locale_override_params_jsont = override_jsont
    ~kind:"emulation.SetLocaleOverrideParameters"
    ~mem:"locale"
    (Jsont.option Jsont.string)

let set_locale_override_command =
  let params_jsont = set_locale_override_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "emulation.setLocaleOverride" ~params_jsont ~result_jsont

let set_locale_override c ?exts ~locale:override ?contexts ?user_contexts () =
  let params = { override; contexts; user_contexts } in
  call c ?exts set_locale_override_command params

(* emulation.setNetworkConditions *)

type network_conditions = [`Offline]
let network_conditions_jsont =
  let make _type = `Offline in
  Jsont.Object.map ~kind:"NetworkConditionsOffline" make
  |> Jsont.Object.mem "type" Jsont.string ~enc:(fun _ -> "offline")
  |> Jsont.Object.finish

let set_network_conditions_params_jsont = override_jsont
    ~kind:"emulation.SetNetworkConditionParameters"
    ~mem:"networkConditions"
    (Jsont.option network_conditions_jsont)

let set_network_conditions_command =
  let params_jsont = set_network_conditions_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "emulation.setNetworkConditions" ~params_jsont ~result_jsont

let set_network_conditions
    c ?exts ~network_conditions:override ?contexts ?user_contexts ()
  =
  let params = { override; contexts; user_contexts } in
  call c ?exts set_network_conditions_command params

(* emulation.setScreenOrientationOverride *)

let set_screen_orientation_override_params_jsont = override_jsont
    ~kind:"emulation.SetScreenOrientationOverrideParameters"
    ~mem:"screenOrientation"
    (Jsont.option Screen_orientation.jsont)

let set_screen_orientation_override_command =
  let params_jsont = set_screen_orientation_override_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "emulation.setScreenOrientationOverride"
    ~params_jsont ~result_jsont

let set_screen_orientation_override
    c ?exts ~screen_orientation:override ?contexts ?user_contexts ()
  =
  let params = { override; contexts; user_contexts } in
  call c ?exts set_screen_orientation_override_command params

(* emulation.setScreenSettingsOverride *)

let set_screen_settings_override_params_jsont = override_jsont
    ~kind:"emulation.SetScreenSettingsOverrideParameters"
    ~mem:"screenArea"
    (Jsont.option Screen_area.jsont)

let set_screen_settings_override_command =
  let params_jsont = set_screen_settings_override_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "emulation.setScreenSettingsOverride"
    ~params_jsont ~result_jsont

let set_screen_settings_override
    c ?exts ~screen_area:override ?contexts ?user_contexts ()
  =
  let params = { override; contexts; user_contexts } in
  call c ?exts set_screen_settings_override_command params

(* emulation.setScriptingEnabled *)

let set_scripting_enabled_params_jsont = override_jsont
  ~kind:"emulation.SetScriptingEnabledParameters"
  ~mem:"enabled"
  (Jsont.option Jsont.bool)

let set_scripting_enabled_command =
  let params_jsont = set_scripting_enabled_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "emulation.setScriptingEnabled" ~params_jsont ~result_jsont

let set_scripting_enabled
    c ?exts ~enabled:override ?contexts ?user_contexts ()
  =
  let params = { override; contexts; user_contexts } in
  call c ?exts set_scripting_enabled_command params

(* emulation.setScrollbarTypeOverride *)

let set_scrollbar_type_override_params_jsont = override_jsont
    ~kind:"emulation.SetScrollbarTypeOverrideParameters"
    ~mem:"ScrollbarType"
    (Jsont.option Jsont.string)

let set_scrollbar_type_override_command =
  let params_jsont = set_scrollbar_type_override_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "emulation.setScrollbarTypeOverride"
    ~params_jsont ~result_jsont

let set_scrollbar_type_override
    c ?exts ~scrollbar_type ?contexts ?user_contexts ()
  =
  let override = match scrollbar_type with
  | Some `Classic -> Some "classic" | Some `Overlay -> Some "overlay"
  | None -> None
  in
  let params = { override; contexts; user_contexts } in
  call c ?exts set_scrollbar_type_override_command params

(* emulation.setTimezoneOverride *)

let set_timezone_override_params_jsont = override_jsont
    ~kind:"emulation.SetTimezoneOverrideParameters"
    ~mem:"timezone"
    (Jsont.option Jsont.string)

let set_timezone_override_command =
  let params_jsont = set_timezone_override_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "emulation.setTimezoneOverride" ~params_jsont ~result_jsont

let set_timezone_override
    c ?exts ~timezone:override ?contexts ?user_contexts ()
  =
  let params = { override; contexts; user_contexts } in
  call c ?exts set_timezone_override_command params

(* emulation.setTouchOverride *)

let set_touch_override_params_jsont = override_jsont
    ~kind:"emulation.SetTouchOverrideParameters"
    ~mem:"maxTouchPoints"
    (Jsont.option Js_uint.jsont)

let set_touch_override_command =
  let params_jsont = set_touch_override_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "emulation.setTouchOverride" ~params_jsont ~result_jsont

let set_touch_override
    c ?exts ~max_touch_points:override ?contexts ?user_contexts ()
  =
  let params = { override; contexts; user_contexts } in
  call c ?exts set_touch_override_command params

(* emulation.setUserAgentOverride *)

let set_user_agent_override_params_jsont = override_jsont
    ~kind:"emulation.SetUserAgentOverrideParameters"
    ~mem:"userAgent"
    (Jsont.option Jsont.string)

let set_user_agent_override_command =
  let params_jsont = set_user_agent_override_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "emulation.setUserAgentOverride" ~params_jsont ~result_jsont

let set_user_agent_override
    c ?exts ~user_agent:override ?contexts ?user_contexts ()
  =
  let params = { override; contexts; user_contexts } in
  call c ?exts set_user_agent_override_command params
