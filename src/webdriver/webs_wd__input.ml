(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs_wd__types
open Webs_wd__protocol

let name = "input"

(* Types *)

module File_dialog_info = struct
  type t =
    { context : Browsing_context.t;
      user_context : User_context.t option;
      element : Webs_wd__script.Shared_reference.t option;
      multiple : bool }
  let context i = i.context
  let user_context i = i.user_context
  let element i = i.element
  let multiple i = i.multiple
  let jsont =
    let make context user_context element multiple =
      { context; user_context; element; multiple }
    in
    Jsont.Object.map ~kind:"input.FileDialogInfo" make
    |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:context
    |> Jsont.Object.opt_mem "userContext" User_context.jsont ~enc:user_context
    |> Jsont.Object.opt_mem
      "element" Webs_wd__script.Shared_reference.jsont ~enc:element
    |> Jsont.Object.mem "multiple" Jsont.bool ~enc:multiple
    |> Jsont.Object.finish
end

module Element_origin = struct
  type t = { element : Webs_wd__script.Shared_reference.t }
  let make ~element () = { element }
  let element e = e.element
  let type_jsont = Jsont.enum ["element", "element"]
  let jsont =
    let make _type element = { element } in
    Jsont.Object.map ~kind:"input.ElementOrigin" make
    |> Jsont.Object.mem "type" type_jsont ~enc:(fun _ -> "element")
    |> Jsont.Object.mem "element" Webs_wd__script.Shared_reference.jsont
    |> Jsont.Object.finish
end

module Origin = struct
  type t = [ `Viewport | `Pointer | `Element of Element_origin.t ]
  let jsont =
    let dec_string = Jsont.enum ["viewport", `Viewport; "pointer", `Pointer] in
    let dec_object =
      let dec e = `Element e in
      let enc = function `Element e -> e | _ -> assert false in
      Jsont.map Element_origin.jsont ~dec ~enc
    in
    let enc = function
    | `Viewport | `Pointer -> dec_string
    | `Element _ -> dec_object
    in
    Jsont.any ~kind:"input.Origin" ~dec_string ~dec_object ~enc ()
end

module Pause_action = struct
  type t = { duration : int option }
  let make ?duration () = { duration }
  let duration a = a.duration
  let jsont =
    let make duration = { duration } in
    Jsont.Object.map ~kind:"input.PauseAction" make
    |> Jsont.Object.opt_mem "duration" Js_uint.jsont ~enc:duration
    |> Jsont.Object.finish

  let jsont_case () =
    Jsont.Object.Case.map "pause" jsont ~dec:(fun a -> `Pause a)
end

module Key_down_action = struct
  type t = { value : string }
  let make ~value () = { value }
  let value a = a.value
  let jsont =
    let make value = { value } in
    Jsont.Object.map ~kind:"input.KeyDownAction" make
    |> Jsont.Object.mem "value" Jsont.string ~enc:value
    |> Jsont.Object.finish

  let jsont_case () =
    Jsont.Object.Case.map "keyDown" jsont ~dec:(fun a -> `Key_down a)
end

module Key_up_action = struct
  type t = { value : string }
  let make ~value () = { value }
  let value a = a.value
  let jsont =
    let make value = { value } in
    Jsont.Object.map ~kind:"input.KeyUpAction" make
    |> Jsont.Object.mem "value" Jsont.string ~enc:value
    |> Jsont.Object.finish

  let jsont_case () =
    Jsont.Object.Case.map "keyUp" jsont ~dec:(fun a -> `Key_up a)
end

module Pointer_up_action = struct
  type t = { button : int }
  let make ~button () = { button }
  let button a = a.button
  let jsont =
    let make button = { button } in
    Jsont.Object.map ~kind:"input.PointerUpAction" make
    |> Jsont.Object.mem "button" Js_uint.jsont ~enc:button
    |> Jsont.Object.finish

  let jsont_case () =
    Jsont.Object.Case.map "pointerUp" jsont ~dec:(fun a -> `Pointer_up a)
end

module Pointer_down_action = struct
  type t =
    { button : int;
      width : int option;
      height : int option;
      pressure : float option;
      tangential_pressure : float option;
      twist : float option;
      altitude_angle : float option;
      azimuth_angle : float option }
  let make
      ?width ?height ?pressure ?tangential_pressure ?twist ?altitude_angle
      ?azimuth_angle ~button ()
    =
    { button; width; height; pressure; tangential_pressure; twist;
      altitude_angle; azimuth_angle; }
  let button a = a.button
  let width a = a.width
  let height a = a.height
  let pressure a = a.pressure
  let tangential_pressure a = a.tangential_pressure
  let twist a = a.twist
  let altitude_angle a = a.altitude_angle
  let azimuth_angle a = a.azimuth_angle
  let jsont =
    let make
        button width height pressure tangential_pressure twist
        altitude_angle azimuth_angle
      =
      { button; width; height; pressure; tangential_pressure; twist;
        altitude_angle; azimuth_angle; }
    in
    Jsont.Object.map ~kind:"input.PointerDownAction" make
    |> Jsont.Object.mem "button" Js_uint.jsont ~enc:button
    |> Jsont.Object.opt_mem "width" Js_uint.jsont ~enc:width
    |> Jsont.Object.opt_mem "height" Js_uint.jsont ~enc:height
    |> Jsont.Object.opt_mem "pressure" Jsont.number ~enc:pressure
    |> Jsont.Object.opt_mem
      "tangentialPressure" Jsont.number ~enc:tangential_pressure
    |> Jsont.Object.opt_mem "twist" Jsont.number ~enc:twist
    |> Jsont.Object.opt_mem "altitudeAngle" Jsont.number ~enc:altitude_angle
    |> Jsont.Object.opt_mem "azimuthAngle" Jsont.number ~enc:azimuth_angle
    |> Jsont.Object.finish

  let jsont_case () =
    Jsont.Object.Case.map "pointerDown" jsont ~dec:(fun a -> `Pointer_down a)
end

module Pointer_move_action = struct
  type t =
    { x : float;
      y : float;
      duration : int option;
      origin : Origin.t option;
      width : int option;
      height : int option;
      pressure : float option;
      tangential_pressure : float option;
      twist : float option;
      altitude_angle : float option;
      azimuth_angle : float option }
  let make
      ?width ?height ?pressure ?tangential_pressure ?twist ?altitude_angle
      ?azimuth_angle ?origin ?duration ~x ~y ()
    =
    { x; y; duration; origin; width; height; pressure; tangential_pressure;
      twist; altitude_angle; azimuth_angle; }
  let x a = a.x
  let y a = a.y
  let duration a = a.duration
  let origin a = a.origin
  let width a = a.width
  let height a = a.height
  let pressure a = a.pressure
  let tangential_pressure a = a.tangential_pressure
  let twist a = a.twist
  let altitude_angle a = a.altitude_angle
  let azimuth_angle a = a.azimuth_angle
  let jsont =
    let make
        x y duration origin width height pressure tangential_pressure twist
        altitude_angle azimuth_angle
      =
      { x; y; duration; origin; width; height; pressure; tangential_pressure;
        twist; altitude_angle; azimuth_angle; }
    in
    Jsont.Object.map ~kind:"input.PointerMoveAction" make
    |> Jsont.Object.mem "x" Jsont.number ~enc:x
    |> Jsont.Object.mem "y" Jsont.number ~enc:y
    |> Jsont.Object.opt_mem "duration" Js_uint.jsont ~enc:duration
    |> Jsont.Object.opt_mem "origin" Origin.jsont ~enc:origin
    |> Jsont.Object.opt_mem "width" Js_uint.jsont ~enc:width
    |> Jsont.Object.opt_mem "height" Js_uint.jsont ~enc:height
    |> Jsont.Object.opt_mem "pressure" Jsont.number ~enc:pressure
    |> Jsont.Object.opt_mem
      "tangentialPressure" Jsont.number ~enc:tangential_pressure
    |> Jsont.Object.opt_mem "twist" Jsont.number ~enc:twist
    |> Jsont.Object.opt_mem "altitudeAngle" Jsont.number ~enc:altitude_angle
    |> Jsont.Object.opt_mem "azimuthAngle" Jsont.number ~enc:azimuth_angle
    |> Jsont.Object.finish

  let jsont_case () =
    Jsont.Object.Case.map "pointerMove" jsont ~dec:(fun a -> `Pointer_move a)
end

module Wheel_scroll_action = struct
  type t =
    { x : int;
      y : int;
      delta_x : int;
      delta_y : int;
      duration : int option;
      origin : Origin.t option }
  let make ?origin ?duration ~x ~y ~delta_x ~delta_y () =
    { x; y; delta_x; delta_y; duration; origin }
  let x a = a.x
  let y a = a.y
  let delta_x a = a.delta_x
  let delta_y a = a.delta_y
  let duration a = a.duration
  let origin a = a.origin
  let jsont =
    let make x y delta_x delta_y duration origin =
      { x; y; delta_x; delta_y; duration; origin }
    in
    Jsont.Object.map ~kind:"input.WheelScrollAction" make
    |> Jsont.Object.mem "x" Js_int.jsont ~enc:x
    |> Jsont.Object.mem "y" Js_int.jsont ~enc:y
    |> Jsont.Object.mem "deltaX" Js_int.jsont ~enc:delta_x
    |> Jsont.Object.mem "deltaY" Js_int.jsont ~enc:delta_y
    |> Jsont.Object.opt_mem "duration" Js_int.jsont ~enc:duration
    |> Jsont.Object.opt_mem "origin" Origin.jsont ~enc:origin
    |> Jsont.Object.finish

  let jsont_case () =
    Jsont.Object.Case.map "scroll" jsont ~dec:(fun a -> `Wheel_scroll a)
end

module None_source_action = struct
  type t = [ `Pause of Pause_action.t ]
  let jsont =
    let pause = Pause_action.jsont_case () in
    let enc_case = function
    | `Pause p -> Jsont.Object.Case.value pause p
    in
    let cases = Jsont.Object.Case.[make pause] in
    Jsont.Object.map ~kind:"input.NoneSourceAction" Fun.id
    |> Jsont.Object.case_mem "type"
      Jsont.string cases ~enc_case ~enc:Fun.id ~tag_to_string:Fun.id
    |> Jsont.Object.finish
end

module None_source_actions = struct
  type t = { id : string; actions : None_source_action.t list }
  let make ~id ~actions () = { id; actions }
  let id a = a.id
  let actions a = a.actions
  let jsont =
    let make id actions = { id; actions } in
    Jsont.Object.map ~kind:"input.NoneSourceActions" make
    |> Jsont.Object.mem "id" Jsont.string ~enc:id
    |> Jsont.Object.mem
      "actions" (Jsont.list None_source_action.jsont) ~enc:actions
    |> Jsont.Object.finish
end

module Key_source_action = struct
  type t =
  [ `Pause of Pause_action.t
  | `Key_down of Key_down_action.t
  | `Key_up of Key_up_action.t ]

  let jsont =
    let pause = Pause_action.jsont_case () in
    let key_down = Key_down_action.jsont_case () in
    let key_up = Key_up_action.jsont_case () in
    let enc_case = function
    | `Pause a -> Jsont.Object.Case.value pause a
    | `Key_down a -> Jsont.Object.Case.value key_down a
    | `Key_up a -> Jsont.Object.Case.value key_up a
    in
    let cases = Jsont.Object.Case.[make pause; make key_down; make key_up] in
    Jsont.Object.map ~kind:"input.KeyAction" Fun.id
    |> Jsont.Object.case_mem "type"
      Jsont.string cases ~enc_case ~enc:Fun.id ~tag_to_string:Fun.id
    |> Jsont.Object.finish
end

module Key_source_actions = struct
  type t = { id : string; actions : Key_source_action.t list }
  let make ~id ~actions () = { id; actions }
  let id a = a.id
  let actions a = a.actions
  let jsont =
    let make id actions = { id; actions } in
    Jsont.Object.map ~kind:"input.KeySourceActions" make
    |> Jsont.Object.mem "id" Jsont.string ~enc:id
    |> Jsont.Object.mem
      "actions" (Jsont.list Key_source_action.jsont) ~enc:actions
    |> Jsont.Object.finish
end

module Pointer_type = struct
  type t = [ `Mouse | `Pen | `Touch ]
  let jsont =
    Jsont.enum ~kind:"input.PointerType"
      ["mouse", `Mouse; "pen", `Pen; "touch", `Touch]
end

module Pointer_parameters = struct
  type t = { pointer_type : Pointer_type.t option }
  let make ?pointer_type () = { pointer_type }
  let pointer_type p = p.pointer_type
  let jsont =
    let make pointer_type = { pointer_type } in
    Jsont.Object.map ~kind:"input.PointerType" make
    |> Jsont.Object.opt_mem "pointerType" Pointer_type.jsont ~enc:pointer_type
    |> Jsont.Object.finish
end

module Pointer_source_action = struct
  type t =
  [ `Pause of Pause_action.t
  | `Pointer_down of Pointer_down_action.t
  | `Pointer_up of Pointer_up_action.t
  | `Pointer_move of Pointer_move_action.t ]

  let jsont =
    let pause = Pause_action.jsont_case () in
    let pointer_down = Pointer_down_action.jsont_case () in
    let pointer_up = Pointer_up_action.jsont_case () in
    let pointer_move = Pointer_move_action.jsont_case () in
    let enc_case = function
    | `Pause a -> Jsont.Object.Case.value pause a
    | `Pointer_down a -> Jsont.Object.Case.value pointer_down a
    | `Pointer_up a -> Jsont.Object.Case.value pointer_up a
    | `Pointer_move a -> Jsont.Object.Case.value pointer_move a
    in
    let cases = Jsont.Object.Case.[
        make pause; make pointer_down; make pointer_up; make pointer_move]
    in
    Jsont.Object.map ~kind:"input.PointerAction" Fun.id
    |> Jsont.Object.case_mem "type"
      Jsont.string cases ~enc_case ~enc:Fun.id ~tag_to_string:Fun.id
    |> Jsont.Object.finish
end

module Pointer_source_actions = struct
  type t =
    { id : string;
      parameters : Pointer_parameters.t option;
      actions : Pointer_source_action.t list }
  let make ?parameters ~id ~actions () = { id; parameters; actions }
  let id a = a.id
  let parameters a = a.parameters
  let actions a = a.actions
  let jsont =
    let make id parameters actions = { id; parameters; actions } in
    Jsont.Object.map ~kind:"input.KeySourceActions" make
    |> Jsont.Object.mem "id" Jsont.string ~enc:id
    |> Jsont.Object.opt_mem
      "parameters" Pointer_parameters.jsont ~enc:parameters
    |> Jsont.Object.mem
      "actions" (Jsont.list Pointer_source_action.jsont) ~enc:actions
    |> Jsont.Object.finish
end

module Wheel_source_action = struct
  type t =
  [ `Pause of Pause_action.t
  | `Wheel_scroll of Wheel_scroll_action.t ]

  let jsont =
    let pause = Pause_action.jsont_case () in
    let wheel_scroll = Wheel_scroll_action.jsont_case () in
    let enc_case = function
    | `Pause a -> Jsont.Object.Case.value pause a
    | `Wheel_scroll a -> Jsont.Object.Case.value wheel_scroll a
    in
    let cases = Jsont.Object.Case.[make pause; make wheel_scroll] in
    Jsont.Object.map ~kind:"input.WheelSourceAction" Fun.id
    |> Jsont.Object.case_mem "type"
      Jsont.string cases ~enc_case ~enc:Fun.id ~tag_to_string:Fun.id
    |> Jsont.Object.finish
end

module Wheel_source_actions = struct
  type t = { id : string; actions : Wheel_source_action.t list }
  let make ~id ~actions () = { id; actions }
  let id a = a.id
  let actions a = a.actions
  let jsont =
    let make id actions = { id; actions } in
    Jsont.Object.map ~kind:"input.WheelSourceActions" make
    |> Jsont.Object.mem "id" Jsont.string ~enc:id
    |> Jsont.Object.mem
      "actions" (Jsont.list Wheel_source_action.jsont) ~enc:actions
    |> Jsont.Object.finish
end

module Source_actions = struct
  type t =
  [ `None of None_source_actions.t
  | `Key of Key_source_actions.t
  | `Pointer of Pointer_source_actions.t
  | `Wheel of Wheel_source_actions.t ]
  let jsont =
    let none =
      Jsont.Object.Case.map "none" None_source_actions.jsont
        ~dec:(fun n -> `None n)
    in
    let key =
      Jsont.Object.Case.map "key" Key_source_actions.jsont
        ~dec:(fun n -> `Key n)
    in
    let pointer =
      Jsont.Object.Case.map "pointer" Pointer_source_actions.jsont
        ~dec:(fun n -> `Pointer n)
    in
    let wheel =
      Jsont.Object.Case.map "wheel" Wheel_source_actions.jsont
        ~dec:(fun n -> `Wheel n)
    in
    let enc_case = function
    | `None n -> Jsont.Object.Case.value none n
    | `Key k -> Jsont.Object.Case.value key k
    | `Pointer p -> Jsont.Object.Case.value pointer p
    | `Wheel w  -> Jsont.Object.Case.value wheel w
    in
    let cases =
      Jsont.Object.Case.[make none; make key; make pointer; make wheel]
    in
    Jsont.Object.map ~kind:"input.SourceActions" Fun.id
    |> Jsont.Object.case_mem "type"
      Jsont.string cases ~enc_case ~enc:Fun.id ~tag_to_string:Fun.id
    |> Jsont.Object.finish
end

(* [input.performActions] *)

type perform_actions_params =
  { context : Browsing_context.t;
    actions : Source_actions.t list }

let perform_actions_params_jsont =
  let make context actions = { context; actions } in
  let context p = p.context and actions p = p.actions in
  Jsont.Object.map ~kind:"input.PerformActionsParameters" make
  |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:context
  |> Jsont.Object.mem "actions" (Jsont.list Source_actions.jsont) ~enc:actions
  |> Jsont.Object.finish

let perform_action_command =
  let params_jsont = perform_actions_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "input.performActions" ~params_jsont ~result_jsont

let perform_actions c ?exts ~context ~actions () =
  call ?exts c perform_action_command { context; actions }

(* [input.releaseActions] *)

type release_actions_parmas =
  { context : Browsing_context.t }

let release_actions_params_jsont =
  let make context = { context } in
  let context p = p.context in
  Jsont.Object.map ~kind:"input.ReleaseActionsParameters" make
  |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:context
  |> Jsont.Object.finish

let release_actions_command =
  let params_jsont = release_actions_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "input.releaseActions" ~params_jsont ~result_jsont

let release_actions c ?exts ~context () =
  call ?exts c release_actions_command { context }

(* [input.releaseActions] *)

type set_files_parmas =
  { context : Browsing_context.t;
    element : Webs_wd__script.Shared_reference.t;
    files : string list;  }

let set_files_params_jsont =
  let make context element files = { context; element; files } in
  let context p = p.context and element p = p.element and files p = p.files in
  Jsont.Object.map ~kind:"input.setFilesParameters" make
  |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:context
  |> Jsont.Object.mem
    "element" Webs_wd__script.Shared_reference.jsont ~enc:element
  |> Jsont.Object.mem "files" Jsont.(list string) ~enc:files
  |> Jsont.Object.finish

let release_action_command =
  let params_jsont = set_files_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "input.setFiles" ~params_jsont ~result_jsont

let set_files c ?exts ~context ~element ~files () =
  call ?exts c release_action_command { context; element; files }

(* [input.fileDialogOpened] *)

let file_dialog_opened =
  let params_jsont = File_dialog_info.jsont in
  Event.define "input.fileDialogOpened" ~params_jsont
