(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs_wd__types
open Webs_wd__protocol

let name = "browsingContext"

(* Types *)

module Navigation = struct
  type t = string
  let jsont = Jsont.with_doc ~kind:"browsingContext.Navigation" Jsont.string
end

module Base_navigation_info = struct
  type 'a t =
    { context : Browsing_context.t;
      navigation : Navigation.t option;
      timestamp : int;
      url : string;
      user_context : User_context.t option;
      other : 'a; }

  let context i = i.context
  let navigation i = i.navigation
  let timestamp i = i.timestamp
  let url i = i.url
  let user_context i = i.user_context
  let jsont_open ~kind make =
    Jsont.Object.map ~kind make
    |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:context
    |> Jsont.Object.mem "navigation"
      (Jsont.option Navigation.jsont) ~enc:navigation
    |> Jsont.Object.mem "timestamp" Js_uint.jsont ~enc:timestamp
    |> Jsont.Object.mem "url" Jsont.string ~enc:url
    |> Jsont.Object.opt_mem
      "userContext" User_context.jsont ~enc:user_context
end

module Box_clip_rectangle = struct
  type t = { x : float; y : float; width : float; height : float }
  let make ~x ~y ~width ~height () = { x; y; width; height }
  let x r = r.x
  let y r = r.y
  let width r = r.width
  let height r = r.height
  let jsont =
    let make x y width height = { x; y; width; height } in
    Jsont.Object.map ~kind:"browsingContext.BoxClipRectangle" make
    |> Jsont.Object.mem "x" Jsont.number ~enc:x
    |> Jsont.Object.mem "y" Jsont.number ~enc:y
    |> Jsont.Object.mem "width" Jsont.number ~enc:width
    |> Jsont.Object.mem "height" Jsont.number ~enc:height
    |> Jsont.Object.finish
end

module Capture_screenshot_result = struct
  type t = { data : string }
  let data r = r.data
  let jsont =
    let make data = { data } in
    Jsont.Object.map ~kind:"browsingContext.CaptureScreenshotResult" make
    |> Jsont.Object.mem "data" Jsont.string ~enc:data
    |> Jsont.Object.finish
end

module Clip_rectangle = struct
  type t =
  [ `Element_clip of Webs_wd__script.Shared_reference.t
  | `Box_clip of Box_clip_rectangle.t ]

  let element_clip_rectangle_jsont =
    Jsont.Object.map ~kind:"browsingContext.ElementClipRectangle" Fun.id
    |> Jsont.Object.mem
      "element" Webs_wd__script.Shared_reference.jsont ~enc:Fun.id
    |> Jsont.Object.finish

  let jsont =
    let element_clip =
      let dec c = `Element_clip c in
      Jsont.Object.Case.map "element" element_clip_rectangle_jsont ~dec
    in
    let box_clip =
      let dec c = `Box_clip c in
      Jsont.Object.Case.map "box" Box_clip_rectangle.jsont ~dec
    in
    let enc_case = function
    | `Element_clip c -> Jsont.Object.Case.value element_clip c
    | `Box_clip b -> Jsont.Object.Case.value box_clip b
    in
    let cases = Jsont.Object.Case.[make element_clip; make box_clip] in
    Jsont.Object.map ~kind:"browsingContext.ClipRectangle" Fun.id
    |> Jsont.Object.case_mem
      "type" Jsont.string cases ~enc:Fun.id ~enc_case ~tag_to_string:Fun.id
    |> Jsont.Object.finish
end

module Download_will_begin_params = struct
  include Base_navigation_info
  type other = { suggested_filename : string }
  type nonrec t = other t
  let suggested_filename n = n.other.suggested_filename
  let jsont =
    let make context navigation timestamp url user_context suggested_filename =
      let other = { suggested_filename } in
      { context; navigation; timestamp; url; user_context; other }
    in
    jsont_open ~kind:"browsingContext.NavigationInfo" make
    |> Jsont.Object.mem "suggestedFilename" Jsont.string ~enc:suggested_filename
    |> Jsont.Object.finish
end

module Download_end_params = struct
  include Base_navigation_info
  type status = [ `Canceled | `Complete of string option ]
  type nonrec t = status t
  let status n = n.other
  let status_enc n = match n.other with
  | `Canceled -> "canceled" | `Complete _ -> "complete"

  let filepath_enc n = match n.other with
  | `Canceled -> None | `Complete filepath -> Some filepath

  let jsont =
    let make context navigation timestamp url user_context status filepath : t =
      let status = match status with
      | "canceled" -> `Canceled
      | "complete" -> `Complete (Option.join filepath)
      | status ->
          Jsont.Error.msgf Jsont.Meta.none "unknown value %s in member status"
            status
      in
      { context; navigation; timestamp; url; user_context; other = status }
    in
    jsont_open ~kind:"browsingContext.DownloadEndParams" make
    |> Jsont.Object.mem "status" Jsont.string ~enc:status_enc
    |> Jsont.Object.opt_mem "filepath" Jsont.(option string) ~enc:filepath_enc
    |> Jsont.Object.finish
end

module Create_result = struct
  type t =
    { context : Browsing_context.t;
      user_context : User_context.t option }
  let context r = r.context
  let user_context r = r.user_context
  let jsont =
    let make context user_context = { context; user_context } in
    Jsont.Object.map ~kind:"browsingContext.CreateResult" make
    |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:context
    |> Jsont.Object.opt_mem "userContext" User_context.jsont ~enc:user_context
    |> Jsont.Object.finish
end

module History_updated_params = struct
  type t =
    { context : Browsing_context.t;
      timestamp : int;
      url : string;
      user_context : User_context.t option }

  let context h = h.context
  let timestamp h = h.timestamp
  let url h = h.url
  let user_context h = h.user_context
  let jsont =
    let make context timestamp url user_context =
      { context; timestamp; url; user_context }
    in
    Jsont.Object.map ~kind:"HistoryUpdatedParameters" make
    |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:context
    |> Jsont.Object.mem "timestamp" Js_uint.jsont ~enc:timestamp
    |> Jsont.Object.mem "url" Jsont.string ~enc:url
    |> Jsont.Object.opt_mem "userContext" User_context.jsont ~enc:user_context
    |> Jsont.Object.finish
end

module Image_format = struct
  type t = { type' : string; quality : float option }
  let make ?quality ~type' () = { type'; quality }
  let type' f = f.type'
  let quality f = f.quality
  let jsont =
    let make type' quality = { type'; quality } in
    Jsont.Object.map ~kind:"browsingContext.ImageFormat" make
    |> Jsont.Object.mem "type" Jsont.string ~enc:type'
    |> Jsont.Object.mem "quality" (Jsont.option Jsont.number) ~enc:quality
    |> Jsont.Object.finish
end

module Info = struct
  type t =
    { children : t list option;
      client_window : Webs_wd__browser.Client_window.t;
      context : Browsing_context.t;
      original_opener : Browsing_context.t option;
      url : string;
      user_context : User_context.t;
      parent : Browsing_context.t option }
  let children i = i.children
  let client_window i = i.client_window
  let context i = i.context
  let original_opener i = i.original_opener
  let url i = i.url
  let user_context i = i.user_context
  let parent i = i.parent
  let jsont =
    let rec t =
      lazy begin
        let make
            children client_window context original_opener url user_context
            parent
          =
          { children; client_window; context; original_opener; url;
            user_context; parent = Option.join parent }
        in
        Jsont.Object.map ~kind:"browsingContext.Info" make
        |> Jsont.Object.mem
          "children" (Jsont.option (Jsont.list (Jsont.rec' t))) ~enc:children
        |> Jsont.Object.mem
          "clientWindow" Webs_wd__browser.Client_window.jsont
          ~enc:client_window
        |> Jsont.Object.mem
          "context" Browsing_context.jsont ~enc:context
        |> Jsont.Object.mem
          "originalOpener" (Jsont.option Browsing_context.jsont)
          ~enc:original_opener
        |> Jsont.Object.mem
          "url" Jsont.string ~enc:url
        |> Jsont.Object.mem
          "userContext" User_context.jsont ~enc:user_context
        |> Jsont.Object.opt_mem
          "parent" (Jsont.option Browsing_context.jsont)
          ~enc:(fun i -> Some i.parent)
        |> Jsont.Object.finish
      end
    in
    Lazy.force t
end

module Get_tree_result = struct
  type t = { contexts : Info.t list }
  let contexts r = r.contexts
  let jsont =
    let make contexts = { contexts } in
    Jsont.Object.map ~kind:"browsingContext.GetTreeParameters" make
    |> Jsont.Object.mem "contexts" (Jsont.list Info.jsont) ~enc:contexts
    |> Jsont.Object.finish
end

module Locator = struct
  module Accessibility = struct
    type t = { name : string option; role : string option }
    let make ?name ?role () = { name; role }
    let name a = a.name
    let role a = a.role
    let jsont =
      let value_jsont =
        Jsont.Object.map (fun name role -> { name; role })
        |> Jsont.Object.opt_mem "name" Jsont.string ~enc:name
        |> Jsont.Object.opt_mem "role" Jsont.string ~enc:role
        |> Jsont.Object.finish
      in
      Jsont.Object.map ~kind:"browsingContext.AccessibilityLocator" Fun.id
      |> Jsont.Object.mem "value" value_jsont ~enc:Fun.id
      |> Jsont.Object.finish
  end

  module Css = struct
    type t = string
    let jsont =
      Jsont.Object.map ~kind:"browsingContext.CssLocator" Fun.id
      |> Jsont.Object.mem "value" Jsont.string ~enc:Fun.id
      |> Jsont.Object.finish
  end

  module Context = struct
    type t = Browsing_context.t
    let jsont =
      let value_jsont =
        Jsont.Object.map Fun.id
        |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:Fun.id
        |> Jsont.Object.finish
      in
      Jsont.Object.map ~kind:"browsingContext.ContextLocator" Fun.id
      |> Jsont.Object.mem "value" value_jsont ~enc:Fun.id
      |> Jsont.Object.finish
  end

  module Inner_text = struct
    type match_type = Full | Partial
    let match_type_jsont = Jsont.enum ["full", Full; "partial", Partial]

    type t =
      { value : string;
        ignore_case : bool option;
        match_type : match_type option;
        max_depth : int option }

    let make ~value ?ignore_case ?match_type ?max_depth () =
      { value; ignore_case; match_type; max_depth }
    let value l = l.value
    let ignore_case l = l.ignore_case
    let match_type l = l.match_type
    let max_depth l = l.max_depth
    let jsont =
      let make value ignore_case match_type max_depth =
        { value; ignore_case; match_type; max_depth }
      in
      Jsont.Object.map ~kind:"browsingContext.InnerTextLocator" make
      |> Jsont.Object.mem "value" Jsont.string ~enc:value
      |> Jsont.Object.opt_mem "ignoreCase" Jsont.bool ~enc:ignore_case
      |> Jsont.Object.opt_mem "matchType" match_type_jsont ~enc:match_type
      |> Jsont.Object.opt_mem "maxDepth" Js_uint.jsont ~enc:max_depth
      |> Jsont.Object.finish
  end

  module Xpath = struct
    type t = string
    let jsont =
      Jsont.Object.map ~kind:"browsingContext.XPathLocator" Fun.id
      |> Jsont.Object.mem "value" Jsont.string ~enc:Fun.id
      |> Jsont.Object.finish
  end

  type t =
  [ `Accessibility of Accessibility.t
  | `Css of Css.t
  | `Context of Context.t
  | `Inner_text of Inner_text.t
  | `Xpath of Xpath.t ]

  let jsont =
    let accessibility =
      Jsont.Object.Case.map "accessibility" Accessibility.jsont
        ~dec:(fun l -> `Accessibility l)
    in
    let css =
      Jsont.Object.Case.map "css" Css.jsont ~dec:(fun l -> `Css l)
    in
    let context =
      Jsont.Object.Case.map "context" Context.jsont ~dec:(fun l -> `Context l)
    in
    let inner_text =
        Jsont.Object.Case.map "innerText" Inner_text.jsont
          ~dec:(fun l -> `Inner_text l)
    in
    let xpath =
      Jsont.Object.Case.map "xpath" Xpath.jsont
        ~dec:(fun l -> `Xpath l)
    in
    let enc_case = function
    | `Accessibility l -> Jsont.Object.Case.value accessibility l
    | `Css l -> Jsont.Object.Case.value css l
    | `Context l -> Jsont.Object.Case.value context l
    | `Inner_text l -> Jsont.Object.Case.value inner_text l
    | `Xpath l -> Jsont.Object.Case.value xpath l
    in
    let cases =
      Jsont.Object.Case.[make accessibility; make css; make context;
                         make inner_text; make xpath]
    in
    Jsont.Object.map ~kind:"browsingContext.Locator" Fun.id
    |> Jsont.Object.case_mem
      "type" Jsont.string cases ~enc:Fun.id ~enc_case ~tag_to_string:Fun.id
    |> Jsont.Object.finish
end

module Locate_nodes_result = struct
  type t = { nodes : Webs_wd__script.Node_remote_value.t list }
  let nodes r = r.nodes
  let jsont =
    let make nodes = { nodes } in
    Jsont.Object.map ~kind:"browsingContext.LocateNodesResult" make
    |> Jsont.Object.mem "nodes"
      (Jsont.list Webs_wd__script.Node_remote_value.jsont_standalone) ~enc:nodes
    |> Jsont.Object.finish
end

module Navigate_result = struct
  type t = { navigation : Navigation.t option; url : string; }
  let navigation r = r.navigation
  let url r = r.url
  let jsont =
    let make navigation url = { navigation; url } in
    Jsont.Object.map ~kind:"browsingContext.NavigateResult" make
    |> Jsont.Object.mem
      "navigation" (Jsont.option Navigation.jsont) ~enc:navigation
    |> Jsont.Object.mem "url" Jsont.string ~enc:url
    |> Jsont.Object.finish
end

module Navigation_info = struct
  include Base_navigation_info
  type nonrec t = unit t
  let jsont =
    let make context navigation timestamp url user_context =
      { context; navigation; timestamp; url; user_context; other = () }
    in
    jsont_open ~kind:"browsingContext.NavigationInfo" make
    |> Jsont.Object.finish
end

module Print_result = struct
  type t = { data : string }
  let data r = r.data
  let jsont =
    let make data = { data } in
    Jsont.Object.map ~kind:"browsingContext.PrintResult" make
    |> Jsont.Object.mem "data" Jsont.string ~enc:data
    |> Jsont.Object.finish
end

module Print_margin = struct
  type t =
    { bottom : float option;
      left : float option;
      right : float option;
      top : float option; }
  let make ?bottom ?left ?right ?top () = { bottom; left; right; top }
  let bottom m = m.bottom
  let left m = m.left
  let right m = m.right
  let top m = m.top
  let jsont =
    let make bottom left right top = { bottom; left; right; top } in
    Jsont.Object.map ~kind:"browsingContext.PrintMarginParameters" make
    |> Jsont.Object.opt_mem "bottom" Jsont.number ~enc:bottom
    |> Jsont.Object.opt_mem "left" Jsont.number ~enc:left
    |> Jsont.Object.opt_mem "right" Jsont.number ~enc:right
    |> Jsont.Object.opt_mem "top" Jsont.number ~enc:top
    |> Jsont.Object.finish
end

module Print_orientation = struct
  type t = [ `Landscape | `Portrait ]
end

module Print_page = struct
  type t =
    { width : float option;
      height : float option; }
  let make ?width ?height () = { height; width }
  let width p = p.width
  let height p = p.height
  let a4 = make ~width:21.0 ~height:29.7 ()
  let jsont =
    let make width height = { width; height } in
    Jsont.Object.map ~kind:"browsingContext.PrintPageParameters" make
    |> Jsont.Object.opt_mem "width" Jsont.number ~enc:width
    |> Jsont.Object.opt_mem "height" Jsont.number ~enc:height
    |> Jsont.Object.finish
end

module Readiness_state = struct
  type t = [`None | `Interactive | `Complete]
  let jsont =
    Jsont.enum ~kind:"browsingContext.ReadinessState" @@
    ["none", `None; "interactive", `Interactive; "complete", `Complete]
end

module User_prompt_type = struct
  type t = [`Alert | `Beforeunload | `Confirm | `Prompt]
  let jsont =
    Jsont.enum ~kind:"browsingContext.UserPromptType" @@
    ["alert", `Alert; "beforeunload", `Beforeunload; "confirm", `Confirm;
     "prompt", `Prompt; ]
end

module User_prompt_closed_params = struct
  type t =
    { context : Browsing_context.t;
      accepted : bool;
      type' : User_prompt_type.t;
      user_context : User_context.t option;
      user_text : string option }
  let context p = p.context
  let accepted p = p.accepted
  let type' p = p.type'
  let user_context p = p.user_context
  let user_text p = p.user_text
  let jsont =
    let make context accepted type' user_context user_text =
      { context; accepted; type'; user_context; user_text }
    in
    Jsont.Object.map ~kind:"browsingContext.UserPromptClosedParameters" make
    |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:context
    |> Jsont.Object.mem "accepted" Jsont.bool ~enc:accepted
    |> Jsont.Object.mem "type" User_prompt_type.jsont ~enc:type'
    |> Jsont.Object.opt_mem "userContext" User_context.jsont ~enc:user_context
    |> Jsont.Object.opt_mem "userText" Jsont.string ~enc:user_text
    |> Jsont.Object.finish
end

module User_prompt_opened_params = struct
  type t =
    { context : Browsing_context.t;
      handler : Webs_wd__session.User_prompt_handler_type.t;
      message : string;
      type' : User_prompt_type.t;
      user_context : User_context.t option;
      default_value : string option }
  let context p = p.context
  let handler p = p.handler
  let message p = p.message
  let type' p = p.type'
  let user_context p = p.user_context
  let default_value p = p.default_value
  let jsont =
    let make context handler message type' user_context default_value =
      {context; handler; message; type'; user_context; default_value}
    in
    Jsont.Object.map ~kind:"browsingContext.UserPromptOpenedParameters" make
    |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:context
    |> Jsont.Object.mem
      "handler" Webs_wd__session.User_prompt_handler_type.jsont ~enc:handler
    |> Jsont.Object.mem "message" Jsont.string ~enc:message
    |> Jsont.Object.mem "type" User_prompt_type.jsont ~enc:type'
    |> Jsont.Object.opt_mem "userContext" User_context.jsont ~enc:user_context
    |> Jsont.Object.opt_mem "defaultValue" Jsont.string ~enc:default_value
    |> Jsont.Object.finish
end

module Viewport = struct
  type t = { width : int; height : int }
  let make ~width ~height () = { width; height }
  let width p = p.width
  let height p = p.height
  let jsont =
    let make width height = { width; height } in
    Jsont.Object.map ~kind:"browsingContext.Viewport" make
    |> Jsont.Object.mem "width" Js_uint.jsont ~enc:width
    |> Jsont.Object.mem "height" Js_uint.jsont ~enc:height
    |> Jsont.Object.finish
end

(* browsingContext.activate *)

type activate_params = { context : Browsing_context.t }
let activate_params_jsont =
  let make context = { context } in
  let context p = p.context in
  Jsont.Object.map ~kind:"browsingContext.ActivateParameters" make
  |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:context
  |> Jsont.Object.finish

let activate_command =
  let params_jsont = activate_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "browsingContext.activate" ~params_jsont ~result_jsont

let activate c ?exts ~context () = call ?exts c activate_command { context }

(* browsingContext.captureScreenshot *)

type capture_screenshot_params =
  { context : Browsing_context.t;
    origin : string option;
    format : Image_format.t option;
    clip : Clip_rectangle.t option;  }

let capture_screenshot_params_jsont =
  let make context origin format clip = { context; origin; format; clip } in
  let context p = p.context and origin p = p.origin and format p = p.format
  and clip p = p.clip in
  Jsont.Object.map ~kind:"browsingContext.CaptureScreenshotParameters" make
  |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:context
  |> Jsont.Object.opt_mem "origin" Jsont.string ~enc:origin
  |> Jsont.Object.opt_mem "format" Image_format.jsont ~enc:format
  |> Jsont.Object.opt_mem "clip" Clip_rectangle.jsont ~enc:clip
  |> Jsont.Object.finish

let capture_screenshot_command =
  let params_jsont = capture_screenshot_params_jsont in
  let result_jsont = Capture_screenshot_result.jsont in
  Command.define "browsingContext.captureScreenshot" ~params_jsont ~result_jsont

let capture_screenshot c ?exts ~context ?origin ?format ?clip () =
  let origin = match origin with
  | None -> None
  | Some `Viewport -> Some "viewport"
  | Some `Document -> Some "document"
  in
  let params = { context; origin; format; clip } in
  call ?exts c capture_screenshot_command params

(* browsingContext.close *)

type close_params =
  { context : Browsing_context.t;
    prompt_unload : bool option }

let close_params_jsont =
  let make context prompt_unload = { context; prompt_unload } in
  Jsont.Object.map ~kind:"browsingContext.CloseParameters" make
  |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:(fun p -> p.context)
  |> Jsont.Object.opt_mem
    "promptUnload" Jsont.bool ~enc:(fun p -> p.prompt_unload)
  |> Jsont.Object.finish

let close_command =
  let params_jsont = close_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "browsingContext.close" ~params_jsont ~result_jsont

let close c ?exts ~context ?prompt_unload () =
  call ?exts c close_command { context; prompt_unload }

(* browsingContext.create *)

type create_params =
  { type' : string;
    reference_context : Browsing_context.t option;
    background : bool option;
    user_context : User_context.t option }

let create_params_jsont =
  let make type' reference_context background user_context =
    { type'; reference_context; background; user_context }
  in
  let type' p = p.type' and reference_context p = p.reference_context
  and background p = p.background
  and user_context (p : create_params) = p.user_context in
  Jsont.Object.map ~kind:"browsingContext.CreateParameters" make
  |> Jsont.Object.mem
    "type" Jsont.string ~enc:type'
  |> Jsont.Object.opt_mem
    "referenceContext" Browsing_context.jsont ~enc:reference_context
  |> Jsont.Object.opt_mem
    "background" Jsont.bool ~enc:background
  |> Jsont.Object.opt_mem
    "userContext" User_context.jsont ~enc:user_context
  |> Jsont.Object.finish

let create_command =
  let params_jsont = create_params_jsont in
  let result_jsont = Create_result.jsont in
  Command.define "browsingContext.create" ~params_jsont ~result_jsont

let create c ?exts ~type' ?user_context ?background ?reference_context () =
  let type' = match type' with `Tab -> "tab" | `Window -> "window" in
  let params = { type'; reference_context; background; user_context } in
  call ?exts c create_command params

(* browsingContext.getTree *)

type get_tree_params =
  { max_depth : int option;
    root : Browsing_context.t option }

let get_tree_params_jsont =
  let make max_depth root = { max_depth; root } in
  let max_depth p = p.max_depth and root p = p.root in
  Jsont.Object.map ~kind:"browsingContext.GetTreeParameters" make
  |> Jsont.Object.opt_mem "maxDepth" Js_uint.jsont ~enc:max_depth
  |> Jsont.Object.opt_mem "root" Browsing_context.jsont ~enc:root
  |> Jsont.Object.finish

let get_tree_command =
  let params_jsont = get_tree_params_jsont in
  let result_jsont = Get_tree_result.jsont in
  Command.define "browsingContext.getTree" ~params_jsont ~result_jsont

let get_tree c ?exts ?max_depth ?root () =
  call ?exts c get_tree_command { max_depth; root }

(* browsingContext.handleUserPrompt *)

type handle_user_prompt_params =
  { context : Browsing_context.t;
    accept : bool option;
    user_text : string option; }

let handle_user_prompt_params_jsont =
  let make context accept user_text = { context; accept; user_text } in
  let context p = p.context and accept p = p.accept
  and user_text p = p.user_text in
  Jsont.Object.map ~kind:"browsingContext.HandleUserPromptParameters" make
  |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:context
  |> Jsont.Object.opt_mem "accept" Jsont.bool ~enc:accept
  |> Jsont.Object.opt_mem "userText" Jsont.string ~enc:user_text
  |> Jsont.Object.finish

let handle_user_prompt_command =
  let params_jsont = handle_user_prompt_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "browsingContext.handleUserPrompt" ~params_jsont ~result_jsont

let handle_user_prompt c ?exts ~context ?accept ?user_text () =
  call ?exts c handle_user_prompt_command { context; accept; user_text }

(* browsingContext.locateNodes *)

type locate_nodes_params =
  { context : Browsing_context.t;
    locator : Locator.t;
    max_node_count : int option;
    serialization_options : Webs_wd__script.Serialization_options.t option;
    start_nodes : Webs_wd__script.Shared_reference.t list option }

let locate_nodes_params_jsont =
  let make context locator max_node_count serialization_options start_nodes =
    { context; locator; max_node_count; serialization_options; start_nodes }
  in
  let context p = p.context and locator p = p.locator
  and max_node_count p = p.max_node_count
  and serialization_options p = p.serialization_options
  and start_nodes p = p.start_nodes
  in
  Jsont.Object.map ~kind:"browsingContext.LocateNodesParamaters" make
  |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:context
  |> Jsont.Object.mem "locator" Locator.jsont ~enc:locator
  |> Jsont.Object.opt_mem "maxNodeCount" Js_uint.jsont ~enc:max_node_count
  |> Jsont.Object.opt_mem
    "serializationOptions" Webs_wd__script.Serialization_options.jsont
    ~enc:serialization_options
  |> Jsont.Object.opt_mem
    "startNodes"
    (Jsont.list Webs_wd__script.Shared_reference.jsont) ~enc:start_nodes
  |> Jsont.Object.finish


let locate_nodes_command =
  let params_jsont = locate_nodes_params_jsont in
  let result_jsont = Locate_nodes_result.jsont in
  Command.define "browsingContext.locateNodes" ~params_jsont ~result_jsont

let locate_nodes c
    ?exts ~context ~locator ?max_node_count ?serialization_options ?start_nodes
    ()
  =
  let params =
    { context; locator; max_node_count; serialization_options; start_nodes }
  in
  call ?exts c locate_nodes_command params

(* browsingContext.navigate *)

type navigate_params =
  { context : Browsing_context.t;
    url : string;
    wait : Readiness_state.t option }

let navigate_params_jsont =
  let make context url wait = { context; url; wait } in
  let context p = p.context and url (p : navigate_params) = p.url
  and wait p = p.wait in
  Jsont.Object.map ~kind:"browsingContext.NavigateParameters" make
  |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:context
  |> Jsont.Object.mem "url" Jsont.string ~enc:url
  |> Jsont.Object.opt_mem "wait" Readiness_state.jsont ~enc:wait
  |> Jsont.Object.finish

let navigate_command =
  let params_jsont = navigate_params_jsont in
  let result_jsont = Navigate_result.jsont in
  Command.define "browsingContext.navigate" ~params_jsont ~result_jsont

let navigate c ?exts ~context ~url ?wait () =
  call ?exts c navigate_command { context; url; wait }

(* browsingContext.print *)

type print_params =
  { context : Browsing_context.t;
    background : bool option;
    margin : Print_margin.t option;
    orientation : string option;
    page : Print_page.t option;
    page_ranges : string list option;
    scale : float option;
    shrink_to_fit : bool option }

let print_params_jsont =
  let make
      context background margin orientation page page_ranges scale
      shrink_to_fit
    =
    { context; background; margin; orientation; page; page_ranges; scale;
      shrink_to_fit;}
  in
  let context p = p.context and background p = p.background
  and margin p = p.margin and orientation p = p.orientation and page p = p.page
  and page_ranges p = p.page_ranges and scale p = p.scale
  and shrink_to_fit p = p.shrink_to_fit in
  Jsont.Object.map ~kind:"browsingContext.PrintParams" make
  |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:context
  |> Jsont.Object.opt_mem "background" Jsont.bool ~enc:background
  |> Jsont.Object.opt_mem "margin" Print_margin.jsont ~enc:margin
  |> Jsont.Object.opt_mem "orientation" Jsont.string ~enc:orientation
  |> Jsont.Object.opt_mem "page" Print_page.jsont ~enc:page
  |> Jsont.Object.opt_mem "pageRanges" Jsont.(list string) ~enc:page_ranges
  |> Jsont.Object.opt_mem "scale" Jsont.number ~enc:scale
  |> Jsont.Object.opt_mem "shrinkToFit" Jsont.bool ~enc:shrink_to_fit
  |> Jsont.Object.finish

let print_command =
  let params_jsont = print_params_jsont in
  let result_jsont = Print_result.jsont in
  Command.define "browsingContext.print" ~params_jsont ~result_jsont

let print
    c ?exts ~context ?background ?margin ?orientation ?page ?page_ranges ?scale
    ?shrink_to_fit ()
  =
  let orientation = match orientation with
  | None -> None
  | Some `Portrait -> Some "portrait"
  | Some `Landscape -> Some "landscape"
  in
  let page_ranges = match page_ranges with
  | None -> None | Some l ->
      let mk_range (first, last) =
        if last = -1 then Printf.sprintf "%d-" first else
        if first = -1 then Printf.sprintf "-%d" last else
        Printf.sprintf "%d-%d" first last
      in
      Some (List.map mk_range l)
  in
  let params =
    { context; background; margin; orientation; page; page_ranges;
      scale; shrink_to_fit }
  in
  call ?exts c print_command params

(* browsingContext.reload *)

type reload_params =
  { context : Browsing_context.t;
    ignore_cache : bool option;
    wait : Readiness_state.t option }

let reload_params_jsont =
  let make context ignore_cache wait = { context; ignore_cache; wait } in
  let context p = p.context and ignore_cache p = p.ignore_cache
  and wait p = p.wait in
  Jsont.Object.map ~kind:"browsingContext.ReloadParameters" make
  |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:context
  |> Jsont.Object.opt_mem "ignoreCache" Jsont.bool ~enc:ignore_cache
  |> Jsont.Object.opt_mem "wait" Readiness_state.jsont ~enc:wait
  |> Jsont.Object.finish

let reload_command =
  let params_jsont = reload_params_jsont in
  let result_jsont = Navigate_result.jsont in
  Command.define "browsingContext.reload" ~params_jsont ~result_jsont

let reload c ?exts ~context ?ignore_cache ?wait () =
  call ?exts c reload_command { context; ignore_cache; wait }

(* browsingContext.setBypassCSP *)

type set_bypass_csp_params =
  { bypass : bool option;
    contexts : Browsing_context.t list option;
    user_contexts : User_context.t list option; }

let set_bypass_csp_params_jsont =
  let make bypass contexts user_contexts = {bypass; contexts; user_contexts} in
  let bypass p = p.bypass and contexts p = p.contexts
  and user_contexts p = p.user_contexts in
  Jsont.Object.map ~kind:"SetBypassCSPParameters" make
  |> Jsont.Object.mem
    "bypass" (Jsont.option Jsont.bool) ~enc:bypass
  |> Jsont.Object.opt_mem
    "contexts" (Jsont.list Browsing_context.jsont) ~enc:contexts
  |> Jsont.Object.opt_mem
    "userContexts" (Jsont.list User_context.jsont) ~enc:user_contexts
  |> Jsont.Object.finish

let set_bypass_csp_command =
  let params_jsont = set_bypass_csp_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "browsingContext.setBypassCSP" ~params_jsont ~result_jsont

let set_bypass_csp c ?exts ~bypass ?contexts ?user_contexts () =
  let bypass = if bypass then Some true else None in
  call ?exts c set_bypass_csp_command { bypass; contexts; user_contexts }

(* browsingContext.setViewport *)

type set_viewport_params =
  { context : Browsing_context.t option;
    viewport : Viewport.t option option;
    device_pixel_ratio : float option option;
    user_contexts : User_context.t list option; }

let set_viewport_params_jsont =
  let make context viewport device_pixel_ratio user_contexts =
    {context; viewport; device_pixel_ratio; user_contexts}
  in
  let context p = p.context and viewport p = p.viewport
  and device_pixel_ratio p = p.device_pixel_ratio
  and user_contexts p = p.user_contexts in
  Jsont.Object.map ~kind:"SetViewportParameters" make
  |> Jsont.Object.opt_mem "context"  Browsing_context.jsont ~enc:context
  |> Jsont.Object.opt_mem "viewport" (Jsont.option Viewport.jsont) ~enc:viewport
  |> Jsont.Object.opt_mem
    "devicePixelRatio" (Jsont.option Jsont.number) ~enc:device_pixel_ratio
  |> Jsont.Object.opt_mem
    "userContexts" (Jsont.list User_context.jsont) ~enc:user_contexts
  |> Jsont.Object.finish

let set_viewport_command =
  let params_jsont = set_viewport_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "browsingContext.setViewport" ~params_jsont ~result_jsont

let set_viewport
    c ?exts ?context ?viewport ?device_pixel_ratio ?user_contexts ()
  =
  let params = { context; viewport; device_pixel_ratio; user_contexts } in
  call ?exts c set_viewport_command params

(* browsingContext.traverseHistory *)

type traverse_history_params =
  { context : Browsing_context.t;
    delta : int; }

let traverse_history_params_jsont =
  let make context delta = { context; delta } in
  let context p = p.context and delta p = p.delta in
  Jsont.Object.map ~kind:"browsingContext.TraverseHistoryParameters" make
  |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:context
  |> Jsont.Object.mem "delta" Js_int.jsont ~enc:delta
  |> Jsont.Object.finish

let traverse_history_command =
  let params_jsont = traverse_history_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "browsingContext.traverseHistory" ~params_jsont ~result_jsont

let traverse_history c ?exts ~context ~delta () =
  let params = { context; delta } in
  call ?exts c traverse_history_command params

(* Events *)

let context_created =
  let params_jsont = Info.jsont in
  Event.define "browsingContext.contextCreated" ~params_jsont

let context_destroyed =
  let params_jsont = Info.jsont in
  Event.define "browsingContext.contextDestroyed" ~params_jsont

let dom_content_loaded =
  let params_jsont = Navigation_info.jsont in
  Event.define "browsingContext.domContentLoaded" ~params_jsont

let download_will_begin =
  let params_jsont = Download_will_begin_params.jsont in
  Event.define "browsingContext.downloadWillBegin" ~params_jsont

let download_end =
  let params_jsont = Download_end_params.jsont in
  Event.define "browsingContext.downloadEnd" ~params_jsont

let fragment_navigated =
  let params_jsont = Navigation_info.jsont in
  Event.define "browsingContext.fragmentNavigated" ~params_jsont

let history_updated =
  let params_jsont = History_updated_params.jsont in
  Event.define "browsingContext.historyUpdated" ~params_jsont

let load =
  let params_jsont = Navigation_info.jsont in
  Event.define "browsingContext.load" ~params_jsont

let navigation_started =
  let params_jsont = Navigation_info.jsont in
  Event.define "browsingContext.navigationStarted" ~params_jsont

let navigation_aborted =
  let params_jsont = Navigation_info.jsont in
  Event.define "browsingContext.navigationAborted" ~params_jsont

let navigation_committed =
  let params_jsont = Navigation_info.jsont in
  Event.define "browsingContext.navigationCommitted" ~params_jsont

let navigation_failed =
  let params_jsont = Navigation_info.jsont in
  Event.define "browsingContext.navigationFailed" ~params_jsont

let user_prompt_closed =
  let params_jsont = User_prompt_closed_params.jsont in
  Event.define "browsingContext.userPromptClosed" ~params_jsont

let user_prompt_opened =
  let params_jsont = User_prompt_opened_params.jsont in
  Event.define "browsingContext.userPromptOpened" ~params_jsont
