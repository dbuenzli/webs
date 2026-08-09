(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs_wd__types
open Webs_wd__protocol

let name = "browser"

(* Types *)

module Client_window = struct
  type t = string
  let jsont = Jsont.with_doc ~kind:"browser.ClientWindow" Jsont.string
end

module Client_window_info = struct
  type state = Fullscreen | Maximized | Minimized | Normal
  let state_jsont =
    Jsont.enum ~kind:"browser.ClientWindowInfoState" (* unnamed in the spec *)
      ["fullscreen", Fullscreen; "maximized", Maximized; "minimized", Minimized;
       "normal", Normal]

  type t =
    { active : bool;
      client_window : Client_window.t;
      height : Js_uint.t;
      state : state;
      width : Js_uint.t;
      x : Js_int.t;
      y : Js_int.t }

  let active i = i.active
  let client_window i = i.client_window
  let height i = i.height
  let state i = i.state
  let width i = i.width
  let x i = i.x
  let y i = i.y
  let jsont =
    let make active client_window height state width x y =
      { active; client_window; height; state; width; x; y }
    in
    Jsont.Object.map ~kind:"browser.ClientWindowInfo" make
    |> Jsont.Object.mem "active" Jsont.bool ~enc:active
    |> Jsont.Object.mem "clientWindow" Client_window.jsont ~enc:client_window
    |> Jsont.Object.mem "height" Js_uint.jsont ~enc:height
    |> Jsont.Object.mem "state" state_jsont ~enc:state
    |> Jsont.Object.mem "width" Js_uint.jsont ~enc:width
    |> Jsont.Object.mem "x" Js_int.jsont ~enc:x
    |> Jsont.Object.mem "y" Js_int.jsont ~enc:y
    |> Jsont.Object.finish
end

module Download_behavior = struct
  type destination_folder = string
  type t = [`Allowed of destination_folder | `Denied ]
end

module Get_client_windows_result = struct
  type t = { client_windows : Client_window_info.t list }
  let client_windows r = r.client_windows
  let jsont =
    let make client_windows = { client_windows } in
    Jsont.Object.map ~kind:"browser.getClientWindowsResult" make
    |> Jsont.Object.mem
      "clientWindows" (Jsont.list Client_window_info.jsont) ~enc:client_windows
    |> Jsont.Object.finish
end

module User_context_info = struct
  type t = { user_context : User_context.t }
  let make ~user_context () = { user_context }
  let user_context u = u.user_context
  let jsont =
    let make user_context = { user_context } in
    Jsont.Object.map ~kind:"browser.UserContextInfo" make
    |> Jsont.Object.mem "userContext" User_context.jsont ~enc:user_context
    |> Jsont.Object.finish
end

module Get_user_contexts_result = struct
  type t = { user_contexts : User_context_info.t list }
  let user_contexts r = r.user_contexts
  let jsont =
    let make user_contexts = { user_contexts } in
    Jsont.Object.map ~kind:"browser.getUserContextsResult" make
    |> Jsont.Object.mem
      "userContexts" (Jsont.list User_context_info.jsont) ~enc:user_contexts
    |> Jsont.Object.finish
end

(* browser.close *)

let close_command =
  let params_jsont = Empty_params.jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "browser.close" ~params_jsont ~result_jsont

let close c ?exts () = call ?exts c close_command Empty_params.empty

(* browser.createUserContext *)

type create_user_context_params =
  { accept_insecure_certs : bool option;
    proxy : Webs_wd__session.Proxy_configuration.t option;
    unhandled_prompt_behavior : Webs_wd__session.User_prompt_handler.t option; }

let create_user_context_params_jsont =
  let make accept_insecure_certs proxy unhandled_prompt_behavior =
    { accept_insecure_certs; proxy; unhandled_prompt_behavior}
  in
  Jsont.Object.map ~kind:"browser.CreateUserContextParameters" make
  |> Jsont.Object.opt_mem
    "acceptInsecureCerts" Jsont.bool ~enc:(fun p -> p.accept_insecure_certs)
  |> Jsont.Object.opt_mem
    "proxy" Webs_wd__session.Proxy_configuration.jsont ~enc:(fun p -> p.proxy)
  |> Jsont.Object.opt_mem
    "unhandledPromptBehavior" Webs_wd__session.User_prompt_handler.jsont
    ~enc:(fun p -> p.unhandled_prompt_behavior)
  |> Jsont.Object.finish

let create_user_context_command =
  let params_jsont = create_user_context_params_jsont in
  let result_jsont = User_context_info.jsont in
  Command.define "browser.createUserContext" ~params_jsont ~result_jsont

let create_user_context
    c ?exts ?accept_insecure_certs ?proxy ?unhandled_prompt_behavior ()
  =
  let params = { accept_insecure_certs; proxy; unhandled_prompt_behavior } in
  call ?exts c create_user_context_command params

(* browser.getClientWindows *)

let get_client_windows_command =
  let params_jsont = Empty_params.jsont in
  let result_jsont = Get_client_windows_result.jsont in
  Command.define "browser.getClientWindows" ~params_jsont ~result_jsont

let get_client_windows c ?exts () =
  call ?exts c get_client_windows_command Empty_params.empty

(* browser.getUserContexts *)

let get_user_contexts_command =
  let params_jsont = Empty_params.jsont in
  let result_jsont = Get_user_contexts_result.jsont in
  Command.define "browser.getUserContexts" ~params_jsont ~result_jsont

let get_user_contexts c ?exts () =
  call ?exts c get_user_contexts_command Empty_params.empty

(* browser.removeUserContext *)

type remove_user_context_params = { user_context : User_context.t }
let remove_user_context_params_jsont =
  let make user_context = { user_context } in
  let user_context p = p.user_context in
  Jsont.Object.map ~kind:"browser.RemoveUserContextParameters" make
  |> Jsont.Object.mem "userContext" User_context.jsont ~enc:user_context
  |> Jsont.Object.finish

let remove_user_context_command =
  let params_jsont = remove_user_context_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "browser.removeUserContext" ~params_jsont ~result_jsont

let remove_user_context c ?exts ~user_context () =
  call ?exts c remove_user_context_command { user_context }

(* browser.setClientWindowState *)

type set_client_window_state_params =
  { client_window : Client_window.t;
    state : Client_window_info.state;
    width : int option;
    height : int option;
    x : int option;
    y : int option }

let set_client_window_state_params_jsont =
  let make client_window state width height x y =
    { client_window; state; width; height; x; y }
  in
  let client_window p = p.client_window in
  let state p = p.state in
  let width p = p.width in
  let height p = p.height in
  let x p = p.x in
  let y p = p.y in
  Jsont.Object.map ~kind:"browser.SetClientWindowStateParameters" make
  |> Jsont.Object.mem "clientWindow" Client_window.jsont ~enc:client_window
  |> Jsont.Object.mem "state" Client_window_info.state_jsont ~enc:state
  |> Jsont.Object.opt_mem "width" Js_uint.jsont ~enc:width
  |> Jsont.Object.opt_mem "height" Js_uint.jsont ~enc:height
  |> Jsont.Object.opt_mem "x" Js_int.jsont ~enc:x
  |> Jsont.Object.opt_mem "y" Js_int.jsont ~enc:y
  |> Jsont.Object.finish

let set_client_window_state_command =
  let params_jsont = set_client_window_state_params_jsont in
  let result_jsont = Client_window_info.jsont in
  Command.define "browser.setClientWindowState" ~params_jsont ~result_jsont

let set_client_window_state
    c ?exts ~client_window ~state ?width ?height ?x ?y ()
  =
  let params = { client_window; state; width; height; x; y } in
  call ?exts c set_client_window_state_command params

(* browser.setDownloadBehavior *)

type download_behavior_object =
  (* formally this is a case object, we are too lazy to model it since
     just use it for encoding. *)
  { type' : string;
    destination_folder : string option }

let download_behavior_object_jsont =
  let make type' destination_folder = { type'; destination_folder } in
  let type' p = p.type' in
  let destination_folder p = p.destination_folder in
  Jsont.Object.map ~kind:"browser.DownloadBehavior" make
  |> Jsont.Object.mem "type" Jsont.string ~enc:type'
  |> Jsont.Object.opt_mem
    "destinationFolder" Jsont.string ~enc:destination_folder
  |> Jsont.Object.finish

type set_download_behavior_params =
  { download_behavior : download_behavior_object option;
    user_contexts : User_context.t list option }

let set_download_behavior_params_jsont =
  let make download_behavior user_contexts =
    { download_behavior; user_contexts }
  in
  let download_behavior p = p.download_behavior in
  let user_contexts p = p.user_contexts in
  Jsont.Object.map ~kind:"browser.SetDownloadBehaviorParameters" make
  |> Jsont.Object.mem
    "downloadBehaviour" (Jsont.option download_behavior_object_jsont)
    ~enc:download_behavior
  |> Jsont.Object.opt_mem
    "userContexts" (Jsont.list User_context.jsont) ~enc:user_contexts
  |> Jsont.Object.finish

let set_download_behavior_command =
  let params_jsont = set_download_behavior_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "browser.setDownloadBehavior" ~params_jsont ~result_jsont

let set_download_behavior c ?exts ~download_behavior ?user_contexts () =
  let download_behavior = match download_behavior with
  | None -> None
  | Some (`Allowed d) -> Some { type' = "allowed"; destination_folder = Some d }
  | Some `Denied -> Some { type' = "denied"; destination_folder = None }
  in
  let params = { download_behavior; user_contexts } in
  call ?exts c set_download_behavior_command params
