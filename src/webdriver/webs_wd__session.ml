(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs_wd__types
open Webs_wd__protocol

let name = "session"

(* Types *)

module User_prompt_handler_type = struct
  type t = Accept | Dismiss | Ignore
  let jsont =
    let cases = ["accept", Accept; "dismiss", Dismiss; "ignore", Ignore] in
    Jsont.enum ~kind:"session.UserPromptHandlerType" cases
end

module User_prompt_handler = struct
  type t =
    { alert : User_prompt_handler_type.t option;
      before_unload : User_prompt_handler_type.t option;
      confirm : User_prompt_handler_type.t option;
      default : User_prompt_handler_type.t option;
      file : User_prompt_handler_type.t option;
      prompt : User_prompt_handler_type.t option; }

  let make ?alert ?before_unload ?confirm ?default ?file ?prompt () =
    { alert; before_unload; confirm; default; file; prompt }

  let make' alert before_unload confirm default file prompt =
    { alert; before_unload; confirm; default; file; prompt }

  let alert u = u.alert
  let before_unload u = u.before_unload
  let confirm u = u.confirm
  let default u = u.default
  let file u = u.file
  let prompt u = u.prompt
  let jsont =
    let htyp = User_prompt_handler_type.jsont in
    Jsont.Object.map ~kind:"session.UserPromptHandler" make'
    |> Jsont.Object.opt_mem "alert" htyp ~enc:alert
    |> Jsont.Object.opt_mem "beforeUnload" htyp ~enc:before_unload
    |> Jsont.Object.opt_mem "confirm" htyp ~enc:confirm
    |> Jsont.Object.opt_mem "default" htyp ~enc:default
    |> Jsont.Object.opt_mem "file" htyp ~enc:file
    |> Jsont.Object.opt_mem "prompt" htyp ~enc:prompt
    |> Jsont.Object.finish
end

module Proxy_configuration = struct
  module Only_exts_type = struct
    type t = { exts : Exts.t }
    let make ?(exts = Exts.none) () = { exts }
    let make' exts = { exts }
    let exts p = p.exts
  end

  module Autodetect = struct
    include Only_exts_type
    let jsont =
      Jsont.Object.map ~kind:"session.AutodetectProxyConfiguration" make'
      |> Jsont.Object.keep_unknown Exts.jsont ~enc:exts
      |> Jsont.Object.finish
  end

  module Direct = struct
    include Only_exts_type
    let jsont =
      Jsont.Object.map ~kind:"session.DirectProxyConfiguration" make'
      |> Jsont.Object.keep_unknown Exts.jsont ~enc:exts
      |> Jsont.Object.finish
  end

  module Manual = struct
    type t =
      { http_proxy : string option;
        ssl_proxy : string option;
        socks_proxy : string option;
        socks_version : int option;
        no_proxy : string list option;
        exts : Exts.t }

    let make
        ?(exts = Exts.none) ?http_proxy ?ssl_proxy ?socks_proxy ?socks_version
        ?no_proxy ()
      =
      { http_proxy; ssl_proxy; socks_proxy; socks_version; no_proxy; exts }

    let make' http_proxy ssl_proxy socks_proxy socks_version no_proxy exts =
      { http_proxy; ssl_proxy; socks_proxy; socks_version; no_proxy; exts }

    let http_proxy p = p.http_proxy
    let ssl_proxy p = p.ssl_proxy
    let socks_proxy p = p.socks_proxy
    let socks_version p = p.socks_version
    let no_proxy p = p.no_proxy
    let exts p = p.exts
    let jsont =
      Jsont.Object.map ~kind:"session.ManualProxyConfiguration" make'
      |> Jsont.Object.opt_mem "httpProxy" Jsont.string ~enc:http_proxy
      |> Jsont.Object.opt_mem "sslProxy" Jsont.string ~enc:ssl_proxy
      |> Jsont.Object.opt_mem "socksProxy" Jsont.string ~enc:socks_proxy
      |> Jsont.Object.opt_mem "socksVersion" Jsont.uint8 ~enc:socks_version
      |> Jsont.Object.opt_mem "noProxy" Jsont.(list string) ~enc:no_proxy
      |> Jsont.Object.keep_unknown Exts.jsont ~enc:exts
      |> Jsont.Object.finish
  end

  module Pac = struct
    type t =
      { proxy_autoconfig_url : string;
        exts : Exts.t }

    let make ?(exts = Exts.none) ~proxy_autoconfig_url () =
      { proxy_autoconfig_url; exts }

    let make' proxy_autoconfig_url exts = { proxy_autoconfig_url; exts }

    let proxy_autoconfig_url p = p.proxy_autoconfig_url
    let exts p = p.exts

    let jsont =
      Jsont.Object.map ~kind:"session.PacProxyConfiguration" make'
      |> Jsont.Object.mem
        "proxyAutoconfigUrl" Jsont.string ~enc:proxy_autoconfig_url
      |> Jsont.Object.keep_unknown Exts.jsont ~enc:exts
      |> Jsont.Object.finish
  end

  module System = struct
    include Only_exts_type
    let jsont =
      Jsont.Object.map ~kind:"session.SystemProxyConfiguration" make'
      |> Jsont.Object.keep_unknown Exts.jsont ~enc:exts
      |> Jsont.Object.finish
  end

  type t =
  | Autodetect of Autodetect.t
  | Direct of Direct.t
  | Manual of Manual.t
  | Pac of Pac.t
  | System of System.t
  | Empty of unit (* Work around Firefox empty object. *)

  let jsont =
    let autodetect =
      let dec c = Autodetect c in
      Jsont.Object.Case.map "autodetect" Autodetect.jsont ~dec
    in
    let direct =
      let dec c = Direct c in
      Jsont.Object.Case.map "direct" Direct.jsont ~dec
    in
    let manual =
      let dec c = Manual c in
      Jsont.Object.Case.map "manual" Manual.jsont ~dec
    in
    let pac =
      let dec c = Pac c in
      Jsont.Object.Case.map "pac" Pac.jsont ~dec
    in
    let system =
      let dec c = System c in
      Jsont.Object.Case.map "system" System.jsont ~dec
    in
    let empty =
      let empty = Jsont.Object.map () |> Jsont.Object.finish in
      (* Work around Firefox empty object *)
      let dec () = Empty () in
      Jsont.Object.Case.map "empty" empty ~dec
    in
    let enc_case = function
    | Autodetect c -> Jsont.Object.Case.value autodetect c
    | Direct c -> Jsont.Object.Case.value direct c
    | Manual c -> Jsont.Object.Case.value manual c
    | Pac c -> Jsont.Object.Case.value pac c
    | System c -> Jsont.Object.Case.value system c
    | Empty c -> Jsont.Object.Case.value empty c
    in
    let cases =
      Jsont.Object.Case.[make autodetect; make direct; make manual; make pac;
                         make system; make empty ]
    in
    Jsont.Object.map ~kind:"session.ProxyConfiguration" Fun.id
    |> Jsont.Object.case_mem "proxyType" Jsont.string cases
      ~tag_to_string:Fun.id ~enc:Fun.id ~enc_case
      (* Work around Firefox empty object *)
      ~dec_absent:"empty" ~enc_omit:(String.equal "empty")
    |> Jsont.Object.finish
end

module Capability_request = struct
  type t =
    { accept_insecure_certs : bool option;
      browser_name : string option;
      browser_version : string option;
      platform_name : string option;
      proxy : Proxy_configuration.t option;
      unhandled_prompt_behaviour : User_prompt_handler.t option;
      exts : Exts.t; }

  let make ?(exts = Exts.none) ?accept_insecure_certs ?browser_name
      ?browser_version ?platform_name ?proxy
      ?unhandled_prompt_behaviour () =
    { accept_insecure_certs; browser_name; browser_version;
      platform_name; proxy; unhandled_prompt_behaviour; exts }

  let make'
      accept_insecure_certs browser_name browser_version platform_name proxy
      unhandled_prompt_behaviour exts
    =
    { accept_insecure_certs; browser_name; browser_version;
      platform_name; proxy; unhandled_prompt_behaviour; exts }

  let accept_insecure_certs c = c.accept_insecure_certs
  let browser_name c = c.browser_name
  let browser_version c = c.browser_version
  let platform_name c = c.platform_name
  let proxy c = c.proxy
  let unhandled_prompt_behaviour c = c.unhandled_prompt_behaviour
  let exts c = c.exts
  let jsont =
    Jsont.Object.map ~kind:"session.CapabilityRequest" make'
    |> Jsont.Object.opt_mem
      "acceptInsecureCerts" Jsont.bool ~enc:accept_insecure_certs
    |> Jsont.Object.opt_mem "browserName" Jsont.string ~enc:browser_name
    |> Jsont.Object.opt_mem "browserVersion" Jsont.string ~enc:browser_version
    |> Jsont.Object.opt_mem "platformName" Jsont.string ~enc:platform_name
    |> Jsont.Object.opt_mem "proxy" Proxy_configuration.jsont ~enc:proxy
    |> Jsont.Object.opt_mem
      "unhandledPromptBehaviour" User_prompt_handler.jsont
      ~enc:unhandled_prompt_behaviour
    |> Jsont.Object.keep_unknown Exts.jsont ~enc:exts
    |> Jsont.Object.finish
end

module Capabilities_request = struct
  type t =
    { always_match : Capability_request.t option;
      first_match : Capability_request.t list option; }

  let make ?always_match ?first_match () = { always_match; first_match }
  let none = make ()
  let always_match c = c.always_match
  let first_match c = c.first_match
  let jsont =
    let make always_match first_match = { always_match; first_match } in
    Jsont.Object.map ~kind:"session.CapabilitiesRequest" make
    |> Jsont.Object.opt_mem
      "alwaysMatch" Capability_request.jsont ~enc:always_match
    |> Jsont.Object.opt_mem
      "firstMatch" (Jsont.list Capability_request.jsont) ~enc:first_match
    |> Jsont.Object.finish
end

module Capabilities = struct
  type t =
    { accept_insecure_certs : bool;
      browser_name : string;
      browser_version : string;
      platform_name : string;
      set_window_rect : bool;
      user_agent : string;
      proxy : Proxy_configuration.t option;
      unhandled_prompt_behaviour : User_prompt_handler.t option;
      websocket_url : string option;
      exts : Exts.t; }

  let make'
      accept_insecure_certs browser_name browser_version platform_name
      set_window_rect user_agent proxy unhandled_prompt_behaviour
      websocket_url exts
    =
    { accept_insecure_certs; browser_name; browser_version;
      platform_name; set_window_rect; user_agent; proxy;
      unhandled_prompt_behaviour; websocket_url; exts }

  let accept_insecure_certs c = c.accept_insecure_certs
  let browser_name c = c.browser_name
  let browser_version c = c.browser_version
  let platform_name c = c.platform_name
  let set_window_rect c = c.set_window_rect
  let user_agent c = c.user_agent
  let proxy c = c.proxy
  let unhandled_prompt_behaviour c = c.unhandled_prompt_behaviour
  let websocket_url c = c.websocket_url
  let exts c = c.exts

  let jsont =
    let kind = "session.Capabilities" (* N.B. unnamed in the spec *) in
    Jsont.Object.map ~kind make'
    |> Jsont.Object.mem
      "acceptInsecureCerts" Jsont.bool ~enc:accept_insecure_certs
    |> Jsont.Object.mem "browserName" Jsont.string ~enc:browser_name
    |> Jsont.Object.mem "browserVersion" Jsont.string ~enc:browser_version
    |> Jsont.Object.mem "platformName" Jsont.string ~enc:platform_name
    |> Jsont.Object.mem "setWindowRect" Jsont.bool ~enc:set_window_rect
    |> Jsont.Object.mem "userAgent" Jsont.string ~enc:user_agent
    |> Jsont.Object.opt_mem "proxy" Proxy_configuration.jsont ~enc:proxy
    |> Jsont.Object.opt_mem
      "unhandledPromptBehavior" User_prompt_handler.jsont
      ~enc:unhandled_prompt_behaviour
    |> Jsont.Object.opt_mem "webSocketUrl" Jsont.string ~enc:websocket_url
    |> Jsont.Object.keep_unknown Exts.jsont ~enc:exts
    |> Jsont.Object.finish
end

module New_result = struct
  type t = { session_id : string; capabilities : Capabilities.t; }
  let session_id r = r.session_id
  let capabilities r = r.capabilities
  let jsont =
    let make session_id capabilities = { session_id; capabilities } in
    Jsont.Object.map ~kind:"session.NewResult" make
    |> Jsont.Object.mem "sessionId" Jsont.string ~enc:session_id
    |> Jsont.Object.mem "capabilities" Capabilities.jsont ~enc:capabilities
    |> Jsont.Object.finish
end

module Status_result = struct
  type t = { ready : bool; message : string; }
  let ready r = r.ready
  let message r = r.message
  let jsont =
    let make ready message = { ready; message } in
    Jsont.Object.map ~kind:"session.StatusResult" make
    |> Jsont.Object.mem "ready" Jsont.bool ~enc:ready
    |> Jsont.Object.mem "message" Jsont.string ~enc:message
    |> Jsont.Object.finish
end

module Subscription = struct
  type t = string
  let jsont = Jsont.with_doc ~kind:"session.Subscription" Jsont.string
end

module Subscribe_result = struct
  type t = { subscription : Subscription.t }
  let subscription r = r.subscription
  let jsont =
    let make subscription = { subscription } in
    Jsont.Object.map ~kind:"session.SubscribeResult" make
    |> Jsont.Object.mem "subscription" Subscription.jsont ~enc:subscription
    |> Jsont.Object.finish
end

(* session.status *)

let status_command =
  let params_jsont = Empty_params.jsont in
  let result_jsont = Status_result.jsont in
  Command.define "session.status" ~params_jsont ~result_jsont

let status c ?exts () = call ?exts c status_command Empty_params.empty

(* session.new *)

type new_params = { capabilities : Capabilities_request.t }
let new_params_jsont =
  let make capabilities = { capabilities } in
  let caps (p : new_params) = p.capabilities in
  Jsont.Object.map ~kind:"session.NewParameters" make
  |> Jsont.Object.mem "capabilities" Capabilities_request.jsont ~enc:caps
  |> Jsont.Object.finish

let new_command =
  let params_jsont = new_params_jsont in
  let result_jsont = New_result.jsont in
  Command.define "session.new" ~params_jsont ~result_jsont

let new' c ?exts ?(capabilities = Capabilities_request.none) () =
  call ?exts c new_command { capabilities }

(* session.end *)

let end_command =
  let params_jsont = Empty_params.jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "session.end" ~params_jsont ~result_jsont

let end' c ?exts () = call ?exts c end_command Empty_params.empty

(* session.subscribe *)

type subscribe_params =
  { events : string list;
    contexts : Browsing_context.t list option;
    user_contexts : User_context.t list option; }

let subscribe_params_jsont =
  let make events contexts user_contexts = {events; contexts; user_contexts} in
  let events p = p.events and contexts p = p.contexts
  and user_contexts p = p.user_contexts in
  Jsont.Object.map ~kind:"session.SubscribeParameters" make
  |> Jsont.Object.mem
    "events" Jsont.(list string) ~enc:events
  |> Jsont.Object.opt_mem
    "contexts" Jsont.(list Browsing_context.jsont) ~enc:contexts
  |> Jsont.Object.opt_mem
    "userContexts" Jsont.(list User_context.jsont) ~enc:user_contexts
  |> Jsont.Object.finish

let subscribe_command =
  let params_jsont = subscribe_params_jsont in
  let result_jsont = Subscribe_result.jsont in
  Command.define "session.subscribe" ~params_jsont ~result_jsont

let subscribe c ?exts ~events ?contexts ?user_contexts () =
  call ?exts c subscribe_command { events; contexts; user_contexts }

(* session.unsubscribe *)

type unsubscribe_params =
  (* Formally both should not be present at the same time
     we ensure that at the API level. *)
  { events : string list option;
    subscriptions : Subscription.t list option }

let unsubscribe_params_jsont =
  let make events subscriptions = { events; subscriptions } in
  let events p = p.events in
  let subscriptions p = p.subscriptions in
  Jsont.Object.map ~kind:"session.UnsubscribeBy{ID,Attributes}Request" make
  |> Jsont.Object.opt_mem "events" Jsont.(list string) ~enc:events
  |> Jsont.Object.opt_mem
    "subscriptions" Jsont.(list Subscription.jsont) ~enc:subscriptions
  |> Jsont.Object.finish

let unsubscribe_command =
  let params_jsont = unsubscribe_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "session.unsubscribe" ~params_jsont ~result_jsont

let unsubscribe c ?exts req () =
  let params = match req with
  | `Events evs -> { events = Some evs; subscriptions = None }
  | `Subscriptions ss -> { events = None; subscriptions = Some ss }
  in
  call ?exts c unsubscribe_command params
