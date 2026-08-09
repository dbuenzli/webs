(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open More
open Result.Syntax
open Webs_webdriver

let connection = Test.Arg.make ()

(* TODO should we wrap each module test in a session ? *)

let log ~module' cmd t v =
  Log.debug (fun m -> m "@[%s.%s:@ @[%a@]@]" module' cmd (Wd.pp_json t) v)

let trace ~module' cmd t v =
  Log.debug (fun m -> m "@[%s.%s:@ @[%a@]@]" module' cmd (Wd.pp_json t) v); v

let test_session_module =
  Test.test' connection "session module" @@ fun c ->
  let log cmd t v = log ~module':"session" cmd t v in
  log "status" Wd_session.Status_result.jsont @@
  Wd_session.status c ();
  log "new" Wd_session.New_result.jsont @@
  Wd_session.new' c ();
  let subscribe = Wd_session.subscribe c ~events:["browsingContext"] () in
  log "subscribe" Wd_session.Subscribe_result.jsont subscribe;
  (*  let subscriptions = (`Subscriptions [subscribe.subscription]) in
  let unsubscribe = Wd_session.unsubscribe c subscriptions () in
      log "unsubscribe" Wd_session.unsubscribe_result_jsont unsubscribe; *)
  if false then begin
    log "session.end" Wd.Empty_result.jsont @@
    Wd_session.end' c ()
  end;
  ()

let test_browser_module =
  Test.test' connection "browser module" @@ fun c ->
  let log cmd t v = log ~module':"browser" cmd t v in
  let trace cmd t v = trace ~module':"browser" cmd t v in
  log "createUserContext" Wd_browser.User_context_info.jsont @@
  Wd_browser.create_user_context c ();
  let ws =
    trace "getClientWindows" Wd_browser.Get_client_windows_result.jsont @@
    Wd_browser.get_client_windows c ()
  in
  begin match Wd_browser.Get_client_windows_result.client_windows ws with
  | [] -> ()
  | w :: _ ->
      try
        let client_window = Wd_browser.Client_window_info.client_window w in
        log "setClientWindowState" Wd_browser.Client_window_info.jsont @@
        Wd_browser.set_client_window_state c ~client_window ~state:Normal ()
      with
      | Wd.Error (Response _ as e) ->
          (* Unimplemented on Firefox *)
          Log.debug (fun m -> m "%a" Fmt.lines (Wd.Error.to_string e))
  end;
  let cs =
    trace "getUserContexts" Wd_browser.Get_user_contexts_result.jsont @@
    Wd_browser.get_user_contexts c ()
  in
  let () =
    let non_default_user_context u =
      let user_context = Wd_browser.User_context_info.user_context u in
      if user_context <> "default" then Some u else None
    in
    let ucs = Wd_browser.Get_user_contexts_result.user_contexts cs in
    match List.find_map non_default_user_context ucs with
    | None -> ()
    | Some u ->
        let user_context = Wd_browser.User_context_info.user_context u in
        let res = Wd_browser.remove_user_context c ~user_context () in
        log "removeUserContext" Wd.Empty_result.jsont res
  in
  ()

let test_browsing_context =
  Test.test' connection "browsingContext module" @@ fun c ->
  let log cmd t v = log ~module':"browsingContext" cmd t v in
  let trace cmd t v = trace ~module':"browsingContext" cmd t v in
  Log.if_error ~use:() @@
  let ctx =
    trace "create" Wd_browsing_context.Create_result.jsont @@
    Wd_browsing_context.create c ~type':`Tab ()
  in
  let context = Wd_browsing_context.Create_result.context ctx in
  let viewport =
    Some (Wd_browsing_context.Viewport.make ~width:1920 ~height:1080 ())
  in
  let device_pixel_ratio = Some 2.0 in
  log "setViewport" Wd.Empty_result.jsont @@
  Wd_browsing_context.set_viewport c ~context ~viewport ~device_pixel_ratio ();
  let url = "https://erratique.ch" and wait = `Complete in
  log "navigate" Wd_browsing_context.Navigate_result.jsont @@
  Wd_browsing_context.navigate c ~context ~url ~wait ();
  let data =
    Wd_browsing_context.Capture_screenshot_result.data @@
    Wd_browsing_context.capture_screenshot c ~context ()
  in
  let* png = Webs_base64.decode Padded data in
  let data =
    let page = Wd_browsing_context.Print_page.a4 in
    let margin = Wd_browsing_context.Print_margin.make () in
    Wd_browsing_context.Print_result.data @@
    Wd_browsing_context.print c ~context ~page ~margin ()
  in
  let* pdf = Webs_base64.decode Padded data in
  (* FIXME remove that *)
  let force = true and make_path = false in
  let* () = More.Os.File.write ~force ~make_path (Fpath.v "/tmp/shot.png")png in
  let* () = More.Os.File.write ~force ~make_path (Fpath.v "/tmp/shot.pdf")pdf in
  let _nodes =
    log "locateNodes" Wd_browsing_context.Locate_nodes_result.jsont @@
    let locator = `Css "html head title" in
    let serialization_options =
      Wd_script.Serialization_options.make ~max_dom_depth:None ()
    in
    Wd_browsing_context.locate_nodes c ~context ~locator
      ~serialization_options ()
  in
  Ok ()

open Cmdliner
open Cmdliner.Term.Syntax

let main () =
  Test.main' @@
  let+ trace =
    let doc = "Trace the protocol and the command results." in
    Arg.(value & flag & info ["trace"] ~doc)
  and+ driver =
    let alts = Wd.Connection.[
        "chrome", Chrome; "edge", Edge; "firefox", Firefox; "safari", Safari ]
    in
    let driver_conv = Arg.enum ~docv:"DRIVER" alts in
    let doc_alts = Arg.doc_alts_enum alts in
    let doc = Fmt.str "$(docv) is the WebDriver to use. Must be %s." doc_alts in
    Arg.(value & opt driver_conv Firefox & info ["driver"] ~doc)
  in
  fun () ->
    if trace then Log.set_level Debug;
    Test.error_to_fail @@ Result.join @@ Wd.Connection.with_open driver @@
    fun c ->
    Test.autorun () ~args:Test.Arg.[value connection c];
    log ~module':"browser" "close" Wd.Empty_result.jsont @@
    Wd_browser.close c ();
    Ok 0

let () = if !Sys.interactive then () else exit (main ())
