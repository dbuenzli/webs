(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open More
open Webs_webdriver

let () = if true then Log.set_level Info

let drive c ~url =
  let events =
    [ Wd.Event.name Wd_log.entry_added;
      Wd.Event.name Wd_browsing_context.dom_content_loaded; ]
  in
  ignore (Wd_session.new' c ());
  ignore (Wd_session.subscribe c ~events ());
  let context =
    Wd_browsing_context.Create_result.context @@
    Wd_browsing_context.create c ~type':`Tab ()
  in
  Log.stdout (fun m -> m "Navigating");
  ignore (Wd_browsing_context.navigate c ~context ~url ~wait:`None ());
  while true do
    let log = Wd.Event.await Wd_log.entry_added in
    let text = Option.value ~default:"<unknown>" (Wd_log.Entry.text log) in
    Log.stdout (fun m -> m "%s" text);
  done;
  ignore (Wd.Event.await Wd_browsing_context.context_destroyed);
  ()

let browser_console driver ~gui url =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let config = Wd.Connection.Config.make ~headless:(not gui) () in
  Wd.Connection.with_open ~config driver @@ fun c ->
  drive c ~url;
  Cmdliner.Cmd.Exit.ok

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  Cmd.make (Cmd.info "browser-console" ~version:"%%VERSION%%") @@
  let+ driver =
    let alts = Wd.Connection.[
        "chrome", Chrome; "edge", Edge; "firefox", Firefox; "safari", Safari ]
    in
    let driver_conv = Arg.enum ~docv:"DRIVER" alts in
    let doc_alts = Arg.doc_alts_enum alts in
    let doc = Fmt.str "$(docv) is the WebDriver to use. Must be %s." doc_alts in
    Arg.(value & opt driver_conv Firefox & info ["driver"] ~doc)
  and+ gui =
    let doc = "Run with the GUI instead of headless mode" in
    Arg.(value & flag & info ["g"; "gui"] ~doc)
  and+ url =
    let doc = "$(docv) is the URL to open" in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"URL")
  in
  browser_console driver ~gui url

let main () = Cmd.eval' cmd
let () = if !Sys.interactive then () else exit (main ())
