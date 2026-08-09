(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs_wd__types
open Webs_wd__protocol

let name = "log"

(* Types *)

module Entry = struct
  type level = [`Debug | `Info | `Warn | `Error]
  let level_jsont =
    Jsont.enum ~kind:"log.Level"
      ["debug", `Debug; "info", `Info; "warn", `Warn; "error", `Error]

  type method' = string
  type type' =
    [ `Console of method' * Webs_wd__script.Remote_value.t list
    | `Javascript
    | `Other of string ]

  type t =
    { level : level;
      source : Webs_wd__script.Source.t;
      text : string option;
      timestamp : int;
      stacktrace : Webs_wd__script.Stack_trace.t option;
      type' : type' }

  let level e = e.level
  let source e = e.source
  let text e = e.text
  let timetamp e = e.timestamp
  let stacktrace e = e.stacktrace
  let type' e = e.type'

  (* This is ugly see https://github.com/dbuenzli/jsont/issues/15 *)
  let jsont =
    let make level source text timestamp stacktrace type' method' args =
      let type' = match type' with
      | "console" ->
          let method' = match method' with
          | None -> Jsont.Error.msg Jsont.Meta.none "missing member method"
          | Some m -> m
          in
          let args = match args with
          | None -> Jsont.Error.msg Jsont.Meta.none "missing member args"
          | Some a -> a
          in
          `Console (method', args)
      | "javascript" -> `Javascript
      | text -> `Other text
      in
      { level; source; text; timestamp; stacktrace; type' }
    in
    let type' c = match c.type' with
    | `Console _ -> "console" | `Javascript -> "javascript"
    | `Other other -> other
    in
    let method' c = match c.type' with
    | `Console (m, _) -> Some m | _ -> None
    in
    let args c = match c.type' with
    | `Console (_, args) -> Some args | _ -> None
    in
    Jsont.Object.map ~kind:"log.Entry" make
    |> Jsont.Object.mem "level" level_jsont ~enc:level
    |> Jsont.Object.mem "source" Webs_wd__script.Source.jsont ~enc:source
    |> Jsont.Object.mem "text" Jsont.(option string) ~enc:text
    |> Jsont.Object.mem "timestamp" Js_uint.jsont ~enc:timetamp
    |> Jsont.Object.opt_mem
      "stackTrace" Webs_wd__script.Stack_trace.jsont ~enc:stacktrace
    |> Jsont.Object.mem "type" Jsont.string ~enc:type'
    |> Jsont.Object.opt_mem "method" Jsont.string  ~enc:method'
    |> Jsont.Object.opt_mem
      "args" Jsont.(list Webs_wd__script.Remote_value.jsont) ~enc:args
    |> Jsont.Object.finish
end

(* [log.entryAdded] *)

let entry_added =
  let params_jsont = Entry.jsont in
  Event.define "log.entryAdded" ~params_jsont
