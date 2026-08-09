(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs_wd__types
open Webs_wd__protocol

let name = "webExtension"

(* Types *)

module Extension = struct
  type t = string
  let jsont = Jsont.with_doc ~kind:"webExtension.Extension" Jsont.string
end

module Extension_data = struct
  type t =
  | Path of string
  | Archive_path of string
  | Base64 of string

  let path_jsont =
    Jsont.Object.map ~kind:"webExtension.ExtensionPath" Fun.id
    |> Jsont.Object.mem "path" Jsont.string ~enc:Fun.id
    |> Jsont.Object.finish

  let archive_path_jsont =
    Jsont.Object.map ~kind:"webExtension.ExtensionArchivePath" Fun.id
    |> Jsont.Object.mem "archivePath" Jsont.string ~enc:Fun.id
    |> Jsont.Object.finish

  let archive_path_jsont =
    Jsont.Object.map ~kind:"webExtension.ExtensionBase64Encoded" Fun.id
    |> Jsont.Object.mem "value" Jsont.string ~enc:Fun.id
    |> Jsont.Object.finish

  let jsont =
    let path = Jsont.Object.Case.map "path" path_jsont ~dec:(fun p -> Path p) in
    let archive_path =
      Jsont.Object.Case.map "archivePath" path_jsont
        ~dec:(fun p -> Archive_path p)
    in
    let base64 =
      Jsont.Object.Case.map "base64" path_jsont ~dec:(fun v -> Base64 v)
    in
    let enc_case = function
    | Path p -> Jsont.Object.Case.value path p
    | Archive_path p -> Jsont.Object.Case.value archive_path p
    | Base64 p -> Jsont.Object.Case.value base64 p
    in
    let cases =
      Jsont.Object.Case.[make path; make archive_path; make base64 ]
    in
    Jsont.Object.map ~kind:"webExtension.ExtensionData" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string cases
      ~enc:Fun.id ~enc_case ~tag_to_string:Fun.id
    |> Jsont.Object.finish
end

module Install_result = struct
  type t = { extension : Extension.t }
  let extension r = r.extension
  let jsont =
    let make extension = { extension } in
    Jsont.Object.map ~kind:"webExtension.InstallResult" make
    |> Jsont.Object.mem "extension" Extension.jsont ~enc:extension
    |> Jsont.Object.finish
end

(* webExtension.install *)

type install_params = { extension_data : Extension_data.t }
let install_params_jsont =
  let make extension_data = { extension_data } in
  let extension_data p = p.extension_data in
    Jsont.Object.map ~kind:"webExtension.InstallParameters" make
    |> Jsont.Object.mem "extensionData"
      Extension_data.jsont ~enc:extension_data
    |> Jsont.Object.finish

let install_command =
  let params_jsont = install_params_jsont in
  let result_jsont = Install_result.jsont in
  Command.define "webExtension.install" ~params_jsont ~result_jsont

let install c ?exts ~extension_data () =
  call ?exts c install_command { extension_data }

(* webExtension.uninstall *)


type uninstall_params = { extension : Extension.t }
let uninstall_params_jsont =
  let make extension = { extension } in
  let extension p = p.extension in
    Jsont.Object.map ~kind:"webExtension.UninstallParameters" make
    |> Jsont.Object.mem "extension" Extension.jsont ~enc:extension
    |> Jsont.Object.finish

let uninstall_command =
  let params_jsont = uninstall_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "webExtension.uninstall" ~params_jsont ~result_jsont

let uninstall c ?exts ~extension () =
  call ?exts c uninstall_command { extension }
