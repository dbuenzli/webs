(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs_wd__types
open Webs_wd__protocol

let name = "storage"

(* Types *)

module Partition_key = struct
  type t =
    { user_context : string option;
      source_origin : string option;
      exts : Exts.t }
  let make ?(exts = Exts.none) ?user_context ?source_origin () =
    { user_context; source_origin; exts }
  let user_context p = p.user_context
  let source_origin p = p.source_origin
  let exts p = p.exts
  let jsont =
    let make user_context source_origin exts =
      { user_context; source_origin; exts }
    in
    Jsont.Object.map ~kind:"storage.ParitionKey" make
    |> Jsont.Object.opt_mem "userContext" Jsont.string ~enc:user_context
    |> Jsont.Object.opt_mem "sourceOrigin" Jsont.string ~enc:source_origin
    |> Jsont.Object.keep_unknown Exts.jsont ~enc:exts
    |> Jsont.Object.finish
end

module Cookie_filter = struct
  type t =
    { name : string option;
      value : Webs_wd__network.Bytes_value.t option;
      domain : string option;
      path : string option;
      size : int option;
      http_only : bool option;
      secure : bool option;
      same_site : Webs_wd__network.Same_site.t option;
      expiry : int option;
      exts : Exts.t }
  let make
      ?name ?value ?domain ?path ?size ?http_only ?secure ?same_site ?expiry
      ?(exts = Exts.none) ()
    =
    { name; value; domain; path; size; http_only; secure; same_site; expiry;
      exts }
  let name c = c.name
  let value c = c.value
  let domain c = c.domain
  let path c = c.path
  let size c = c.size
  let http_only c = c.http_only
  let secure c = c.secure
  let same_site c = c.same_site
  let expiry c = c.expiry
  let exts c = c.exts
  let jsont =
    let make
        name value domain path size http_only secure same_site expiry exts
      =
      { name; value; domain; path; size; http_only; secure; same_site;
        expiry; exts }
    in
    Jsont.Object.map ~kind:"storage.CookieFilter" make
    |> Jsont.Object.opt_mem "name" Jsont.string ~enc:name
    |> Jsont.Object.opt_mem
      "value" Webs_wd__network.Bytes_value.jsont ~enc:value
    |> Jsont.Object.opt_mem "domain" Jsont.string ~enc:domain
    |> Jsont.Object.opt_mem "path" Jsont.string ~enc:path
    |> Jsont.Object.opt_mem "size" Js_uint.jsont ~enc:size
    |> Jsont.Object.opt_mem "httpOnly" Jsont.bool ~enc:http_only
    |> Jsont.Object.opt_mem "secure" Jsont.bool ~enc:secure
    |> Jsont.Object.opt_mem
      "sameSite" Webs_wd__network.Same_site.jsont ~enc:same_site
    |> Jsont.Object.opt_mem "expiry" Js_uint.jsont ~enc:expiry
    |> Jsont.Object.keep_unknown Exts.jsont ~enc:exts
    |> Jsont.Object.finish
end

module Browsing_context_partition_descriptor = struct
  type t = { context : Browsing_context.t }
  let make ~context () = { context }
  let context c = c.context
  let jsont =
    let make context = { context } in
    Jsont.Object.map ~kind:"storage.BrowsingContextPartitionDescriptor" make
    |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:context
    |> Jsont.Object.finish
end

module Storage_key_partition_descriptor = struct
  type t =
    { user_context : User_context.t option;
      source_origin : string option;
      exts : Exts.t }

  let make ?(exts = Exts.none) ?source_origin ?user_context () =
    { user_context; source_origin; exts }

  let user_context s = s.user_context
  let source_origin s = s.source_origin
  let exts s = s.exts
  let jsont =
    let make user_context source_origin exts =
      { user_context; source_origin; exts }
    in
    Jsont.Object.map ~kind:"storage.StorageKeyPartitionDescriptor" make
    |> Jsont.Object.opt_mem "userContext" User_context.jsont ~enc:user_context
    |> Jsont.Object.opt_mem "sourceOrigin" Jsont.string ~enc:source_origin
    |> Jsont.Object.keep_unknown Exts.jsont ~enc:exts
    |> Jsont.Object.finish
end

module Partition_descriptor = struct
  type t =
  [ `Browsing_context of Browsing_context_partition_descriptor.t
  | `Storage_key of Storage_key_partition_descriptor.t ]
  let jsont =
    let browsing_context =
      Jsont.Object.Case.map "context"
        Browsing_context_partition_descriptor.jsont
        ~dec:(fun c -> `Browsing_context c)
    in
    let storage_key =
      Jsont.Object.Case.map "storageKey"
        Storage_key_partition_descriptor.jsont
        ~dec:(fun s -> `Storage_key s)
    in
    let enc_case = function
    | `Browsing_context c -> Jsont.Object.Case.value browsing_context c
    | `Storage_key s -> Jsont.Object.Case.value storage_key s
    in
    let cases = Jsont.Object.Case.[make browsing_context; make storage_key] in
    Jsont.Object.map ~kind:"storage.PartitionDescriptor" Fun.id
    |> Jsont.Object.case_mem
      "type" Jsont.string cases ~enc_case ~enc:Fun.id ~tag_to_string:Fun.id
    |> Jsont.Object.finish
end

module Partial_cookie = struct
  type t =
    { name : string;
      value : Webs_wd__network.Bytes_value.t;
      domain : string;
      path : string option;
      http_only : bool option;
      secure : bool option;
      same_site : Webs_wd__network.Same_site.t option;
      expiry : int option;
      exts : Exts.t }
  let make
      ~name ~value ~domain ?path ?http_only ?secure ?same_site ?expiry
      ?(exts = Exts.none) ()
    =
    { name; value; domain; path; http_only; secure; same_site; expiry;
      exts }
  let name c = c.name
  let value c = c.value
  let domain c = c.domain
  let path c = c.path
  let http_only c = c.http_only
  let secure c = c.secure
  let same_site c = c.same_site
  let expiry c = c.expiry
  let exts c = c.exts
  let jsont =
    let make
        name value domain path http_only secure same_site expiry exts
      =
      { name; value; domain; path; http_only; secure; same_site;
        expiry; exts }
    in
    Jsont.Object.map ~kind:"storage.PartialCookie" make
    |> Jsont.Object.mem "name" Jsont.string ~enc:name
    |> Jsont.Object.mem
      "value" Webs_wd__network.Bytes_value.jsont ~enc:value
    |> Jsont.Object.mem "domain" Jsont.string ~enc:domain
    |> Jsont.Object.opt_mem "path" Jsont.string ~enc:path
    |> Jsont.Object.opt_mem "httpOnly" Jsont.bool ~enc:http_only
    |> Jsont.Object.opt_mem "secure" Jsont.bool ~enc:secure
    |> Jsont.Object.opt_mem
      "sameSite" Webs_wd__network.Same_site.jsont ~enc:same_site
    |> Jsont.Object.opt_mem "expiry" Js_uint.jsont ~enc:expiry
    |> Jsont.Object.keep_unknown Exts.jsont ~enc:exts
    |> Jsont.Object.finish
end

module Get_cookies_result = struct
  type t =
    { cookies : Webs_wd__network.Cookie.t list;
      partition_key : Partition_key.t }

  let cookies r = r.cookies
  let partition_key r = r.partition_key
  let jsont =
    let make cookies partition_key = { cookies; partition_key } in
    Jsont.Object.map ~kind:"storage.GetCookiesResult" make
    |> Jsont.Object.mem
      "cookies" (Jsont.list Webs_wd__network.Cookie.jsont) ~enc:cookies
    |> Jsont.Object.mem
      "partitionKey" Partition_key.jsont ~enc:partition_key
    |> Jsont.Object.finish
end

module Set_cookie_result = struct
  type t = { partition_key : Partition_key.t }
  let partition_key r = r.partition_key
  let jsont =
    let make partition_key = { partition_key } in
    Jsont.Object.map ~kind:"storage.SetCookieResult" make
    |> Jsont.Object.mem "partitionKey" Partition_key.jsont ~enc:partition_key
    |> Jsont.Object.finish
end

module Delete_cookies_result = Set_cookie_result

(* [storage.getCookies] *)

type get_cookies_params =
  { filter : Cookie_filter.t option;
    partition : Partition_descriptor.t option }

let get_cookies_params_jsont =
  let make filter partition = { filter; partition } in
  let filter p = p.filter and partition p = p.partition in
  Jsont.Object.map ~kind:"storage.GetCookiesParameters" make
  |> Jsont.Object.opt_mem "filter" Cookie_filter.jsont ~enc:filter
  |> Jsont.Object.opt_mem "partition" Partition_descriptor.jsont ~enc:partition
  |> Jsont.Object.finish

let get_cookies_command =
  let params_jsont = get_cookies_params_jsont in
  let result_jsont = Get_cookies_result.jsont in
  Command.define "storage.getCookies" ~params_jsont ~result_jsont

let get_cookies c ?exts ?filter ?partition () =
  call c ?exts get_cookies_command {filter; partition}

(* [storage.setCookie] *)

type set_cookie_params =
  { cookie : Partial_cookie.t;
    partition : Partition_descriptor.t option; }

let set_cookie_params =
  let make cookie partition = { cookie; partition } in
  let cookie p = p.cookie and partition p = p.partition in
  Jsont.Object.map ~kind:"storage.SetCookieParameters" make
  |> Jsont.Object.mem "cookie" Partial_cookie.jsont ~enc:cookie
  |> Jsont.Object.opt_mem "partition" Partition_descriptor.jsont ~enc:partition
  |> Jsont.Object.finish

let set_cookie_command =
  let params_jsont = set_cookie_params in
  let result_jsont = Set_cookie_result.jsont in
  Command.define "storage.setCookie" ~params_jsont ~result_jsont

let set_cookie c ?exts ~cookie ?partition () =
  call c ?exts set_cookie_command {cookie; partition}

(* [storage.deleteCookies] *)

type delete_cookies_params =
  { filter : Cookie_filter.t option;
    partition : Partition_descriptor.t option }

let delete_cookies_params_jsont =
  let make filter partition = { filter; partition } in
  let filter p = p.filter and partition p = p.partition in
  Jsont.Object.map ~kind:"storage.DeleteCookiesParameters" make
  |> Jsont.Object.opt_mem "filter" Cookie_filter.jsont ~enc:filter
  |> Jsont.Object.opt_mem "partition" Partition_descriptor.jsont ~enc:partition
  |> Jsont.Object.finish

let delete_cookies_command =
  let params_jsont = delete_cookies_params_jsont in
  let result_jsont = Delete_cookies_result.jsont in
  Command.define "storage.deleteCookies" ~params_jsont ~result_jsont

let delete_cookies c ?exts ?filter ?partition () =
  call c ?exts delete_cookies_command {filter; partition}
