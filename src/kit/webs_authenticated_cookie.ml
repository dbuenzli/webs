(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs

(* Setting and clearing *)

let set ~private_key ~expire ?attributes ~name data response =
  let value = Webs_authenticatable.encode ~private_key ~expire data in
  let cookie = Http.Cookie.encode ?attributes ~name value in
  let headers = Http.Response.headers response in
  let headers = Http.Headers.add_set_cookie cookie headers in
  Http.Response.with_headers headers response

let clear ?attributes ~name response =
  let max_age = Some (-1) in
  let attributes = Http.Cookie.attributes ?init:attributes ~max_age () in
  let cookie = Http.Cookie.encode ~attributes ~name "" in
  let headers = Http.Response.headers response in
  let headers = Http.Headers.add_set_cookie cookie headers in
  Http.Response.with_headers headers response

(* Getting *)

(* TODO I think we should rather switch to bad request on decoding
   errors and simply have Webs_authenticatable.error *)

type error = [ Webs_authenticatable.error | `Cookie of string ]

let error_message = function
| `Cookie s -> s
| #Webs_authenticatable.error as e -> Webs_authenticatable.error_message e

let error_string r = Result.map_error error_message r

let find ~private_key ~now ~name request =
  match Http.Request.find_cookie ~name request with
  | Error e -> Error (`Cookie e)
  | Ok (None | Some "") -> Ok None
  | Ok (Some c) ->
      let d = Webs_authenticatable.decode ~private_key ~now c in
      let d =
        (d :> (Webs_authenticatable.time option * string, error) result)
      in
      Result.map Option.some d
