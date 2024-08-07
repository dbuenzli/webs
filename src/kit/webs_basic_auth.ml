(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs

let ( let* ) = Result.bind
let strf = Printf.sprintf

type error = [ `Unknown_username | `Wrong_password ]
type username = string
type password = string
type check = username:username -> password:password -> (unit, error) result

let decode_basic_authentication credentials =
  match Http.Headers.values_of_string ~sep:' ' credentials with
  | scheme :: credentials :: _ ->
      let scheme = Http.Connector.Private.string_lowercase scheme in
      if scheme <> "basic"
      then Error (strf "auth-scheme %s: unsupported" scheme) else
      let* credentials =
        Result.map_error (Fun.const "base64 decode error") @@
        Http.Base64.decode credentials
      in
      begin match String.index_opt credentials ':' with
      | None -> Error ("No ':' found in basic authentication credentials")
      | Some i ->
          let u =
            Http.Connector.Private.string_subrange ~last:(i - 1) credentials
          in
          let p =
            Http.Connector.Private.string_subrange ~first:(i + 1) credentials
          in
          Ok (u, p)
      end
  | _ -> Error ("Not a basic auth-scheme")

let decode_credentials request =
  Http.Request.decode_header Http.Headers.authorization
    decode_basic_authentication request

let unauthorized_401 ~realm ~explain =
  let auth = strf "basic realm=\"%s\", charset=\"utf-8\"" realm in
  let headers = Http.Headers.(def www_authenticate) auth Http.Headers.empty in
  Http.Response.unauthorized_401 ~explain ~headers ()

let enticate ~check ~realm request =
  let* credentials = decode_credentials request in
  match credentials with
  | None -> unauthorized_401 ~realm ~explain:"No authorization header"
  | Some (username, password) ->
      match check ~username ~password with
      | Ok () -> Ok username
      | Error e ->
          let explain = match e with
          | `Unknown_username -> strf "user %s: unknown" username
          | `Wrong_password -> strf "user %s: wrong password" username
          in
          unauthorized_401 ~realm ~explain
