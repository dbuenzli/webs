(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Result.Syntax
open Webs

(* Fetch to string *)

let fetch ~follow url =
  let* httpc = Webs_spawn_client.make () in
  Http_client.get httpc ~follow ~url

(* Fetch more control *)

let get httpc ~follow ~url =
  let* request = Http.Request.of_url `GET ~url in
  let* response = Http_client.request httpc ~follow request in
  match Http.Response.status response with
  | 200 -> Http.Body.to_string (Http.Response.body response)
  | st -> Error (Format.asprintf "%a" Http.Status.pp st)

(* Fetch final URL *)

let fetch ~follow url =
  let* httpc = Webs_spawn_client.make () in
  let* request = Http.Request.of_url `GET ~url in
  let* response = Http_client.request httpc ~follow request in
  match Http.Response.status response with
  | 200 ->
      let location =
        let headers = Http.Response.headers response in
        match Http.Headers.find Http_client.x_follow_location headers with
        | None -> url | Some url -> url
      in
      let* data = Http.Body.to_string (Http.Response.body response) in
      Ok (location, data)
  | st ->
      Error (Format.asprintf "%a" Http.Status.pp st)
