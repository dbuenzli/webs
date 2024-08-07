(*---------------------------------------------------------------------------
   Copyright (c) 2024 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax
open Webs

module Client = struct
  type t = Cmd.t

  let id _ = "curl"

  let curl_cli_headers hs =
    (* XXX Should we drop the Host: header ? *)
    let curl_cli_header n acc = function
    | "" -> (Http.Headers.Name.encode n ^ ";" (* cf man curl *)) :: acc
    | v -> (String.concat ": " [Http.Headers.Name.encode n; v]) :: acc
    in
    let encode_header n v acc =
      if not (Http.Headers.Name.equal n Http.Headers.set_cookie)
      then curl_cli_header n acc v else
      let vs = Http.Headers.values_of_set_cookie_value v in
      List.fold_left (curl_cli_header Http.Headers.set_cookie) acc vs
    in
    Http.Headers.fold encode_header hs []

  let response_of_curl_stdout s =
    let b = Bytes.unsafe_of_string s in
    match Http.Connector.Private.decode_http11_response b ~first:0 with
    | r -> Ok r | exception Failure e -> Error e

  let request curl request =
    let method' =
      let method' = Http.Request.method' request in
      let is_head = method' = `HEAD in
      let method' = Http.Method.encode method' in
      Cmd.(arg "-X" % method' %% if' is_head (arg "--head"))
    in
    let headers = Http.Request.headers request in
    let body = Http.Request.body request in
    let headers = Http.Headers.for_connector headers body in
    let headers = curl_cli_headers headers in
    let headers = Cmd.of_list ~slip:"-H" Fun.id headers in
    let has_body = not (Http.Body.is_empty body) in
    let* stdin =
      if has_body then Ok Os.Cmd.in_stdin else
      let* body = Http.Body.to_string body in
      Ok (Os.Cmd.in_string body)
    in
    let body = Cmd.(if' has_body (arg "--data-binary" % "@-")) in
    let* url = Http.Request.to_url request in
    let base = Cmd.(arg "-s" (* silent *) % "-i" (* resp. headers *)) in
    let args = Cmd.(base %% method' %% headers %% body % url) in
    let* out = Os.Cmd.run_out ~trim:false ~stdin Cmd.(curl %% args) in
    response_of_curl_stdout out
end

let default = Cmd.tool "curl"
let make ?search ?(cmd = default) ?(insecure = false) () =
  let* curl = Os.Cmd.get ?search cmd in
  let cmd = Cmd.(curl %% if' insecure (arg "--insecure")) in
  Ok (Http_client.make (module Client) cmd)
