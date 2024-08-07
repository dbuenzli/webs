(*---------------------------------------------------------------------------
   Copyright (c) 2024 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs

let ( let* ) = Result.bind

type tool = string

let tool_exists =
  (* Maybe we should rather reimplement PATH lookup. Wish for B0_std… *)
  let strf = Printf.sprintf in
  let exists_win32 tool =
    (* `where` does not support full path lookups *)
    if String.equal (Filename.basename tool) tool
    then (Sys.command (strf "where %s 1> NUL 2> NUL" tool) = 0)
    else Sys.file_exists tool
  in
  let exists_posix tool =
    Sys.command (strf "command -v %s 1>/dev/null 2>/dev/null" tool) = 0
  in
  let exists = if Sys.win32 then exists_win32 else exists_posix in
  fun tool ->
    try
      if exists tool then Ok () else
      Error (strf "%s: No such file or not found in PATH" tool)
    with
    | Sys_error e -> Error (strf "%s: %s" tool e)

module Curl = struct
  type t =
    { tool : tool;
      cmd : string list;
      trace : (int -> string array -> unit) }

  let id curl = curl.tool

  let cli_headers hs =
    let curl_cli_header n acc v =
      let h = match v with
      | "" -> Http.Headers.Name.encode n ^ ";" (* cf man curl *)
      | v -> (String.concat ": " [Http.Headers.Name.encode n; v])
      in
      "-H" :: h :: acc
    in
    let encode_header n v acc =
      if not (Http.Headers.Name.equal n Http.Headers.set_cookie)
      then curl_cli_header n acc v else
      let vs = Http.Headers.values_of_set_cookie_value v in
      List.fold_left (curl_cli_header Http.Headers.set_cookie) acc vs
    in
    Http.Headers.fold encode_header hs []

  let cli curl request =
    let method' =
      let method' = Http.Request.method' request in
      let is_head = method' = `HEAD in
      let method' = Http.Method.encode method' in
      ("-X" :: method' :: if is_head then ["--head"] else []);
    in
    let headers = Http.Request.headers request in
    let body = Http.Request.body request in
    let has_body = not (Http.Body.is_empty body) in
    let* body_data =
      if has_body then Result.map Option.some (Http.Body.to_string body)
      else Ok None
    in
    let headers = Http.Headers.for_connector headers body in
    let headers = cli_headers headers in
    let body = if has_body then ["--data-binary"; "@-"] else [] in
    let* url = Http.Request.to_url request in
    let base = ["-i" (* response headers *)] in
    let cmd = [curl.cmd; base; method'; headers; body; [url]] in
    let cmd = List.concat cmd in
    Ok (cmd, body_data)

  let response_of_curl_stdout s =
    let b = Bytes.unsafe_of_string s in
    match Http.Connector.Private.decode_http11_response b ~first:0 with
    | r -> Ok r | exception Failure e -> Error e

  let request curl request =
    let err e = Error (Printf.sprintf "%s spawn: %s" curl.tool e) in
    let* cmd, body_data = cli curl request in
    let cmd = Array.of_list cmd in
    try
      (* TODO better body handling with bytesrw *)
      let pout, pin as p = Unix.open_process_args curl.tool cmd in
      try
        let pid = Unix.process_pid p in
        let () = curl.trace pid cmd in
        begin match body_data with
        | None -> () | Some b ->
            Out_channel.output_string pin b;
            Out_channel.flush pin;
        end;
        Out_channel.close_noerr pin;
        let out = In_channel.input_all pout in
        In_channel.close_noerr pout;
        match Unix.close_process p with
        | Unix.WEXITED 0 -> response_of_curl_stdout out
        | Unix.WEXITED n -> err (Printf.sprintf "exited with %d" n)
        | Unix.WSIGNALED n -> err (Printf.sprintf "signaled with %d" n)
        | Unix.WSTOPPED n -> err (Printf.sprintf "stopped with %d" n)
      with
      | Sys_error e ->
          ignore (Unix.close_process p); err e
      | Unix.Unix_error (e, _, _) ->
          ignore (Unix.close_process p); err (Unix.error_message e)
    with
    | Unix.Unix_error (e, _, _) ->
        let () = curl.trace (-1) cmd in
        err (Unix.error_message e)

  let make ~trace ~insecure tool args =
    let* () = tool_exists tool in
    let cmd = tool :: if insecure then ("--insecure" :: args) else args in
    Ok { tool; cmd; trace }
end

type cmd = [ `Curl of tool * string list ]
let default_cmd = `Curl ("curl", ["-s" (* silent *) ])

let make ?(trace = fun _ _ -> ()) ?(cmd = default_cmd) ?(insecure = false) () =
  match cmd with
  | `Curl (tool, args) ->
      let* c = Curl.make ~trace ~insecure tool args in
      Ok (Http_client.make (module Curl) c)

(* Tracing *)

let pp_trace ppf (pid, args) =
  let pp_arg ppf s = Format.fprintf ppf "%s" (Filename.quote s) in
  let pp_sep = Format.pp_print_space in
  Format.fprintf ppf "@[[EXEC:%d] @[<h>[%a]@]@]@."
    pid (Format.pp_print_iter ~pp_sep Array.iter pp_arg) args

let stderr_tracer pid args =
  let exec = Filename.basename Sys.executable_name in
  Format.eprintf "%s: %a" exec pp_trace (pid, args)
