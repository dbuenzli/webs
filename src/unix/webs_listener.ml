(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs
let ( let* ) = Result.bind

let uerror e = Unix.error_message e
let close_noerr fd = try Unix.close fd with Unix.Unix_error (e, _, _) -> ()

(* Listeners *)

type t =
[ `Host of string * int
| `Sockaddr of Unix.sockaddr
| `Fd of Unix.file_descr ]

let localhost_8000 = `Host ("localhost", 8000)

let rec to_fd = function
| `Fd fd -> Ok (fd, false)
| `Host (name, port) ->
    begin match Unix.gethostbyname name with
    | exception Not_found -> Error (name ^ ": host not found")
    | host -> to_fd (`Sockaddr (Unix.ADDR_INET (host.h_addr_list.(0), port)))
    end
| `Sockaddr addr ->
    let domain = Unix.domain_of_sockaddr addr in
    match Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 with
    | exception Unix.Unix_error (e, _, _) -> Error (uerror e)
    | fd ->
        try
          Unix.setsockopt fd Unix.SO_REUSEADDR true;
          Unix.bind fd addr;
          Ok (fd, true)
        with
        | Unix.Unix_error (e, _, _) -> close_noerr fd; Error (uerror e)

let of_string ~default_port s =
  let is_path s = String.contains s Filename.dir_sep.[0] in
  if is_path s then Ok (`Sockaddr (Unix.ADDR_UNIX s)) else
  match String.rindex_opt s ':' with
  | None -> Ok (`Host (s, default_port))
  | Some i ->
      match String.index_from_opt s i ']' with (* beware IPv6 addresses *)
      | Some _ -> Ok (`Host (s, default_port))
      | None ->
          let host = Http.Private.string_subrange ~last:(i - 1) s in
          let port = Http.Private.string_subrange ~first:(i + 1) s in
          match int_of_string port with
          | port -> Ok (`Host (host, port))
          | exception Failure _ ->
              Error (Printf.sprintf "port %S not an integer" port)

let pp_host ppf (host, port) = Format.fprintf ppf "%s:%d" host port
let pp ppf = function
| `Host (host, port) -> pp_host ppf (host, port)
| `Fd fd -> Format.pp_print_string ppf "<fd>"
| `Sockaddr (Unix.ADDR_UNIX s) -> Format.pp_print_string ppf s
| `Sockaddr (Unix.ADDR_INET (addr, port)) ->
    pp_host ppf (Unix.string_of_inet_addr addr, port)
