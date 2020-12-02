(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs
let ( let* ) = Result.bind
let strf = Printf.sprintf

(* Connection *)

type Resp.connection += Fd of Unix.file_descr

(* Unixies *)

let unix_buffer_size = 65536 (* UNIX_BUFFER_SIZE 4.0.0 *)
let uerror e = Unix.error_message e
let close_noerr fd = try Unix.close fd with Unix.Unix_error (e, _, _) -> ()

let rec lseek src off cmd = try Unix.lseek src off cmd with
| Unix.Unix_error (Unix.EINTR, _, _) -> lseek src off cmd

let rec openfile fn mode perm = try Unix.openfile fn mode perm with
| Unix.Unix_error (Unix.EINTR, _, _) -> openfile fn mode perm

external realpath : string -> string = "ocaml_webs_realpath"
external sendfile :
  src:Unix.file_descr -> off:int -> len:int -> Unix.file_descr -> int =
  "ocaml_webs_sendfile"

(* Sending files *)

let rec really_sendfile ~src ~off ~len dst =
  match sendfile ~src ~off ~len dst with
  | c when c = len -> ()
  | c -> really_sendfile ~src ~off:(off + c) ~len:(len - c) dst
  | exception Unix.Unix_error (Unix.EINTR, _, _) ->
      really_sendfile ~src ~off ~len dst

let copy ?buf:b ~src ~off ~len dst = (* sendfile fallback *)
  let rec unix_read fd b len =
    try Unix.read fd b 0 (min len (Bytes.length b)) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> unix_read fd b len
  in
  let rec unix_write fd s i l =
    let rec write fd s i l = try Unix.single_write fd s i l with
    | Unix.Unix_error (Unix.EINTR, _, _) -> write fd s i l
    in
    let bc = write fd s i l in
    if bc < l then unix_write fd s (i + bc) (l - bc) else ()
  in
  let rec loop buf src dst len = match len with
  | 0 -> ()
  | len ->
      let rlen = unix_read src buf len in
      unix_write dst buf 0 rlen; loop buf src src (len - rlen)
  in
  let b = match b with Some b -> b | None -> Bytes.create unix_buffer_size in
  let () = ignore (lseek src off Unix.SEEK_SET) in
  loop b src dst len

let rec etag_of_file file fd =
  try
    let stat = Unix.fstat fd in
    let mtime = truncate stat.Unix.st_mtime in
    let size = stat.Unix.st_size in
    Ok (Some (Printf.sprintf {|"%x-%x"|} mtime size))
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> etag_of_file file fd
  | Unix.Unix_error (e, _, _) -> Error (uerror e)

let rec prepare_send_file ?(etag = etag_of_file) ?range file =
  let err e = `Error e in
  try
    let fd = Unix.openfile file Unix.[O_RDONLY] 0 in
    try
      let* etag = Result.map_error err (etag file fd) in
      (* TODO that dance looks ridiculus now that etag uses stat. *)
      let file_size = lseek fd 0 Unix.SEEK_END in
      let () = ignore (lseek fd 0 Unix.SEEK_SET) in
      let first, last = match range with
      | None -> 0, file_size - 1 | Some (f, l) -> f, l
      in
      if first < 0 || last < 0 || first > last || last >= file_size
      then Error `Invalid_range
      else Ok (fd, first, last - first + 1, file_size, etag)
    with
    | Unix.Unix_error (e, _, _) -> close_noerr fd; Error (err (uerror e))
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> prepare_send_file ~etag ?range file
  | Unix.Unix_error (Unix.EACCES, _, _) -> Error `Access
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Error `Not_found
  | Unix.Unix_error (e, _, _) -> Error (err (uerror e))

let raw_send_file ~src ~off ~len dst =
  try really_sendfile ~src ~off ~len dst with
  | Sys_error _ (* sendfile unsupported *) -> copy ~src ~off ~len dst

(* TODO replace the following by Webs_kit.Req_to.file_req *)

let bytes_range_of_string s = match String.index_opt s '=' with
| None -> None
| Some i ->
    if Http.string_subrange ~last:(i - 1) s <> "bytes" then None else
    let right = Http.string_subrange ~first:(i + 1) s in
    let _vs = Http.H.values_of_string right in
    failwith "TODO"

let file_spec_of_req ~docroot r = match Req.meth r with
| `GET | `HEAD ->
    begin match Req.path r with
    | [] -> Error (Resp.v Http.s400_bad_request ~reason:"empty path")
    | ps ->
        match Http.Path.to_undotted_filepath ps with
        | Error e -> Error (Resp.v Http.s400_bad_request ~reason:e)
        | Ok ps ->
            let file = Http.Path.prefix_filepath docroot ps in
            let range = match Http.H.(find range (Req.headers r)) with
            | None -> Ok None
            | Some v ->
                match bytes_range_of_string v with
                | None -> Ok None
                | Some v -> Ok (List.hd v)
            in
            match range with
            | Error e -> Error (Resp.v Http.s400_bad_request ~reason:e)
            | Ok range ->
                let st = match range with
                | None -> Http.s200_ok
                | Some _ -> Http.s206_partial_content
                in
                Ok (st, file, range)
    end
| _ -> Error (Resp.not_allowed ~allow:[`GET; `HEAD] ())

let send_file ?etag ~docroot r =
  let explain file e = strf "%s: %s" file e in
  let* st, file, range = file_spec_of_req ~docroot r in
  match prepare_send_file ?etag ?range file with
  | Error `Access ->
      let explain = explain file "permission denied" in
      Error (Resp.v ~explain Http.s404_not_found)
  | Error `Not_found ->
      let explain = explain file "no such file" in
      Error (Resp.v ~explain Http.s404_not_found)
  | Error `Invalid_range ->
      failwith "sendfile invalid-range err TODO"
  | Error (`Error err) ->
      let explain = explain file err in
      Error (Resp.v ~explain Http.s500_server_error)
  | Ok (src, off, len, file_len, etag_value) ->
      let file_type = Webs_kit.Mime_type.of_ext (Filename.extension file) in
      (* TODO handle range addition and content-length *)
      let headers = Http.H.(def content_type file_type empty) in
      let headers = Http.H.(def_if_some etag etag_value headers) in
      let send_file = function
      | Fd fd ->
          (* TODO can leak if we never get to consume the stream,
             see connector implems *)
          let finally () = close_noerr src in
          Fun.protect ~finally @@ fun () ->
          raw_send_file ~src ~off ~len fd
      | _ ->
          invalid_arg "Webs_unix.send_file needs a Webs_unix.Fd connection"
      in
      let body = Resp.direct_body send_file in
      Ok (Resp.v Http.s200_ok ~headers ~body)

(* Connection listeners *)

type listener =
[ `Host of string * int
| `Sockaddr of Unix.sockaddr
| `Fd of Unix.file_descr ]

let listener_localhost = `Host ("localhost", 8000)

let rec fd_of_listener = function
| `Fd fd -> Ok (fd, false)
| `Host (name, port) ->
    begin match Unix.gethostbyname name with
    | exception Not_found -> Error (name ^ ": host not found")
    | h -> fd_of_listener (`Sockaddr (Unix.ADDR_INET (h.h_addr_list.(0), port)))
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

let listener_of_string ?(default_port = 8000) s =
  if String.contains s Filename.dir_sep.[0]
  then Ok (`Sockaddr (Unix.ADDR_UNIX s))
  else match String.rindex_opt s ':' with
  | None -> Ok (`Host (s, default_port))
  | Some i ->
      match String.index_from_opt s i ']' with (* beware IPv6 addresses *)
      | Some _ -> Ok (`Host (s, default_port))
      | None ->
          let h = Http.string_subrange ~last:(i - 1) s in
          let p = Http.string_subrange ~first:(i + 1) s in
          match int_of_string p with
          | exception Failure _ -> Error (strf "port %S not an integer" p)
          | p -> Ok (`Host (h, p))

let pp_listener ppf l =
  let pp_np ppf (n,p) = Format.fprintf ppf "%s:%d" n p in
  match l with
  | `Host (h, n) -> pp_np ppf (h, n)
  | `Fd fd -> Format.pp_print_string ppf "<fd>"
  | `Sockaddr (Unix.ADDR_UNIX s) -> Format.pp_print_string ppf s
  | `Sockaddr (Unix.ADDR_INET (a, p)) ->
      pp_np ppf (Unix.string_of_inet_addr a, p)


module Connector = struct

  (* Reading requests *)

  let rec read fd b ~start:i ~len = try Unix.read fd b i len with
  | Unix.Unix_error (Unix.EINTR, _, _) -> read fd b ~start:i ~len

  let req_body_reader
      ~max_req_body_byte_size ~body_length fd b ~first_start ~first_len
    =
    let body = ref (fun () -> assert false) in
    let needs = match body_length with
    | None -> max_req_body_byte_size
    | Some len -> len
    in
    let needs = ref needs in (* we'll need this exposed for keep-alive *)
    let last () = None in
    let return b start len = match !needs with
    | n when n <= 0 -> body := last; None
    | n ->
        needs := n - len;
        if !needs <= 0 then body := last;
        Some (b, start, min len n)
    in
    let fill () =
      let len = read fd b 0 (Bytes.length b) in
      return b 0 len
    in
    let first () = body := fill; return b first_start first_len in
    body := first; fun () -> !body ()

  (* Writing responses *)

  let rec write fd s ~start:i ~len:l =
    let rec single_write fd s i l = try Unix.single_write fd s i l with
    | Unix.Unix_error (Unix.EINTR, _, _) -> single_write fd s i l
    in
    let bc = single_write fd s i l in
    if bc < l then write fd s ~start:(i + bc) ~len:(l - bc) else ()

  (* TODO we need to figure out what to do with the yielder
     when write fails and with the pull when read fails.
     Maybe we should signal by returning something. *)

  let rec write_stream_chunk fd = function
  | None -> () | Some (b, start, len) -> write fd b ~start ~len

  let resp_body_writer resp = match Resp.body resp with
  | Resp.Empty -> resp, fun fd -> ()
  | Resp.Stream f -> resp, fun fd -> f (write_stream_chunk fd)
  | Resp.Direct f -> resp, fun fd -> f (Fd fd)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
