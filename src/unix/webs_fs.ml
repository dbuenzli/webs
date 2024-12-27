(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs
let ( let* ) = Result.bind

let strf = Printf.sprintf
let uerror e = Unix.error_message e
let file_error_404 file e =
  let log = strf "%s: %s" file e in
  Error (Http.Response.empty Http.Status.not_found_404 ~log)

let rec openfile fn mode perm = try Unix.openfile fn mode perm with
| Unix.Unix_error (Unix.EINTR, _, _) -> openfile fn mode perm

let rec fstat fd = try Unix.fstat fd with
| Unix.Unix_error (Unix.EINTR, _, _) -> fstat fd

let close_noerr fd = try Unix.close fd with Unix.Unix_error (e, _, _) -> ()

let close_on_error fd = function
| Ok _ as v -> v | Error _ as e -> close_noerr fd; e

(* Etaggers *)

type etagger =
  Http.Path.fpath -> Unix.file_descr -> Unix.stats ->
  (Http.Etag.t, string) result

let default_etagger file fd stats =
  let mtime = truncate stats.Unix.st_mtime in
  let size = stats.Unix.st_size in
  let tag = strf "%x-%x" mtime size in
  Ok (Http.Etag.make ~weak:false tag)

type dir_response =
  etagger:etagger -> media_types:Media_type.of_file_ext_map option ->
  Http.Request.t -> Http.Path.fpath -> Unix.file_descr -> Unix.stats ->
  (Http.Response.t, Http.Response.t) result

let dir_404 ~etagger ~media_types _ fpath fd _ =
  close_noerr fd;
  let log = strf "%s is a directory" fpath in
  Http.Response.not_found_404 ~log ()

(* File sending *)

let range_full ~file_size resp_headers =
  let size = Http.Digits.encode file_size in
  let resp_headers = Http.Headers.(def content_length) size resp_headers in
  0, file_size - 1, resp_headers, Http.Status.ok_200

let range_partial ~file_size ~first ~last resp_headers =
  let range_len = Http.Digits.encode (last - first + 1) in
  let crange =
    let file_size = Http.Digits.encode file_size in
    let first, last = Http.Digits.encode first, Http.Digits.encode last in
    String.concat "" ["bytes "; first; "-"; last; "/"; file_size]
  in
  let resp_headers = Http.Headers.(def content_range) crange resp_headers in
  let resp_headers = Http.Headers.(def content_length) range_len resp_headers in
  first, last, resp_headers, Http.Status.partial_content_206

let find_range request file etag ~file_size resp_headers =
  let* range =
    Http.Request.decode_header Http.Headers.range Http.Range.decode request
  in
  match range with
  | None -> Ok (range_full ~file_size resp_headers)
  | Some (`Other _) (* ignore *) -> Ok (range_full ~file_size resp_headers)
  | Some (`Bytes rs as r) ->
      let* if_range =
        Http.Request.decode_header
          Http.Headers.if_range Http.Etag.decode request
      in
      let send_full = match if_range with
      | None -> true
      | Some t -> not (Http.Etag.eval_if_range t (Some etag))
      in
      if send_full then Ok (range_full ~file_size resp_headers) else
      let rec loop = function
      | [] ->
          let reason = Http.Range.encode r and log = file in
          let status = Http.Status.range_not_satisfiable_416 in
          Error (Http.Response.empty status ~reason ~log)
      | r :: rs ->
          match Http.Range.eval_bytes ~length:file_size r with
          | None -> loop rs
          | Some (first, last) ->
              Ok (range_partial ~file_size ~first ~last resp_headers)
      in
      loop rs

let check_if_match_cond request file etag =
  let* cond =
    Http.Request.decode_header
      Http.Headers.if_match Http.Etag.decode_cond request
  in
  let test = match cond with
  | None -> true | Some cond -> Http.Etag.eval_if_match cond (Some etag)
  in
  if test then Ok () else
  let etag = Http.Etag.encode etag in
  let log = strf "%s: etag: %s, if-match failed" file etag in
  Error (Http.Response.empty Http.Status.precondition_failed_412 ~log)

let test_if_none_match request file etag =
  let* cond =
    Http.Request.decode_header Http.Headers.if_none_match
      Http.Etag.decode_cond request
  in
  match cond with
  | None -> Ok true
  | Some cond -> Ok (Http.Etag.eval_if_none_match cond (Some etag))

let close_and_not_modified_304 file fd headers =
  close_noerr fd;
  Ok (Http.Response.empty ~headers Http.Status.not_modified_304 ~log:file)

let close_and_head_ok_200 ~file_size ~file_type file fd headers =
  let length = Http.Digits.encode file_size in
  let headers = Http.Headers.(def content_length) length headers in
  let headers = Http.Headers.(def content_type) file_type headers in
  close_noerr fd;
  Ok (Http.Response.empty ~log:file ~headers Http.Status.ok_200)

let get_ok_200 ~file_type ~first ~last ~headers status file fd =
  let body =
    let length = last - first + 1 in
    let send_file out_fd =
       (* TODO can leak if we never get to consume the stream,
          see connector implems *)
      let finally () = close_noerr fd in
      Fun.protect ~finally @@ fun () ->
      Webs_unix.really_sendfile ~src:fd ~off:first ~length out_fd
    in
    let content = Webs_unix.Fd.Writer send_file in
    let content_length = length and content_type = file_type in
    Http.Body.of_custom_content ~content_length ~content_type content
  in
  Ok (Http.Response.make ~headers ~log:file status body)

let file_response ~etagger ~media_types request method' file fd stat =
  close_on_error fd @@
  let* tag = match etagger file fd stat with
  | Error e -> file_error_404 file e | Ok _ as tag -> tag
  in
  let headers = Http.Headers.empty in
  let headers = Http.Headers.(def etag) (Http.Etag.encode tag) headers in
  let* () = check_if_match_cond request file tag in
  let* test = test_if_none_match request file tag in
  if not test then close_and_not_modified_304 file fd headers else
  let file_size = stat.Unix.st_size in
  let file_type = Media_type.of_filepath ?map:media_types file in
  let mtime = Webs_unix.http_date_of_posix_time_s stat.Unix.st_mtime in
  (* The [last_modified] header affects memory cache in blink based
     browsers, without it, it doesn't seem to get hit. *)
  let headers = Http.Headers.(def last_modified) mtime headers in
  let headers = Http.Headers.(def accept_ranges) "bytes" headers in
  match method' with
  | `HEAD -> close_and_head_ok_200 ~file_size ~file_type file fd headers
  | `GET ->
      let* first, last, headers, status =
        find_range request file tag ~file_size headers
      in
      get_ok_200 ~file_type ~first ~last ~headers status file fd

let send_file
    ?(dir_response = dir_404) ?(etagger = default_etagger) ?media_types
    request file
  =
  let* method' = Http.Request.allow Http.Method.[get; head] request in
  match openfile file Unix.[O_RDONLY] 0 with
  | exception Unix.Unix_error (e, _, _) -> file_error_404 file (uerror e)
  | fd ->
      (* Note: it's less racy if we start by opening the filepath and keep
         it open until the response has been written. *)
      match fstat fd with
      | exception Unix.Unix_error (e, _, _) ->
          close_noerr fd; file_error_404 file (uerror e)
      | stat ->
          if stat.Unix.st_kind = Unix.S_DIR
          then dir_response ~etagger ~media_types request file fd stat
          else file_response ~etagger ~media_types request method' file fd stat

(* Directory index files *)

let dir_index_file file =
  let send_index_file ~etagger ~media_types request dir fd stat =
    close_noerr fd;
    let file = Http.Path.prefix_filepath ~prefix:dir file in
    send_file ~dir_response:dir_404 ~etagger ?media_types request file
  in
  if Http.Path.has_dir_seps file || file = ".."
  then Error (strf "%s: invalid filename" file)
  else Ok send_index_file
