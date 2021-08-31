(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs

let ( let* ) = Result.bind
let strf = Printf.sprintf

(* Unixies *)

let unix_buffer_size = 65536 (* UNIX_BUFFER_SIZE 4.0.0 *)
let uerror e = Unix.error_message e
let close_noerr fd = try Unix.close fd with Unix.Unix_error (e, _, _) -> ()

let rec lseek src off cmd = try Unix.lseek src off cmd with
| Unix.Unix_error (Unix.EINTR, _, _) -> lseek src off cmd

let rec openfile fn mode perm = try Unix.openfile fn mode perm with
| Unix.Unix_error (Unix.EINTR, _, _) -> openfile fn mode perm

let rec fstat fd = try Unix.fstat fd with
| Unix.Unix_error (Unix.EINTR, _, _) -> fstat fd

external realpath : string -> string = "ocaml_webs_realpath"

external sendfile :
  src:Unix.file_descr -> off:int -> len:int -> Unix.file_descr -> int =
  "ocaml_webs_sendfile"

let rec really_sendfile ~src ~off ~len dst =
  match sendfile ~src ~off ~len dst with
  | c when c = len -> ()
  | c -> really_sendfile ~src ~off:(off + c) ~len:(len - c) dst
  | exception Unix.Unix_error (Unix.EINTR, _, _) ->
      really_sendfile ~src ~off ~len dst

let copy_fd ?buf:b ~src ~off ~len dst = (* sendfile fallback *)
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

let really_really_send_file ~src ~off ~len dst =
  try really_sendfile ~src ~off ~len dst with
  | Sys_error _ (* sendfile unsupported *) -> copy_fd ~src ~off ~len dst

(* Connection *)

type Resp.connection += Fd of Unix.file_descr

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

(* Sending files *)

let http_date_of_ptime =
  let wday = [|"Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"|] in
  let month =
    [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct";
      "Nov"; "Dec"|]
  in
  fun ptime ->
  let t = Unix.gmtime ptime in
  let wday = wday.(t.tm_wday) in
  let month = month.(t.tm_mon) in
  strf "%s, %02d %s %04d %02d:%02d:%02d GMT"
    wday t.tm_mday month (t.tm_year + 1900) t.tm_hour t.tm_min t.tm_sec

type etagger =
  Http.fpath -> Unix.file_descr -> Unix.stats -> (Http.Etag.t, string) result

let default_etagger file fd stats =
  let mtime = truncate stats.Unix.st_mtime in
  let size = stats.Unix.st_size in
  let tag = strf "%x-%x" mtime size in
  Ok (Http.Etag.v ~weak:false tag)

type dir_resp =
  etagger:etagger -> mime_types:Http.Mime_type.file_ext_map option ->
  Req.t -> Http.fpath -> (Resp.t, Resp.t) result

let dir_404 ~etagger ~mime_types _ fpath =
  let explain = strf "%s: is a directory" fpath in
  Ok (Resp.v Http.not_found_404 ~explain)

let range_full ~file_size hs =
  let file_size' = Http.Digits.encode file_size in
  let hs = Http.Headers.(hs |> def Http.content_length file_size') in
  0, file_size - 1, hs, Http.ok_200

let range_partial ~file_size ~first ~last hs =
  let range_len = Http.Digits.encode (last - first + 1) in
  let crange =
    let file_size = Http.Digits.encode file_size in
    let first, last = Http.Digits.encode first, Http.Digits.encode last in
    String.concat "" ["bytes "; first; "-"; last; "/"; file_size]
  in
  let hs = Http.Headers.(hs |> def Http.content_range crange) in
  let hs = Http.Headers.(hs |> def Http.content_length range_len) in
  first, last, hs , Http.partial_content_206

let find_range req file etag ~file_size hs =
  let* r = Req.decode_header Http.range Http.Range.decode req in
  match r with
  | None -> Ok (range_full ~file_size hs)
  | Some (`Other _) (* ignore *) -> Ok (range_full ~file_size hs)
  | Some (`Bytes rs as r) ->
      let* iftag = Req.decode_header Http.if_range Http.Etag.decode req in
      let full = match iftag with
      | None -> false
      | Some t -> not (Http.Etag.eval_if_range t (Some etag))
      in
      if full then Ok (range_full ~file_size hs) else
      let rec loop = function
      | [] ->
          let reason = Http.Range.encode r and explain = file in
          Error (Resp.v Http.range_not_satisfiable_416 ~reason ~explain)
      | r :: rs ->
          match Http.Range.eval_bytes ~len:file_size r with
          | None -> loop rs
          | Some (first, last) -> Ok (range_partial ~file_size ~first ~last hs)
      in
      loop rs

let eval_etag_cond ~eval h req etag =
  let* c = Req.decode_header h Http.Etag.decode_cond req in
  match c with
  | None -> Ok true
  | Some c -> Ok (eval c (Some etag))

let check_if_match_cond r file etag =
  let eval = Http.Etag.eval_if_match in
  let* test = eval_etag_cond ~eval Http.if_match r etag in
  if test then Ok () else
  let explain =
    strf "%s: etag: %s, if-match failed" file (Http.Etag.encode etag)
  in
  Error (Resp.v Http.precondition_failed_412 ~explain)

let eval_if_none_match r file etag =
  let eval = Http.Etag.eval_if_none_match in
  eval_etag_cond ~eval Http.if_none_match r etag

let send_file
    ?(dir_resp = dir_404) ?(etagger = default_etagger) ?mime_types req file
  =
  let* is_head = match Req.meth req with
  | `GET -> Ok false | `HEAD -> Ok true
  | _ -> Resp.method_not_allowed_405 ~allowed:[`GET; `HEAD] ()
  in
  let error e = Resp.v Http.not_found_404 ~explain:(strf "%s: %s" file e) in
  try
    let file_fd = openfile file Unix.[O_RDONLY] 0 in
    try
      let stat = fstat file_fd in
      match stat.Unix.st_kind with
      | Unix.S_DIR ->
          close_noerr file_fd; dir_resp ~etagger ~mime_types req file
      | _ ->
          let explain = file in
          let* tag = Result.map_error error (etagger file file_fd stat) in
          let* () = check_if_match_cond req file tag in
          let* cond = eval_if_none_match req file tag in
          if not cond then Ok (Resp.v Http.not_modified_304 ~explain) else
          let file_size = stat.Unix.st_size in
          let file_type = Http.Mime_type.of_filepath ?map:mime_types file in
          let mtime = http_date_of_ptime stat.Unix.st_mtime in
          let hs =
            Http.Headers.(empty
                          |> def Http.content_type file_type
                          (* The following header affects memory cache in blink
                             based browsers, without it, it doesn't get hit. *)
                          |> def Http.last_modified mtime
                          |> def Http.etag (Http.Etag.encode tag)
                          |> def Http.accept_ranges "bytes")
          in
          if is_head then
            let len = Http.Digits.encode file_size in
            let headers = Http.Headers.(hs |> def Http.content_length len) in
            let body = Resp.empty_body in
            Ok (Resp.v Http.ok_200 ~headers ~body ~explain)
          else
          let* first, last, headers, status =
            find_range req file tag ~file_size hs
          in
          let body =
            let len = last - first + 1 in
            let send_file = function
            | Fd fd ->
                (* TODO can leak if we never get to consume the stream,
                   see connector implems *)
                let finally () = close_noerr file_fd in
                Fun.protect ~finally @@ fun () ->
                really_really_send_file ~src:file_fd ~off:first ~len fd
            | _ ->
                invalid_arg
                  "Webs_unix.send_file needs a Webs_unix.Fd connection"
            in
            Resp.direct_body send_file
          in
          Ok (Resp.v status ~headers ~body ~explain)
    with
    | Unix.Unix_error (e, _, _) -> close_noerr file_fd; Error (error (uerror e))
  with
  | Unix.Unix_error (e, _, _) -> Error (error (uerror e))

let dir_index_file file = match Http.Path.has_dir_seps file || file = ".." with
| true -> Error (strf "%s: invalid filename" file)
| false ->
    Ok (fun ~etagger ~mime_types r dir ->
        let file = Http.Path.prefix_filepath ~prefix:dir file in
        send_file ~dir_resp:dir_404 ~etagger ?mime_types r file)


module Time = struct
  type uint64 = int64

  (* Time spans

     Represented by a nanosecond magnitude stored in an unsigned 64-bit
     integer. Allows to represent spans for ~584.5 Julian years. *)

  type span = uint64
  module Span = struct

    (* Time spans *)

    type t = span
    let zero = 0L
    let one = 1L
    let max = -1L
    let add = Int64.add
    let abs_diff s0 s1 = match Int64.unsigned_compare s0 s1 < 0 with
    | true ->  Int64.sub s1 s0
    | false -> Int64.sub s0 s1

    (* Predicates and comparisons *)

    let equal = Int64.equal
    let compare = Int64.unsigned_compare

    (* Conversions *)

    let to_uint64_ns s = s
    let of_uint64_ns ns = ns
    let pp_ms ppf s = Format.fprintf ppf "%Lu" (Int64.unsigned_div s 1_000_000L)
    let pp_ns ppf s = Format.fprintf ppf "%Lu" s
  end

  (* Monotonic time counter *)

  type counter = uint64
  external now_ns : unit -> uint64 = "ocaml_webs_monotonic_now_ns"
  let counter = now_ns
  let count c = Int64.sub (now_ns ()) c
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
