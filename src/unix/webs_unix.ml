(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw
open Webs

let unix_buffer_size = 65536 (* UNIX_BUFFER_SIZE 4.0.0 *)

module Fd = struct
  type Http.Body.custom_content += Writer of Unix.file_descr Http.Body.writer

  (* Uninterrupted reads and writes *)

  let rec read fd b ~start:i ~length = try Unix.read fd b i length with
  | Unix.Unix_error (Unix.EINTR, _, _) -> read fd b ~start:i ~length

  let rec write fd s ~start:i ~length:l =
    let rec single_write fd s i l = try Unix.single_write fd s i l with
    | Unix.Unix_error (Unix.EINTR, _, _) -> single_write fd s i l
    in
    let bc = single_write fd s i l in
    if bc < l then write fd s ~start:(i + bc) ~length:(l - bc) else ()

  (* Body readers and writers *)

  let bytes_reader
      ~max_request_body_byte_size ~content_length fd b ~first_start ~first_len
    =
    (* FIXME use bytesrw limits ? *)
    let needs = match content_length with
    | None -> max_request_body_byte_size
    | Some len -> len
    in
    let needs = ref needs in
    let read () =
      if !needs <= 0 then Bytes.Slice.eod else
      let len = read fd b ~start:0 ~length:(Bytes.length b) in
      let length = Int.min len !needs in
      needs := !needs - length;
      if length = 0
      then Bytes.Slice.eod
      else Bytes.Slice.make b ~first:0 ~length
    in
    let reader = Bytes.Reader.make ~pos:first_len read in
    if first_len > 0 && !needs > 0 then begin
      let first = first_start in
      let length = Int.min first_len !needs in
      needs := !needs - first_len;
      Bytes.Reader.push_back reader (Bytes.Slice.make b ~first ~length)
    end;
    reader

  let rec write_stream_chunk fd = function
  | None -> () | Some (b, start, length) -> write fd b ~start ~length

  let rec bytes_reader_body_writer r fd = match Bytes.Reader.read r with
  | slice when Bytes.Slice.is_eod slice -> ()
  | slice ->
      let start = Bytes.Slice.first slice in
      let length = Bytes.Slice.length slice in
      let bytes = Bytes.Slice.bytes slice in
      write fd bytes ~start ~length; bytes_reader_body_writer r fd

  let body_writer b = match Http.Body.content b with
  | Empty -> fun fd -> ()
  | Byte_writer write -> fun fd -> write (write_stream_chunk fd)
  | Bytes_reader read -> bytes_reader_body_writer read
  | Custom (Writer w) -> w
  | Custom _ -> invalid_arg "Unknown custom body contents"

  (* HTTP/1.1 support *)

  let read_http11_head_crlfs ~max_bytes buf fd =
    let crlfs = ref [] and crlfcrlf = ref 0 in
    let i = ref 0 and free = ref max_bytes in
    (try
      while (!free > 0) do
        let length = Bytes.length buf - !i in
        match read fd buf ~start:!i ~length with
        | 0 -> raise Exit
        | c ->
            let j = !i in
            i := !i + c; free := !free - c;
            for k = j to j + c - 1 do
              let prev = k - 1 in
              if k > 0 && Bytes.get buf prev = '\r' && Bytes.get buf k = '\n'
              then match !crlfs with
              | last :: _ when last + 2 = prev -> crlfcrlf := last; raise Exit
              | crlfs' -> crlfs := prev :: crlfs'
            done
      done
    with Exit -> ());
    if !crlfcrlf = 0 || !crlfs = [] then failwith "" else
    let body_start = !crlfcrlf + 4 in
    let body_start_len = !i - body_start in
    List.rev !crlfs, body_start, body_start_len

  let write_http11_request fd request =
    let write_body = body_writer (Http.Request.body request) in
    let m = Http.Request.method' request in
    let request_target = Http.Request.raw_path request in
    let hs = Http.Request.headers request in
    let hs = Http.Headers.for_connector hs (Http.Request.body request) in
    let head =
      Http.Connector.Private.encode_http11_request_head m ~request_target hs
    in
    let head = Bytes.unsafe_of_string head and length = String.length head in
    write fd head ~start:0 ~length;
    write_body fd
end

(* HTTP-date *)

let http_date_of_posix_time_s =
  let wday = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |] in
  let month =
    [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct";
       "Nov"; "Dec" |]
  in
  fun ptime ->
    let t = Unix.gmtime ptime in
    let wday = wday.(t.tm_wday) in
    let month = month.(t.tm_mon) in
    Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT"
      wday t.tm_mday month (t.tm_year + 1900) t.tm_hour t.tm_min t.tm_sec

(* sendfile(2) *)

external sendfile :
  src:Unix.file_descr -> off:int -> length:int -> Unix.file_descr -> int =
  "ocaml_webs_sendfile"

let rec sendfile_handle_eintr ~src ~off ~length dst =
  match sendfile ~src ~off ~length dst with
  | c when c = length -> ()
  | c -> sendfile_handle_eintr ~src ~off:(off + c) ~length:(length - c) dst
  | exception Unix.Unix_error (Unix.EINTR, _, _) ->
      sendfile_handle_eintr ~src ~off ~length dst

let rec lseek src off cmd = try Unix.lseek src off cmd with
| Unix.Unix_error (Unix.EINTR, _, _) -> lseek src off cmd

let copy_fd ?buf:b ~src ~off ~length dst = (* sendfile fallback *)
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
  loop b src dst length

let really_sendfile ~src ~off ~length dst =
  try sendfile_handle_eintr ~src ~off ~length dst with
  | Sys_error _ (* sendfile unsupported *) -> copy_fd ~src ~off ~length dst

(* Monotonic time *)

module Time = struct
  type uint64 = int64

  (* Time spans are represented by a nanosecond magnitude stored in an
     unsigned 64-bit integer. Allows to represent spans for ~584.5
     Julian years. *)

  type span = uint64
  module Span = struct
    type t = span
    let zero = 0L
    let one = 1L
    let max = -1L
    let add = Int64.add
    let abs_diff s0 s1 = match Int64.unsigned_compare s0 s1 < 0 with
    | true ->  Int64.sub s1 s0
    | false -> Int64.sub s0 s1

    let equal = Int64.equal
    let compare = Int64.unsigned_compare

    let to_uint64_ns s = s
    let of_uint64_ns ns = ns
    let pp_ms ppf s = Format.fprintf ppf "%Lu" (Int64.unsigned_div s 1_000_000L)
    let pp_ns ppf s = Format.fprintf ppf "%Lu" s
  end

  type counter = uint64
  external now_ns : unit -> uint64 = "ocaml_webs_monotonic_now_ns"
  let counter = now_ns
  let count c = Int64.sub (now_ns ()) c
end
