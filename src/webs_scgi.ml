(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult
open Astring
open Webs



(* Netstring parsing *)

let really_input fd b start len = failwith "TODO"
(*
  if len <= 0 then () else
  try match read fd b start len with
  | 0 -> failwith err_ns_malformed
  | r -> really_read fd b (start + r) (len - r)
  with Unix_error (Unix.EINTR, _, _) -> really_input fd b start len
*)

let input_char fd b = really_input fd b 0 1 (* TODO very ineffecient. *)


let input_ns_len fd = failwith "TODO"
(*
      let b = "0" in
      let c = input_char fd b in
      if c = '0' then (if input_char ic <> ':'
*)

let input_netstring ic = failwith "TODO"
(*
  let length =
    let c = input_char ic in
    if c = '0' then
      (if input_char ic <> ':' then failwith err_ns_malformed else 0)
    else
    let i = ref ((Char.code c) - 0x30) in
    try
      while true do
        let c = input_char ic in
        if c = ':' then raise Exit else
        if c < '0' || c > '9' then failwith err_ns_malformed;
        i := !i * 10 + ((Char.code c) - 0x30);
      done;
      assert false
    with Exit ->
      if !i > Sys.max_string_length then failwith err_ns_size else
      !i
  in
  let s =
    if length = 0 then "" else
    let buf = String.create length in
    really_input ic buf 0 length;
    buf
  in
  if (input_char ic) <> ',' then failwith err_ns_malformed else
  s
*)

let get_fd c = failwith "TODO"
(*
  let listen = match Conf.find Conf.listen with
  | None -> Conf.default_listen | Some l -> l
  in
  match listen with
  | `Fd fd -> fd
  | `Addr saddr ->
      let dom = Unix.domain_of_sockaddr saddr in
      let fd = Unix.socket dom Unix.SOCK_STREAM 0 in
      Unix.setsockopt fd Unix.SO_REUSEADDR true;
      Unix.bind fd saddr
*)

(** TODO set signals to avoid sigint on shutdown *)

let connect c service =
  let continue = ref true in
  let fd = get_fd c in
  let rec accept fd = try Unix.accept fd with
  | Unix.Unix_error (Unix.EINTR, _, _) -> accept fd
  in
  try
    Unix.listen fd (-1); (* TODO *)
    while !continue do
      let fd, _ = accept fd in
      ignore (fd);
    done;
	  Ok ()
  with
  | Failure f -> Error (`Webserver (R.msg f))
  | Unix.Unix_error (e, f, v) ->
	    let v = if v = "" then "" else strf " on %s" v in
	   Error (`Connector (R.msg (strf "%s%s: %s" f v (Unix.error_message e))))



(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

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
