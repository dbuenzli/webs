(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs

let ( let* ) = Result.bind
let strf = Printf.sprintf

type time = int
module Private_key = struct
  type t = [ `Hs256 of string ]

  let random_hs256 ?random_state:(r = Random.State.make_self_init ()) () =
    `Hs256 (String.init 64 (fun _ -> Char.chr (Random.State.int r 256)))

  let to_ascii_string = function
  | `Hs256 k -> "HS256:" ^ (Http.Base64.url_encode k)

  let of_ascii_string s = match String.index_opt s ':' with
  | None -> Error (strf "missing ':' separator")
  | Some i ->
      let scheme = Http.Private.string_subrange ~last:(i - 1) s in
      let d = Http.Private.string_subrange ~first:(i + 1) s in
      match scheme with
      | "HS256" ->
          let* k = Http.Base64.url_decode d |> Http.Base64.error_string in
          Ok (`Hs256 k)
      | s -> Error (strf "unknown scheme: %S" scheme)
end

let hs256 = "HS256"

(* Authenticatable *)

type t = string

(* Encode *)

let encode ~private_key:(`Hs256 key) ~expire data =
  let expire = match expire with None -> "" | Some t -> string_of_int t in
  let msg = String.concat ":" [expire; data] in
  let hmac = Webs_hash.Sha_256.hmac ~key msg in
  let hmac = Webs_hash.Sha_256.to_binary_string hmac in
  let hmac = String.concat ":" [hs256; hmac] in
  Http.Base64.url_encode (hmac ^ msg)

(* Decode *)

type format_error =
[ `Base64url of Http.Base64.error
| `Scheme of string option ]

let format_error_message = function
| `Scheme None -> "scheme decode error"
| `Scheme (Some ("HS256" as s)) -> strf "scheme %s decode error" s
| `Scheme (Some s) -> strf "unknown scheme %S" s
| `Base64url e -> Http.Base64.error_message e

type error =
[ `Authentication
| `Expired of time
| `Missing_now_for of time
| `Format of format_error ]

let error_message = function
| `Authentication -> "data not authenticated by private key"
| `Format e -> format_error_message e
| `Expired t -> strf "data expired at %d" t
| `Missing_now_for t -> strf "missing current time for data expiring at %d." t

let error_string r = Result.map_error error_message r

let decode_hmac s = match Http.Base64.url_decode s with
| Error e -> Error (`Base64url e)
| Ok s ->
    if String.length s < 6 then Error (`Scheme None) else
    let algo = Http.Private.string_subrange ~last:4 s in
    let is_hs256 = String.equal algo hs256 && s.[5] = ':' in
    if not is_hs256 then Error (`Scheme (Some algo)) else
    if String.length s < 39 then Error (`Scheme (Some hs256)) else
    let hmac = String.sub s 6 32 (* Length of sha-256 hashes *) in
    let hmac = Webs_hash.Sha_256.of_binary_string hmac |> Result.get_ok in
    let msg = Http.Private.string_subrange ~first:38 s in
    Ok (hmac, msg)

let decode_msg msg = match String.index_opt msg ':' with
| None -> Error (`Scheme (Some hs256))
| Some i ->
    let expire = Http.Private.string_subrange ~last:(i - 1) msg in
    let data = Http.Private.string_subrange ~first:(i + 1) msg in
    if String.equal expire "" then Ok (None, data) else
    match int_of_string_opt expire with
    | None -> Error (`Scheme (Some hs256))
    | Some _ as t -> Ok (t, data)

let decode ~private_key:(`Hs256 key) ~now s = match decode_hmac s with
| Error e -> Error (`Format e)
| Ok (hmac, msg) ->
    let hmac' = Webs_hash.Sha_256.hmac ~key msg in
    if not (Webs_hash.Sha_256.equal hmac hmac') then Error `Authentication else
    match decode_msg msg, now with
    | Error e, _ -> Error (`Format e)
    | Ok (Some t, data) as r, Some now when now < t -> r
    | Ok (Some t, data), Some _ -> Error (`Expired t)
    | Ok (Some t, data), None -> Error (`Missing_now_for t)
    | Ok (None, data) as r, _ -> r

(* Untrusted decode *)

type untrusted =
  [`Untrusted_hs256 of Webs_hash.Sha_256.t * time option * string ]

let untrusted_decode s =
  let* hmac, msg = decode_hmac s in
  let* expire, data = decode_msg msg in
  Ok (`Untrusted_hs256 (hmac, expire, data))
