(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs

let strf = Printf.sprintf

module Sha_256 = struct
  type t = string
  let length h = String.length h
  external hash : string -> string = "ocaml_webs_sha_256"
  let hmac ~key msg = (* see https://tools.ietf.org/html/rfc2104 *)
    let hmac_B = 64 in
    let pad_and_xor ~key ~xor =
      let key_len = String.length key in
      let key = Bytes.unsafe_of_string key in
      let b = Bytes.create hmac_B in
      for i = 0 to hmac_B - 1 do
        let d = if i < key_len then Bytes.get_uint8 key i else 0 in
        Bytes.set_uint8 b i (d lxor xor)
      done;
      Bytes.unsafe_to_string b
    in
    let key = if String.length key > hmac_B then hash key else key in
    let key_xor_opad = pad_and_xor ~key ~xor:0x5c in
    let key_xor_ipad = pad_and_xor ~key ~xor:0x36 in
    hash (key_xor_opad ^ (hash (key_xor_ipad ^ msg)))

  type salt = string
  type key = string

  let random_salt ?random_state:(r = Random.State.make_self_init()) ~length () =
    String.init length (fun _ -> Char.chr (Random.State.int r 256))

  let pbkdf2_hmac ~key_length ~iterations ~salt ~password () =
    let check_positive s v =
      if v <= 0 then invalid_arg (strf "%s not positive (%d)" s v)
    in
    check_positive "key_len" key_length;
    check_positive "iterations" iterations;
    let div_round_up x y = (x + y - 1) / y in
    let max = Int64.(mul (sub (shift_left 1L 32) 1L) 32L) in
    match Int64.unsigned_to_int max with
    | None -> invalid_arg (strf "key_len too long (%d)" key_length)
    | Some _ ->
        (* Note: hmac allocs could be avoided, let's go with that for now. *)
        let hlen = 32 in
        let l = div_round_up key_length hlen in
        let t = Bytes.create (l * hlen) in
        let salt_len = String.length salt in
        let u0 =
          let u0 = Bytes.create (salt_len + 4) in
          Bytes.blit_string salt 0 u0 0 (String.length salt); u0
        in
        let init_u0 i = Bytes.set_int32_be u0 salt_len (Int32.of_int i) in
        let uj = Bytes.create hlen in
        for i = 1 to l do
          init_u0 i;
          let u1 = hmac ~key:password (Bytes.unsafe_to_string u0) in
          let ti_start = (i - 1) * hlen in
          Bytes.blit_string u1 0 t ti_start hlen;
          Bytes.blit_string u1 0 uj 0 hlen;
          for j = 2 to iterations do
            let unext = hmac ~key:password (Bytes.unsafe_to_string uj) in
            Bytes.blit_string unext 0 uj 0 hlen;
            for k = 0 to hlen - 1 do
              Bytes.set_uint8 t (ti_start + k)
                (Bytes.get_uint8 t (ti_start + k) lxor
                 Bytes.get_uint8 (Bytes.unsafe_of_string unext) k)
            done
          done
        done;
        if key_length = Bytes.length t
        then Bytes.unsafe_to_string t
        else Bytes.sub_string t 0 key_length

  let equal_key k0 k1 =
    (* Constant time compare for keys of the same length. Not sure about
       that maybe we should rather defer to C here. *)
    Sys.opaque_identity @@
    let err_key_size k0l k1l = strf "key length differ (%d and %d)" k0l k1l in
    let k0l = String.length k0 in
    let k1l = String.length k1 in
    if k0l <> k1l then invalid_arg (err_key_size k0l k1l) else
    let[@inline] byte s i = Char.code (String.unsafe_get s i) in
    let c = ref 0 in
    for i = 0 to k0l - 1 do c := !c lor (byte k0 i lxor byte k1 i) done;
    (!c = 0)

  let equal = equal_key

  (* Conversions *)

  let to_binary_string = Fun.id
  let of_binary_string s = if String.length s <> 32 then Error () else Ok s
  let to_hex h =
    let[@inline] hex_digit n =
      Char.unsafe_chr (if n < 10 then 0x30 + n else 0x57 + n)
    in
    let rec loop max h i hex k =
      if i > max then Bytes.unsafe_to_string hex else
      let byte = Char.code h.[i] in
      Bytes.set hex k (hex_digit (byte lsr 4));
      Bytes.set hex (k + 1) (hex_digit (byte land 0xF));
      loop max h (i + 1) hex (k + 2)
    in
    let len = String.length h in
    let hex = Bytes.create (2 * len) in
    loop (len - 1) h 0 hex 0

  let of_hex hex =
    let exception Illegal of int in
    let hex_value s i = match s.[i] with
    | '0' .. '9' as c -> Char.code c - 0x30
    | 'A' .. 'F' as c -> 10 + (Char.code c - 0x41)
    | 'a' .. 'f' as c -> 10 + (Char.code c - 0x61)
    | _ -> raise_notrace (Illegal i)
    in
    match String.length hex with
    | len when len <> 64 -> Error len
    | len ->
        let rec loop max h i hex k = match i > max with
        | true -> Ok (Bytes.unsafe_to_string h)
        | false ->
            let hi = hex_value hex k and lo = hex_value hex (k + 1) in
            Bytes.set h i (Char.chr @@ (hi lsl 4) lor lo);
            loop max h (i + 1) hex (k + 2)
        in
        let s_len = len / 2 in
        let s = Bytes.create s_len in
        try loop (s_len - 1) s 0 hex 0 with Illegal i -> Error i

  let of_hex' h = match of_hex h with
  | Ok _ as v -> v
  | Error i ->
      match i = String.length h with
      | true -> Error "Hash not made of 64 ASCII hexadecimal digits"
      | false -> Error (strf "Byte %d: not an ASCII hexadecimal digit" i)

  let pp ppf h = Format.pp_print_string ppf (to_hex h)
end
