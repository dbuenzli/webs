(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs

let strf = Printf.sprintf
let ( let* ) = Result.bind

module Gateway = struct
  let send_file ~header _ file =
    let headers = Http.H.(def header file empty) in
    Ok (Resp.v Http.s200_ok ~headers ~explain:(header :> string))

  let x_accel_redirect = Http.Name.v "x-accel-redirect"
  let x_sendfile = Http.Name.v "x-sendfile"
end

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

  let equal = String.equal
  let compare = String.compare
  let to_bytes = Fun.id
  let of_bytes s = if String.length s <> 32 then Error () else Ok s
  let to_hex h =
    let[@inline] hex_digit n =
      Char.unsafe_chr (if n < 10 then 0x30 + n else 0x57 + n)
    in
    let rec loop max h i hex k = match i > max with
    | true -> Bytes.unsafe_to_string hex
    | false ->
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

  let pp ppf h = Format.pp_print_string ppf (to_hex h)

  let pbkdf2_hmac ~key_len ~iterations ~pass ~salt () =
    let check_positive s v =
      if v <= 0 then invalid_arg (strf "%s not positive (%d)" s v)
    in
    check_positive "key_len" key_len;
    check_positive "iterations" iterations;
    let div_round_up x y = (x + y - 1) / y in
    let max = Int64.(mul (sub (shift_left 1L 32) 1L) 32L) in
    match Int64.unsigned_to_int max with
    | None -> invalid_arg (strf "key_len too long (%d)" key_len)
    | Some _ ->
        (* TODO avoid the hmac allocs *)
        let hlen = 32 in
        let l = div_round_up key_len hlen in
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
          let u1 = hmac ~key:pass (Bytes.unsafe_to_string u0) in
          let ti_start = (i - 1) * hlen in
          Bytes.blit_string u1 0 t ti_start hlen;
          Bytes.blit_string u1 0 uj 0 hlen;
          for j = 2 to iterations do
            let unext = hmac ~key:pass (Bytes.unsafe_to_string uj) in
            Bytes.blit_string unext 0 uj 0 hlen;
            for k = 0 to hlen - 1 do
              Bytes.set_uint8 t (ti_start + k)
                (Bytes.get_uint8 t (ti_start + k) lxor
                 Bytes.get_uint8 (Bytes.unsafe_of_string unext) k)
            done
          done
        done;
        if key_len = Bytes.length t
        then Bytes.unsafe_to_string t
        else Bytes.sub_string t 0 key_len
end

module Authenticatable = struct
  type time = int
  type private_key = string
  let random_private_key () =
    let () = Random.self_init () in
    let b = Bytes.create 64 in
    for i = 0 to 63 do Bytes.set_uint8 b i (Random.int 256) done;
    Bytes.unsafe_to_string b

  type t = string
  let encode ?(base64url = false) ~private_key ~expire:e data =
    let e = match e with None -> "" | Some t -> string_of_int t in
    let msg = String.concat ":" [e; data] in
    let hmac = Sha_256.hmac ~key:private_key msg in
    Http.Base64.encode  ~url:base64url (hmac ^ msg)

  let decode_hmac ?(base64url = false) s =
    match Http.Base64.decode ~url:base64url s with
    | Error _ -> Error `Decode
    | Ok s ->
        if String.length s < 33 then Error `Decode else
        let hmac = Http.string_subrange ~last:31 s in
        let msg = Http.string_subrange ~first:32 s in
        Ok (hmac, msg)

  let decode_msg msg = match String.index_opt msg ':' with
  | None -> Error `Decode
  | Some i ->
      let expire = Http.string_subrange ~last:(i - 1) msg in
      let data = Http.string_subrange ~first:(i + 1) msg in
      match expire with
      | "" -> Ok (None, data)
      | e ->
          match int_of_string expire with
          | exception Failure _ -> Error `Decode
          | t -> Ok (Some t, data)

  let decode ?base64url ~private_key ~now s =
    let* hmac, msg = decode_hmac ?base64url s in
    let hmac' = Sha_256.hmac ~key:private_key msg in
    if not (Sha_256.equal hmac hmac') then Error `Authentication else
    match decode_msg msg with
    | Error _ as e -> e
    | Ok (None, data) as r -> r
    | Ok (Some t, data) as r when now < t -> r
    | Ok (Some _, data) -> Error `Expired

  let untrusted_decode ?base64url s =
    let* hmac, msg = decode_hmac ?base64url s in
    let* expire, data = decode_msg msg in
    Ok (`Untrusted (hmac, expire, data))
end

module Authenticated_cookie = struct
  let cookies hs = match Http.H.(find cookie hs) with (* TODO lazy in Req ? *)
  | None -> Ok []
  | Some s -> Http.Cookie.decode_list  s

  let get ~private_key ~now ~name req = match cookies (Req.headers req) with
  | Error _ -> None
  | Ok cookies ->
      match List.assoc_opt name cookies with
      | None -> None
      | Some cookie ->
          match Authenticatable.decode ~private_key ~now cookie with
          | Ok (_, data) -> (Some data)
          | Error _ -> None

  let set ~private_key ~expire ?atts ~name data resp =
    let value = Authenticatable.encode ~private_key ~expire data in
    let cookie = Http.Cookie.encode ?atts ~name value in
    let hs = Http.H.add_set_cookie cookie (Resp.headers resp) in
    Resp.with_headers hs resp
end

module Session = struct
  type 'a state =
    { eq : 'a -> 'a -> bool;
      encode : 'a -> string;
      decode : string -> ('a, string) result; }

  let[@inline] state ~eq ~encode ~decode () = { eq; encode; decode }

  module State = struct
    let string =
      state ~eq:String.equal ~encode:Fun.id ~decode:Result.ok ()

    let int =
      let encode i = string_of_int i in
      let decode s = Option.to_result ~none:"not an int" (int_of_string_opt s)in
      state ~eq:Int.equal ~encode ~decode ()

    let pair s0 s1 = (* TODO this is of course broken *)
      let encode (v0, v1) = String.concat "\x00" [s0.encode v0; s1.encode v1] in
      let decode s = match String.index_opt s '\x00' with
      | None -> Error "can't decode pair"
      | Some i ->
          let* v0 = s0.decode (Http.string_subrange ~last:(i - 1) s) in
          let* v1 = s1.decode (Http.string_subrange ~first:(i + 1) s) in
          Ok (v0, v1)
      in
      let eq (v0, v1) (v0', v1') = s0.eq v0 v0' && s1.eq v1 v1' in
      state ~eq ~encode ~decode ()
  end

  type 'a handler =
    { load : 'a state -> Req.t -> 'a option;
      save : 'a state -> 'a option -> Resp.t -> Resp.t }

  let handler ~load ~save () = { load; save }

  let eq_state st s0 s1 = match s0, s1 with
  | Some s0, Some s1 -> st.eq s0 s1
  | None, None -> true
  | _ -> false

  type 'a resp = 'a option * Resp.t
  type nonrec 'a result = ('a resp, 'a resp) result

  let setup st handler service =
    fun req ->
    let s = handler.load st req in
    let s', resp = service s req in
    if eq_state st s s' then resp else handler.save st s' resp

  let for_result st = function Ok v -> Ok (st, v) | Error e -> Error (st, e)
  let for_ok st = function Ok v -> Ok (st, v) | Error _ as e -> e
  let for_error st = function Ok _ as v -> v | Error e -> Error (st, e)

  let with_authenticated_cookie ~private_key ?atts ~name () =
    let now = 0 (* TODO get rid of that *) in
    let load st req =
      match Authenticated_cookie.get ~private_key ~now ~name req with
      | None -> None
      | Some "" (* TODO sort that out correctly *) -> None
      | Some s -> Result.to_option (st.decode s)
    in
    let save st s resp =
      let data = match s with
      | None -> (* TODO sort that out correctly *) ""
      | Some s -> st.encode s
      in
      Authenticated_cookie.set ?atts ~private_key ~expire:None ~name data resp
    in
    handler ~load ~save ()
end

(* Basic authentication *)

module Basic_auth = struct
  type user = string
  type check =
    user:user -> pass:string -> (unit, [`User_unknown | `Wrong_password]) result

  let basic_authentication_of_string creds =
    match Http.H.values_of_string ~sep:' ' creds with
    | scheme :: cred :: _ ->
        begin match Http.string_lowercase scheme = "basic" with
        | false -> Error (strf "auth-scheme %s: unsupported" scheme)
        | true ->
            let* user_pass =
              Result.map_error (Fun.const "base64 decode error") @@
              Http.Base64.decode cred
            in
            match String.index_opt user_pass ':' with
            | None -> Error "no ':' found in basic credentials"
            | Some i ->
                let user = Http.string_subrange ~last:(i - 1) user_pass in
                let pass = Http.string_subrange ~first:(i + 1) user_pass in
                Ok (user, pass)
        end
    | _ -> Error ("Not a basic auth-scheme")


  let cancel = Resp.html Http.s401_unauthorized @@
{|<!DOCTYPE html>
<html lang="en">
  <head><meta charset="utf-8"><title>Login cancelled</title></head>
  <body><h1>Login cancelled</h1><a href="">Try again.</a></body>
</html>|}

  let enticate ?(cancel = fun _ -> cancel) ~check ~realm r =
    let error_401 ~explain =
      let auth = Printf.sprintf {|basic realm="%s", charset="utf-8"|} realm in
      let hs = Http.H.(def www_authenticate auth empty) in
      let resp = Resp.with_status ~explain Http.s401_unauthorized (cancel r) in
      Error (Resp.override_headers hs resp)
    in
    match Http.H.(find authorization (Req.headers r)) with
    | None -> error_401 ~explain:"No authorization header"
    | Some creds ->
        let* user, pass = match basic_authentication_of_string creds with
        | Ok _ as v -> v
        | Error explain -> Error (Resp.v ~explain Http.s400_bad_request)
        in
        let* () = match check ~user ~pass with
        | Ok _ as v -> v
        | Error e ->
            let explain = match e with
            | `User_unknown -> strf "user %s: unknown" user
            | `Wrong_password -> strf "user %s: wrong password" user
            in
            error_401 ~explain
        in
        Ok (user, r)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers

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
