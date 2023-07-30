(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs

let sha_1 s =
  (* Based on pseudo-code of RFC 3174. Slow and ugly but does the job.
     Cut and pasted from uuidm. *)
  let sha_1_pad s =
    let len = String.length s in
    let blen = 8 * len in
    let rem = len mod 64 in
    let mlen = if rem > 55 then len + 128 - rem else len + 64 - rem in
    let m = Bytes.create mlen in
    Bytes.blit_string s 0 m 0 len;
    Bytes.fill m len (mlen - len) '\x00';
    Bytes.set m len '\x80';
    if Sys.word_size > 32 then begin
      Bytes.set m (mlen - 8) (Char.unsafe_chr (blen lsr 56 land 0xFF));
      Bytes.set m (mlen - 7) (Char.unsafe_chr (blen lsr 48 land 0xFF));
      Bytes.set m (mlen - 6) (Char.unsafe_chr (blen lsr 40 land 0xFF));
      Bytes.set m (mlen - 5) (Char.unsafe_chr (blen lsr 32 land 0xFF));
    end;
    Bytes.set m (mlen - 4) (Char.unsafe_chr (blen lsr 24 land 0xFF));
    Bytes.set m (mlen - 3) (Char.unsafe_chr (blen lsr 16 land 0xFF));
    Bytes.set m (mlen - 2) (Char.unsafe_chr (blen lsr 8 land 0xFF));
    Bytes.set m (mlen - 1) (Char.unsafe_chr (blen land 0xFF));
    m
  in
  (* Operations on int32 *)
  let ( &&& ) = ( land ) in
  let ( lor ) = Int32.logor in
  let ( lxor ) = Int32.logxor in
  let ( land ) = Int32.logand in
  let ( ++ ) = Int32.add in
  let lnot = Int32.lognot in
  let sr = Int32.shift_right in
  let sl = Int32.shift_left in
  let cls n x = (sl x n) lor (Int32.shift_right_logical x (32 - n)) in
  (* Start *)
  let m = sha_1_pad s in
  let w = Array.make 16 0l in
  let h0 = ref 0x67452301l in
  let h1 = ref 0xEFCDAB89l in
  let h2 = ref 0x98BADCFEl in
  let h3 = ref 0x10325476l in
  let h4 = ref 0xC3D2E1F0l in
  let a = ref 0l in
  let b = ref 0l in
  let c = ref 0l in
  let d = ref 0l in
  let e = ref 0l in
  for i = 0 to ((Bytes.length m) / 64) - 1 do              (* For each block *)
    (* Fill w *)
    let base = i * 64 in
    for j = 0 to 15 do
      let k = base + (j * 4) in
      w.(j) <- sl (Int32.of_int (Char.code @@ Bytes.get m k)) 24 lor
               sl (Int32.of_int (Char.code @@ Bytes.get m (k + 1))) 16 lor
               sl (Int32.of_int (Char.code @@ Bytes.get m (k + 2))) 8 lor
               (Int32.of_int (Char.code @@ Bytes.get m (k + 3)))
    done;
    (* Loop *)
    a := !h0; b := !h1; c := !h2; d := !h3; e := !h4;
    for t = 0 to 79 do
      let f, k =
        if t <= 19 then (!b land !c) lor ((lnot !b) land !d), 0x5A827999l else
        if t <= 39 then !b lxor !c lxor !d, 0x6ED9EBA1l else
        if t <= 59 then
          (!b land !c) lor (!b land !d) lor (!c land !d), 0x8F1BBCDCl
        else
        !b lxor !c lxor !d, 0xCA62C1D6l
      in
      let s = t &&& 0xF in
      if (t >= 16) then begin
        w.(s) <- cls 1 begin
            w.((s + 13) &&& 0xF) lxor
            w.((s + 8) &&& 0xF) lxor
            w.((s + 2) &&& 0xF) lxor
            w.(s)
          end
      end;
      let temp = (cls 5 !a) ++ f ++ !e ++ w.(s) ++ k in
      e := !d;
      d := !c;
      c := cls 30 !b;
      b := !a;
      a := temp;
    done;
    (* Update *)
    h0 := !h0 ++ !a; h1 := !h1 ++ !b; h2 := !h2 ++ !c; h3 := !h3 ++ !d;
    h4 := !h4 ++ !e
  done;
  let h = Bytes.create 20 in
  let i2s h k i =
    Bytes.set h k (Char.unsafe_chr ((Int32.to_int (sr i 24)) &&& 0xFF));
    Bytes.set h (k + 1) (Char.unsafe_chr ((Int32.to_int (sr i 16)) &&& 0xFF));
    Bytes.set h (k + 2) (Char.unsafe_chr ((Int32.to_int (sr i 8)) &&& 0xFF));
    Bytes.set h (k + 3) (Char.unsafe_chr ((Int32.to_int i) &&& 0xFF));
  in
  i2s h 0 !h0; i2s h 4 !h1; i2s h 8 !h2; i2s h 12 !h3; i2s h 16 !h4;
  Bytes.unsafe_to_string h

(* Headers *)

let sec_websocket_accept = Http.Name.v "sec-websocket-accept"
let sec_websocket_extensions = Http.Name.v "sec-websocket-extensions"
let sec_websocket_key = Http.Name.v "sec-websocket-key"
let sec_websocket_protocol = Http.Name.v "sec-websocket-protocol"
let sec_websocket_version = Http.Name.v "sec-websocket-version"

(* Handshake *)

let accept_uuid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
let accept_key key = Http.Base64.encode (sha_1 (key ^ accept_uuid))

(* Upgrading *)

let upgradable hs =
  let connection = Http.Headers.find ~lowervalue:true Http.connection hs in
  let upgrade = Http.Headers.find ~lowervalue:true Http.upgrade hs in
  match connection, upgrade with
  | Some cn, Some up ->
      let upgrade = List.mem "upgrade" (Http.Headers.values_of_string cn) in
      let websocket = List.mem "websocket" (Http.Headers.values_of_string up) in
      websocket && upgrade
  | _, _ -> false

let err_no_key = "No sec-websocket-key header"
let err_no_version = "No sec-websocket-version header"
let err_unsupported_version v = "unsupported sec-websocket-version: " ^  v

let websocket_headers () =
  Http.Headers.(empty
          |> def Http.connection "Upgrade"
          |> def Http.upgrade "websocket")

let err_require_upgrade () =
  Http.Response.v
    Http.Status.upgrade_required_426 ~headers:(websocket_headers ())

let resp_websocket ?protocol key =
  let a = accept_key key in
  let hs = websocket_headers () in
  let hs = Http.Headers.def sec_websocket_accept a hs in
  let hs = match protocol with
  | None -> hs | Some p -> Http.Headers.def sec_websocket_protocol p hs
  in
  Http.Response.v Http.Status.switching_protocols_101 ~headers:hs

let upgrade req =
  let hs = Http.Request.headers req in
  if not (upgradable hs) then err_require_upgrade () else
  let key = Http.Headers.find sec_websocket_key hs in
  let protocols =
    Http.Headers.find ~lowervalue:true sec_websocket_protocol hs
  in
  let version = Http.Headers.find sec_websocket_version hs in
  match version with
  | None -> Http.Response.v Http.Status.bad_request_400 ~reason:err_no_version
  | Some s when s <> "13" ->
      (* RFC 6455 §4.2.2. 4 *)
      let hs = Http.Headers.(def sec_websocket_version "13" empty) in
      let reason = err_unsupported_version s in
      Http.Response.v Http.Status.upgrade_required_426 ~headers:hs ~reason
  | Some _ ->
      match key with
      | None -> Http.Response.v Http.Status.bad_request_400 ~reason:err_no_key
      | Some key ->
          match protocols with
          | None -> resp_websocket ?protocol:None key
          | Some e ->
              Http.Response.v Http.Status.bad_request_400
                ~reason:"unsupported protocol"
