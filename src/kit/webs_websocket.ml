(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Result.Syntax
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
  for i = 0 to ((Bytes.length m) / 64) - 1 do (* For each block *)
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

(* Keys *)

type key = string
let accept_uuid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
let accept_header_value_of_key key =
  Webs_base64.encode `Padded (sha_1 (key ^ accept_uuid))

let random_key ?(crypto_random = Webs_crypto_random.get) () =
  Webs_base64.encode `Padded (crypto_random 16)

(* Headers *)

let sec_websocket_accept = Http.Headers.name "sec-websocket-accept"
let sec_websocket_extensions = Http.Headers.name "sec-websocket-extensions"
let sec_websocket_key = Http.Headers.name "sec-websocket-key"
let sec_websocket_protocol = Http.Headers.name "sec-websocket-protocol"
let sec_websocket_version = Http.Headers.name "sec-websocket-version"

(* Upgrading *)

let err_no_key = "No sec-websocket-key header"
let err_no_version = "No sec-websocket-version header"
let err_unsupported_version v = "unsupported sec-websocket-version: " ^  v

let add_request_upgrade_headers ?(key = random_key ()) headers =
  key,
  headers
  |> Http.Headers.def Http.Headers.upgrade "websocket"
  |> Http.Headers.def Http.Headers.connection "upgrade"
  |> Http.Headers.def sec_websocket_key key
  |> Http.Headers.def sec_websocket_version "13"

let request_upgrade_of_url
    ?key ?(headers = Http.Headers.empty) ?log ?version url
  =
  let key, headers = add_request_upgrade_headers ?key headers in
  let* request = Http.Request.of_url ?log ?version ~headers `GET ~url in
  Ok (key, request)

let headers_upgradable headers =
  let connection = Http.Headers.(find ~lowervalue:true connection) headers in
  let upgrade = Http.Headers.(find ~lowervalue:true upgrade) headers in
  match connection, upgrade with
  | Some connection, Some upgrade ->
      List.mem "upgrade" (Http.Headers.values_of_string connection) &&
      List.mem "websocket" (Http.Headers.values_of_string upgrade)
  | _, _ -> false

let request_upgradable request =
  headers_upgradable (Http.Request.headers request)

let upgrade_required_426 ?reason ~headers () =
  let status = Http.Status.upgrade_required_426 in
  Error (Http.Response.empty status ~headers ?reason)

let check_version ~headers =
  match Http.Headers.find sec_websocket_version headers with
  | None -> Http.Response.bad_request_400 ~reason:err_no_version ()
  | Some "13" -> Ok ()
  | Some v ->
      (* RFC 6455 §4.2.2. 4 *)
      let headers = Http.Headers.empty in
      let headers = Http.Headers.def sec_websocket_version "13" headers in
      let reason = err_unsupported_version v in
      upgrade_required_426 ~headers ~reason ()

let upgrade_response request =
  let websocket_headers () =
    Http.Headers.empty
    |> Http.Headers.(def connection) "upgrade"
    |> Http.Headers.(def upgrade) "websocket"
    in
    let headers = Http.Request.headers request in
    if not (headers_upgradable headers)
    then upgrade_required_426 ~headers:(websocket_headers ()) () else
    let* () = check_version ~headers in
    match Http.Headers.find sec_websocket_key headers with
    | None -> Http.Response.bad_request_400 ~reason:err_no_key ()
    | Some key ->
        let accept = accept_header_value_of_key key in
        let headers = websocket_headers () in
        let headers = Http.Headers.def sec_websocket_accept accept headers in
        Ok (Http.Response.empty Http.Status.switching_protocols_101 ~headers)
