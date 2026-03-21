(*---------------------------------------------------------------------------
   Copyright (c) 2025 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Result.Syntax

let strf = Format.asprintf
let pp_string_as_base64url ppf s =
  Format.pp_print_string ppf (Webs_base64.encode_base64url `Unpadded s)

let base64url_jsont =
  let dec = Jsont.Base.dec_result (Webs_base64.decode_base64url `Unpadded) in
  let enc = Webs_base64.encode_base64url `Unpadded in
  Jsont.Base.string (Jsont.Base.map ~kind:"base64url:unpadded" ~dec ~enc ())

let pp_hex ppf k =
  let pp_char ppf c = Format.fprintf ppf "%02x" (Char.code c) in
  let pp_sep ppf () = Format.pp_print_char ppf ':' in
  Format.fprintf ppf "%a" (Format.pp_print_iter ~pp_sep String.iter pp_char) k

(* Challenges *)

module Challenge = struct
  module Validator = struct
    module Challenge_map = Map.Make (String)
    type crypto_random = int -> string
    type time_s = int
    type now_s = unit -> time_s
    let ptime_now () = truncate (Unix.gettimeofday ())

    let prune_expired ~now map =
      let keep_valid _ (_, expires) = (expires : time_s) > (now : time_s) in
      Challenge_map.filter keep_valid map

    let ensure_free_slot ~max ~now map =
      if Challenge_map.cardinal map < max then map else
      let map = prune_expired ~now map in
      if Challenge_map.cardinal map < max then map else
      (* Drop an arbitrary one *)
      Challenge_map.remove (fst (Challenge_map.min_binding map)) map

    type 'a t =
      { lock : Mutex.t;
        max : int;
        challenge_byte_size : int;
        challenge_validity_s : int;
        now_s : now_s;
        crypto_random : crypto_random;
        (* challenges map to their expiration time and payload *)
        mutable challenges : ('a * time_s) Challenge_map.t; }

    let err_max max = strf "Invalid max %d, must be positive" max
    let err_size s = strf "Challenge byte size %d, must be >= 16" s
    let err_dur d = strf  "Challenge validity duration %d, must positive" d

    let make
        ?(max = 1024) ?(challenge_byte_size = 64)
        ?(challenge_validity_s = 300) ?(now_s = ptime_now)
        ?(crypto_random = Webs_cryptorand.get_random) ()
      =
      if max <= 0 then invalid_arg (err_max max);
      if challenge_byte_size < 16
      then invalid_arg (err_size challenge_byte_size);
      if challenge_validity_s <= 0
      then invalid_arg (err_dur challenge_validity_s);
      { lock = Mutex.create (); max; challenge_byte_size; challenge_validity_s;
        now_s; crypto_random; challenges = Challenge_map.empty; }

    let max v = v.max
    let challenge_byte_size v = v.challenge_byte_size
    let challenge_validity_s v = v.challenge_validity_s
    let pending v =
      Mutex.protect v.lock @@ fun () ->
      v.challenges <- prune_expired ~now:(v.now_s ()) v.challenges;
      Challenge_map.cardinal v.challenges

    let invalidate_pending v =
      Mutex.protect v.lock @@ fun () ->
      v.challenges <- Challenge_map.empty

    let make_challenge ?validity_s v ~payload =
      let validity_s = match validity_s with
      | Some dur -> dur | None -> v.challenge_validity_s
      in
      if validity_s < 0 then invalid_arg (err_dur validity_s) else
      let now = v.now_s () in
      let challenge = v.crypto_random v.challenge_byte_size in
      let expires = now + validity_s in
      Mutex.protect v.lock @@ fun () ->
      let challenges = ensure_free_slot ~max:v.max ~now v.challenges in
      v.challenges <- Challenge_map.add challenge (payload, expires) challenges;
      challenge

    let validate_challenge v c =
      Mutex.protect v.lock @@ fun () ->
      match Challenge_map.find_opt c v.challenges with
      | None -> Error ()
      | Some (payload, expires) ->
          v.challenges <- Challenge_map.remove c v.challenges;
          if expires > v.now_s () then Ok payload else Error ()
  end

  type t = string
  let make = Validator.make_challenge
  let validate = Validator.validate_challenge
  let equal = String.equal
  let compare = String.compare
  let of_binary_string = Fun.id
  let to_binary_string = Fun.id
  let pp = pp_string_as_base64url
end

(* Errors *)

type error =
| Invalid_relying_party_origin of { found : string; expected : string }
| Invalid_relying_party_hash of { found : Bytesrw_crypto.Sha_256.t;
                                  expected : Bytesrw_crypto.Sha_256.t }
| Invalid_client_data_type of { found : string; expected : string }
| Invalid_challenge of Challenge.t
| Invalid_signature of { signature : string (* binary *); msg : string }

let error_message = function
| Invalid_relying_party_origin { found; expected } ->
    strf "Invalid relying party %s in origin, expected %s" found expected
| Invalid_relying_party_hash { found; expected } ->
    strf "Invliad relying party hash %a in authenticator data, expected %a"
      Bytesrw_crypto.Sha_256.pp found Bytesrw_crypto.Sha_256.pp expected
| Invalid_client_data_type { found; expected } ->
    strf "Invalid client data type %s, expected %s" found expected
| Invalid_challenge c ->
    strf "Invalid challenge %a" Challenge.pp c
| Invalid_signature { signature; msg } ->
    strf "Invalid signature %a in attestation (%s)" pp_hex signature msg

module Aaguid = struct
  (* https://github.com/passkeydeveloper/passkey-authenticator-aaguids *)
  type t = string (* 16 bytes *)
  let nil = String.make 16 '\x00'
  let equal = String.equal
  let compare = String.compare
  let of_binary_string s =
    let len = String.length s in
    if len = 16 then Ok s else
    Error (strf "aaguid: Invalid length, found %d expected 16" len)

  let to_binary_string = Fun.id
  let to_ascii_uuid u = (* c&p from uuidm *)
    let hbase = 0x57 in
    let hex hbase i =
      Char.unsafe_chr (if i < 10 then 0x30 + i else hbase + i)
    in
    let s = Bytes.of_string "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX" in
    let i = ref 0 in
    let j = ref 0 in
    let byte s i c =
      Bytes.set s i @@ hex hbase (c lsr 4);
      Bytes.set s (i + 1) @@ hex hbase (c land 0x0F)
    in
    while (!j < 4) do byte s !i (Char.code u.[!j]); i := !i + 2; incr j; done;
    incr i;
    while (!j < 6) do byte s !i (Char.code u.[!j]); i := !i + 2; incr j; done;
    incr i;
    while (!j < 8) do byte s !i (Char.code u.[!j]); i := !i + 2; incr j; done;
    incr i;
    while (!j < 10) do byte s !i (Char.code u.[!j]); i := !i + 2; incr j; done;
    incr i;
    while (!j < 16) do byte s !i (Char.code u.[!j]); i := !i + 2; incr j; done;
    Bytes.unsafe_to_string s

  let pp ppf u = Format.pp_print_string ppf (to_ascii_uuid u)
end

module Credential_id = struct
  type t = string
  let nil = String.make 16 '\x00'
  let equal = String.equal
  let compare = String.compare
  let to_binary_string = Fun.id
  let of_binary_string s =
    let len = String.length s in
    if 16 <= len && len <= 1023 then Ok s else
    Error (Printf.sprintf "credential ID: Invalid length, found %d \
                           expected between 16 and 1023" len)

  let pp ppf c =
    Format.pp_print_string ppf (Webs_base64.encode_base64url `Unpadded c)

  let jsont =
    let dec meta s = match Webs_base64.decode_base64url `Unpadded s with
    | Error e -> Jsont.Error.msg Jsont.Meta.none e
    | Ok id ->
        match of_binary_string id with
        | Ok id -> id
        | Error e -> Jsont.Error.msg Jsont.Meta.none e
    in
    let enc = Webs_base64.encode_base64url `Unpadded in
    Jsont.Base.string (Jsont.Base.map ~kind:"credential_id" ~dec ~enc ())
end

module Public_key = struct
  type algorithm = Es256 | Ed25519
  let algorithm_to_string = function Es256 -> "ES256" | Ed25519 -> "Ed25519"
  let algorithm_jsont =
    let dec meta f = match f with
    | -7.0 -> Es256
    | -8.0 -> Ed25519
    | n -> Jsont.Error.msgf Jsont.Meta.none "Unknown public key algorithm: %g" n
    in
    let enc = function Es256 -> -7. | Ed25519 -> -8. in
    let kind = "public-key-algorithm"in
    Jsont.Base.number (Jsont.Base.map ~kind ~dec ~enc ())

  type t = { algorithm : algorithm; raw_value : string }

  let make algorithm ~raw_value = { algorithm; raw_value }
  let algorithm k = k.algorithm
  let raw_value k = k.raw_value

  let key_data_for_psa k = match k.algorithm with
  | Es256 ->
      (* FIXME do a little bit more checking basically the prefix should
         always be constant because the key are mandated to be uncompressed
         so the bytes we chunk out are the byte 0x04 followed by 32 bytes big
         endian [x] and 32 bytes big endian [y] which is the format PSA expects.
         The bytes before should be an ASN.1 SujectPublicKeyInfo structure
         that describes the key itself.
         3059 3013 0607 2a8648ce3d0201 0608 2a8648ce3d030107 0342 00
         30 59 SEQUENCE and length
         30 13 SEQUENCE and length
         06 07 2a8648ce3d0201    OID 1.2.840.10045.2.1   (ECC)
         06 08 2a8648ce3d030107  OID 1.2.840.10045.3.1.7 (secp256r1)
         03 42 BIT STRING and length
         00 Number of unused bits in the final byte of the bit string. *)
      (* last 65 bytes for now *)
      Ok (String.sub k.raw_value (String.length k.raw_value - 65) 65)
  | Ed25519 ->
      (* We need https://github.com/Mbed-TLS/mbedtls/issues/3757 *)
      Error "Unimplemented"

  module Psa = Bytesrw_crypto.Psa

  let make_psa_key alg key_data =
    Result.map_error (fun e -> strf "pub-key: %s" (Psa.Status.message e)) @@
    let alg, key_type_pub, key_bits, key_data = match alg with
    | Es256 ->
        let alg = Psa.Alg.(ecdsa sha_256) in
        let ecc_family = Psa.Ecc_family.secp_r1 in
        let key_type_pub = Psa.Key_type.ecc_public_key ecc_family in
        let key_bits = 256 in
        alg, key_type_pub, key_bits, key_data
    | Ed25519 ->
        let alg = Psa.Alg.ed25519ph in
        let ecc_family = Psa.Ecc_family.twisted_edwards in
        let key_type_pub = Psa.Key_type.ecc_public_key ecc_family in
        let key_bits = 255 (* Curve25519 *) in
        alg, key_type_pub, key_bits, key_data
    in
    let attrs = Psa.Key_attributes.init () in
    let () = Psa.set_key_usage_flags attrs Psa.Key_usage.verify_message in
    let () = Psa.set_key_algorithm attrs alg in
    let () = Psa.set_key_type attrs key_type_pub in
    let () = Psa.set_key_bits attrs key_bits in
    let key_data = Bytesrw_crypto.Bigbytes.of_string key_data in
    let* key = Psa.import_key attrs key_data in
    Ok (key, alg)

  let extract_signature_for_psa k signature =
    let asn1_ecdsa_signature_r_and_s signature =
      (* We decode an
           Ecdsa-Sig-Value  ::=  SEQUENCE  {
           r     INTEGER,
           s     INTEGER  }
           The first two bytes denote the sequence and sequence length then
           30 45 SEQUENCE and LENGTH
           20 XX …   INTEGER LENGTH and data
           20 XX …   INTEGER LENGTH and data *)
      let decode_int_at n =
        (* FIXME check the 0x20 byte at [n-1] *)
        let len = Char.code signature.[n] in
        let i = String.sub signature (n + 1) len in
        let i =
          (* Maybe unpad or FIXME should we always unpad if we see a zero ? *)
          if Char.code i.[0] = 0x00 &&
             Char.code i.[1] land 0x80 <> 0 then String.sub i 1 (len - 1)
          else i
        in
        i, len
      in
      let r, len = decode_int_at 3 in
      let s, _ = decode_int_at (4 + len + 1) in
      r ^ s
    in
    (* See https://w3c.github.io/webauthn/#sctn-signature-attestation-types *)
    match k.algorithm with
    | Es256 -> Ok (asn1_ecdsa_signature_r_and_s signature)
    | Ed25519 ->
        (* We need https://github.com/Mbed-TLS/mbedtls/issues/3757 *)
        Error "Unimplemented"

  let verify_signature k ~signature data =
    Result.map_error (fun msg -> Invalid_signature { signature; msg }) @@
    (* FIXME do the extraction before in the system, so that the data structures
       can simply be used. Want to keep the decoding errors away from
       that logic. *)
    let* signature = extract_signature_for_psa k signature in
    let* key_data = key_data_for_psa k in
    let* key, alg = make_psa_key k.algorithm key_data in
    Fun.protect ~finally:(fun () -> ignore (Psa.destroy_key key)) @@ fun () ->
    let signature = Bytesrw.Bytes.Slice.of_string signature in
    let input = Bytesrw.Bytes.Slice.of_string data in
    let st = Psa.Sign.verify_message key alg ~input ~signature in
    if Psa.is_success st then Ok () else Error (Psa.Status.message st)

  let pp ppf k =
    let algo = algorithm_to_string k.algorithm in
    Format.fprintf ppf "%s:%a" algo pp_hex k.raw_value
end

module Passkey = struct
  type t =
    { aaguid : Aaguid.t;
      credential_id : Credential_id.t;
      public_key : Public_key.t; }

  let aaguid pk = pk.aaguid
  let credential_id pk = pk.credential_id
  let public_key pk = pk.public_key

  let make ~aaguid ~credential_id ~public_key () =
    { aaguid; credential_id; public_key }

  let of_public_key public_key =
    { aaguid = Aaguid.nil; credential_id = Credential_id.nil; public_key }

  let pp ppf pk =
    Format.fprintf ppf
      "@[<v>{aaguid = %a@,credential_id = %a@,public_key = %a}@]"
      Aaguid.pp pk.aaguid Credential_id.pp pk.credential_id
      Public_key.pp pk.public_key
end

module Client_data = struct
  type t =
    { type' : string;
      challenge : Challenge.t;
      origin : string; }

  let make type' challenge origin =
    let challenge = Challenge.of_binary_string challenge in
    { type'; challenge; origin }

  let jsont =
    Jsont.Object.map make
    |> Jsont.Object.mem "type" Jsont.string
    |> Jsont.Object.mem "challenge" base64url_jsont
    |> Jsont.Object.mem "origin" Jsont.string
    |> Jsont.Object.finish

  let decode json = match Jsont_bytesrw.decode_string jsont json with
  | Ok v -> v | Error e -> Jsont.Error.msg Jsont.Meta.none e

  let pp ppf cd =
    Format.fprintf ppf "@[<v>{type = %s@,challenge = %a@,origin = %s}"
      cd.type' Challenge.pp cd.challenge cd.origin

  let verify_type ~type':expected cd =
    if String.equal cd.type' expected then Ok () else
    Error (Invalid_client_data_type { found = cd.type'; expected })

  let relying_party_id_of_origin origin = match Webs.Url.authority origin with
  | None -> None
  | Some authority -> Some (Webs.Url.Authority.host authority)

  let verify_origin ~rpid:expected cd =
    let err ~found ~expected =
      Error (Invalid_relying_party_origin {found; expected})
    in
    match relying_party_id_of_origin cd.origin with
    | None -> err ~found:cd.origin ~expected
    | Some found ->
        if String.equal found expected then Ok () else err ~found ~expected
end

module Authenticator_data = struct
  (* See https://w3c.github.io/webauthn/#fig-attStructs *)
  type t = string

  let to_binary_string = Fun.id
  let of_binary_string s =
    let len = String.length s in
    if len >= 37 then Ok s else
    Error (strf "Authenticator data: Invalid length, found %d bytes \
                 expected at least 37" len)

  let rpid_hash a =
    Bytesrw_crypto.Sha_256.of_binary_string (String.sub a 0 32) |> Result.get_ok

  let aaguid a =
    if String.length a < 53 then Aaguid.nil else
    Aaguid.of_binary_string (String.sub a 37 16) |> Result.get_ok

  let jsont =
    let dec meta s = match Webs_base64.decode_base64url `Unpadded s with
    | Error e -> Jsont.Error.msg Jsont.Meta.none e
    | Ok id ->
        match of_binary_string id with
        | Ok a -> a
        | Error e -> Jsont.Error.msg Jsont.Meta.none e
    in
    let enc = Webs_base64.encode_base64url `Unpadded in
    Jsont.Base.string (Jsont.Base.map ~kind:"Authenticator data" ~dec ~enc ())

  let verify_rpid_hash ~rpid_hash:expected a =
    let found = rpid_hash a in
    if Bytesrw_crypto.Sha_256.equal found expected then Ok () else
    Error (Invalid_relying_party_hash { found; expected })
end

module Registration = struct
  type t =
    { attestation_object : string;
      authenticator_data : Authenticator_data.t;
      client_data : Client_data.t;
      passkey : Passkey.t; }

  let make
      attestation_object authenticator_data client_data_json algo key_value
      credential_id
    =
    let client_data = Client_data.decode client_data_json in
    let public_key = Public_key.make algo ~raw_value:key_value in
    let aaguid = Authenticator_data.aaguid authenticator_data in
    let passkey = Passkey.make ~aaguid ~credential_id ~public_key () in
    { attestation_object; authenticator_data; client_data; passkey }

  let authenticator_data r = r.authenticator_data

  let public_key_credential_attestation_jsont =
    let authenticator_attestation_response_jsont =
      Jsont.Object.map make
      |> Jsont.Object.mem "attestationObject" base64url_jsont
      |> Jsont.Object.mem "authenticatorData" Authenticator_data.jsont
      |> Jsont.Object.mem "clientDataJSON" base64url_jsont
      |> Jsont.Object.mem "publicKeyAlgorithm" Public_key.algorithm_jsont
      |> Jsont.Object.mem "publicKey" base64url_jsont
      |> Jsont.Object.finish
    in
    Jsont.Object.map (fun id k -> k id)
    |> Jsont.Object.mem "id" Credential_id.jsont
    |> Jsont.Object.mem "response" authenticator_attestation_response_jsont
    |> Jsont.Object.finish

  let of_public_key_credential_json json =
    Jsont_bytesrw.decode_string public_key_credential_attestation_jsont json

  let pp ppf r =
    Format.fprintf ppf
      "@[<v>{attestation_object = %a@,authenticator_data = %a@,\
       client_data = @[%a@]@,passkey = %a}"
      pp_string_as_base64url r.attestation_object
      pp_string_as_base64url r.authenticator_data
      Client_data.pp r.client_data Passkey.pp r.passkey
end

module Assertion = struct
  type t =
    { authenticator_data : Authenticator_data.t;
      client_data : Client_data.t;
      client_data_json : string;
      credential_id : Credential_id.t;
      signature : string;
      user_handle : string; }

  let make
      authenticator_data client_data_json signature user_handle credential_id
    =
    let client_data = Client_data.decode client_data_json in
    { authenticator_data; client_data; client_data_json; signature;
      user_handle; credential_id }

  let authenticator_data a = a.authenticator_data
  let client_data a = a.client_data
  let credential_id a = a.credential_id
  let signature a = a.signature

  let signed_data a =
    let json_hash = Bytesrw_crypto.Sha_256.string a.client_data_json in
    Authenticator_data.to_binary_string a.authenticator_data ^
    Bytesrw_crypto.Sha_256.to_binary_string json_hash

  let public_key_credential_assertion_jsont : t Jsont.t =
    let authenticator_attestation_response_jsont =
      Jsont.Object.map make
      |> Jsont.Object.mem "authenticatorData" Authenticator_data.jsont
      |> Jsont.Object.mem "clientDataJSON" base64url_jsont
      |> Jsont.Object.mem "signature" base64url_jsont
      |> Jsont.Object.mem "userHandle" base64url_jsont
      |> Jsont.Object.finish
    in
    Jsont.Object.map (fun id k -> k id)
    |> Jsont.Object.mem "id" Credential_id.jsont
    |> Jsont.Object.mem "response" authenticator_attestation_response_jsont
    |> Jsont.Object.finish

  let of_public_key_credential_json json =
    Jsont_bytesrw.decode_string public_key_credential_assertion_jsont json

  let pp ppf r =
    Format.fprintf ppf
      "@[<v>{authenticator_data = %a@,client_data = @[%a@]@,\
       credential_id = %a@,signature = %a@,user_handle = %a}"
      pp_string_as_base64url r.authenticator_data
      Client_data.pp r.client_data
      Credential_id.pp r.credential_id
      pp_string_as_base64url r.signature pp_string_as_base64url r.user_handle
end

module Relying_party = struct
  type id = string
  type 'a t =
    { challenge_validator : 'a Challenge.Validator.t;
      id : id;
      id_sha256 : Bytesrw_crypto.Sha_256.t; }

  let make ?(challenge_validator = Challenge.Validator.make ()) ~id () =
    let id_sha256 = Bytesrw_crypto.Sha_256.string id in
    { challenge_validator; id; id_sha256 }

  let challenge ?validity_s rp ~payload =
    Challenge.make ?validity_s rp.challenge_validator ~payload

  let validate_challenge rp c =
    match Challenge.validate rp.challenge_validator c with
    | Ok _ as v -> v
    | Error () -> Error (Invalid_challenge c)
end

let challenge = Relying_party.challenge

let register (rp : 'a Relying_party.t) r =
  let cd = r.Registration.client_data in
  let* () = Client_data.verify_type ~type':"webauthn.create" cd in
  let* () = Client_data.verify_origin ~rpid:rp.id cd in
  let ad = r.Registration.authenticator_data in
  let* () = Authenticator_data.verify_rpid_hash ~rpid_hash:rp.id_sha256 ad in
  let* v = Relying_party.validate_challenge rp cd.challenge in
  Ok (v, r.Registration.passkey)

let verify (rp : 'a Relying_party.t) pk a =
  let client_data = Assertion.client_data a in
  let* () = Client_data.verify_type ~type':"webauthn.get" client_data in
  let* () = Client_data.verify_origin ~rpid:rp.id client_data in
  let ad = Assertion.authenticator_data a in
  let* () = Authenticator_data.verify_rpid_hash ~rpid_hash:rp.id_sha256 ad in
  let pubkey = Passkey.public_key pk in
  let signature = Assertion.signature a and data = Assertion.signed_data a in
  let* () = Public_key.verify_signature pubkey ~signature data in
  let* v = Relying_party.validate_challenge rp client_data.challenge in
  Ok v
