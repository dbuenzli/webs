(*---------------------------------------------------------------------------
   Copyright (c) 2024 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Result.Syntax
open Bytesrw
open Webs

(* TODO improve challenge handling
   TODO Safari doesn't seem to support creds.toJSON() yet.
   Alternatively simply send getAuthenticatorData and getPublickey.
   TODO check account does not exist
   TODO try to streamline a bit more *)

let strf = Format.asprintf
let if_error_bad_request_400 = function
| Error e -> Http.Response.bad_request_400 ~log:e () | Ok _ as v -> v

module Page = struct
  let base64_encode_challenge c =
    let c = Webs_passkey.Challenge.to_binary_string c in
    Webs_base64.encode `Padded c

  let create_account_link ?(label = "Create an account") () =
    strf {|<a href="/users/create">%s</a>|} label

  let create_credentials_js challenge = strf "\
    (async function (event) {
       if (window.PublicKeyCredential) {
         try {
           event.preventDefault();
           //  TODO challenge
           const name = event.target.elements.namedItem('email').value;
           const challenge =
              Uint8Array.from(atob('%s'), c => c.charCodeAt(0));
           const rp = { name: '' };
           const id =
              Uint8Array.from (crypto.randomUUID (), c => c.charCodeAt(0));
           const user = { id: id, name: name, displayName: '' };
           const params = [
              { 'type': 'public-key', 'alg': -8  /* EdDSA */ },
              { 'type': 'public-key', 'alg': -7  /* ES256 */ }];
           const authsel = { residentKey: 'required',
                              useryVerification: 'discouraged' };
           const opts =
           { publicKey: {rp: rp, user: user, challenge: challenge,
                         pubKeyCredParams: params, excludeCredentials: [],
                         authenticatorSelection: authsel }};
           const creds = await navigator.credentials.create(opts);
           const json = JSON.stringify (creds);
           event.target.elements.namedItem('creds').value = json;

         } catch (e) { console.log (e); };
         event.target.submit();
       } // else TODO user error
     })(event);" (base64_encode_challenge challenge)

  let get_credentials_js challenge = strf "\
     (async function (event) {
       if (window.PublicKeyCredential) {
         try {
           event.preventDefault();
           // TODO fetch challenge
           const pubkey =
             { challenge: Uint8Array.from(atob('%s'), c => c.charCodeAt(0)),
               userVerification: 'required',
               allowCredentials: [] };
           const creds = await navigator.credentials.get({publicKey: pubkey});
           const json = JSON.stringify (creds);
           event.target.elements.namedItem('creds').value = json;
         } catch (e) { console.log (e); };
         event.target.submit();
       } // else TODO user error
      })(event);" (base64_encode_challenge challenge)

  let login_button ?(label = "Log in") challenge =
    strf {|
    <form id="login" action="/users/login" method="post" onsubmit="%s">
      <input type="hidden" name="creds">
      <input type="submit" value="%s">
    </form>
      |} (get_credentials_js challenge) label

  let login challenge =
    strf {|<!DOCTYPE html>
           <h1>Log in</h1>
           %s
           <p>Or %s</p>|} (login_button challenge) (create_account_link ())

  let create_account challenge =
    strf {|<!DOCTYPE html>
           <h1>Create account</h1>
           <form id="account-create" action="/users/create" method="post"
                 onsubmit="%s">
              <label>Email: <input type="text" name="email"></label>
              <input type="hidden" name="creds">
              <input type="submit" value="Create account">
           </form>
           <p><a href="/users/login">Already have an account?</a><p>|}
      (create_credentials_js challenge)

  let create_failure =
   strf {|<DOCTYPE html>
          <h1>Could not create account</h1>
          Could not create a passkey for your account. %s|}
     (create_account_link ~label:"Try again" ())

  let missing_passkey challenge =
    strf
      {|<h1>Missing passkey</h1>
        %s
        <p>Don't have a passkey ? Log in
        <a href="/users/recover">by email here</a></p>
        <p>Or %s</p>|}
      (login_button ~label:"Try again" challenge) (create_account_link ())
end

module Passkeys = struct
  (* In memory database, passkeys are lost on restart :-*)
  module Db = Map.Make (Webs_passkey.Credential_id)
  let db = ref Db.empty
  let add pk = db := Db.add (Webs_passkey.Passkey.credential_id pk) pk !db
  let find cid = Db.find_opt cid !db
end

let relying_party = Webs_passkey.Relying_party.make ~id:"localhost" ()

let get_creds of_q query = match Http.Query.find_first "creds" query with
| None | Some "" -> Ok None
| Some creds -> if_error_bad_request_400 (Result.map Option.some (of_q creds))

let login_request request =
  let* method' = Http.Request.allow Http.Method.[post; get] request in
  match method' with
  | `GET ->
      let challenge = Webs_passkey.challenge relying_party ~payload:() in
      Ok (Http.Response.html Http.Status.ok_200 (Page.login challenge))
  | `POST ->
      let* query = Http.Request.to_query request in
      let* attestation =
        get_creds Webs_passkey.Assertion.of_public_key_credential_json query
      in
      match attestation with
      | None ->
          let challenge = Webs_passkey.challenge relying_party ~payload:()in
          let missing = Page.missing_passkey challenge in
          Ok (Http.Response.html Http.Status.ok_200 missing)
      | Some a ->
          let credential_id = Webs_passkey.Assertion.credential_id a in
          match Passkeys.find credential_id with
          | None ->
              let log =
                strf "Unknown credential_id: %a"
                  Webs_passkey.Credential_id.pp credential_id
              in
              Http.Response.bad_request_400 ~log ()
          | Some pk ->
              let* () =
                if_error_bad_request_400 @@
                Result.map_error Webs_passkey.error_message @@
                Webs_passkey.verify relying_party pk a
              in
              let text =
                Format.asprintf "LOGIN SUCCESS %a" Webs_passkey.Assertion.pp a
              in
              Ok (Http.Response.text Http.Status.ok_200 text)

let login_create request =
  let* method' = Http.Request.allow Http.Method.[get; post] request in
  match method' with
  | `GET ->
      let challenge = Webs_passkey.challenge relying_party ~payload:() in
      let page = Page.create_account challenge in
      Ok (Http.Response.html Http.Status.ok_200 page)
  | `POST ->
      let* query = Http.Request.to_query request in
      let* registration =
        get_creds
          Webs_passkey.Registration.of_public_key_credential_json query
      in
      match registration with
      | None ->
          Ok (Http.Response.html Http.Status.ok_200 Page.create_failure)
      | Some r ->
          let* _payload, pk =
            if_error_bad_request_400 @@
            Result.map_error Webs_passkey.error_message @@
            Webs_passkey.register relying_party r
          in
          let () = Passkeys.add pk in
          let text =
            Format.asprintf "@[<v>Ok@,%a@]" Webs_passkey.Registration.pp r
          in
          Ok (Http.Response.text Http.Status.ok_200 text)

let service request =
  Http.Response.result @@ match Http.Request.path request with
  | [ "" ] ->
      let login = ["users"; "login"] in
      Ok (Http.Request.redirect_to_path request Http.Status.found_302 login)
  | ["users"; "login"] -> login_request request
  | ["users"; "create"] -> login_create request
  | ["users"; "recover"] -> Http.Response.not_implemented_501 ~log:"TODO" ()
  | _ -> Http.Response.not_found_404 ()

let main () = Webs_quick.serve service
let () = if !Sys.interactive then () else exit (main ())
