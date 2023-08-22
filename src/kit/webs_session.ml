(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs

let strf = Printf.sprintf
let ( let* ) = Result.bind

module State = struct
  type 'a t =
    { encode : 'a -> string;
      decode : string -> ('a, string) result;
      equal : 'a -> 'a -> bool }

  let make ~encode ~decode ~equal () = { equal; encode; decode }
  let equal s = s.equal
  let encode s = s.encode
  let decode s = s.decode
  let option_equal st s0 s1 = match s0, s1 with
  | None, None -> true | Some s0, Some s1 -> st.equal s0 s1 | _ -> false
end

(* Handler *)

module Handler = struct
  type ('a, 'e) t =
    { load : 'a State.t -> Http.Request.t -> ('a option, 'e) result;
      save : 'a State.t -> 'a option -> Http.Response.t -> Http.Response.t }

  let v ~load ~save () = { load; save }
  let load h = h.load
  let save h = h.save
end

type 'a response = 'a option * Http.Response.t

let setup sd h service = fun req ->
  let r = h.Handler.load sd req in
  let s', resp = service r req in
  match r with
  | Ok s when State.option_equal sd s s' -> resp
  | Ok _ | Error _ -> h.Handler.save sd s' resp

(* Client stored *)

type client_stored_error =
[ Webs_authenticated_cookie.error | `State_decode of string ]

let client_stored_error_message = function
| `State_decode e -> strf "state decode: %s" e
| #Webs_authenticated_cookie.error as e ->
    Webs_authenticated_cookie.error_message e

let client_stored_error_string r =
  Result.map_error client_stored_error_message r

let client_stored ~private_key ?attributes ~name () =
  let load sd r =
    match Webs_authenticated_cookie.find ~private_key ~now:None ~name r with
    | Ok (Some (_, s)) ->
        begin match sd.State.decode s with
        | Error e -> Error (`State_decode e)
        | Ok v -> Ok (Some v)
        end
    | (Ok None | Error _ as v) -> (v :> ('a option, client_stored_error) result)
  in
  let save sd s r = match s with
  | None -> Webs_authenticated_cookie.clear ?attributes ~name r
  | Some s ->
      let data = sd.State.encode s in
      Webs_authenticated_cookie.set
        ?attributes ~private_key ~expire:None ~name data r
  in
  Handler.v ~load ~save ()

(* Result state injection *)

let for_result st = function Ok v -> Ok (st, v) | Error e -> Error (st, e)
let for_ok st = function Ok v -> Ok (st, v) | Error _ as e -> e
let for_error st = function Ok _ as v -> v | Error e -> Error (st, e)
