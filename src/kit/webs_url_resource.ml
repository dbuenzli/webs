(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs

module Named = struct

  (* Resolution *)

  let resolve
      ?(eq = Stdlib.( = )) ~get_res ~res_name ~res_url ~req_name ~req_id ()
    =
    let redirect name id =
      let url = res_url name id in
      let log = "to " ^ url in
      let status = Http.Status.moved_permanently_301 in
      Error (Http.Response.redirect ~log status url)
    in
    match get_res req_id with
    | Error _ as e -> e
    | Ok res ->
        let res_name = res_name res in
        match req_name with
        | None -> redirect res_name req_id
        | Some n -> if eq res_name n then Ok res else redirect res_name req_id

  (* Names *)

  type name = string

  let trim_and_compress c s = (* trims and compresses [c] chars in [s]. *)
    let len = String.length s in
    let clen = ref len in
    let last_was_c = ref false in
    if s.[0] = c then decr clen;
    for i = 0 to len - 1 do
      let is_c = s.[i] = c in
      if is_c && !last_was_c then decr clen;
      last_was_c := is_c;
    done;
    if s.[len - 1] = c then decr clen;
    if !clen = len then s else
    if !clen <= 0 then "" else
    let b = Bytes.create !clen in
    last_was_c := true; (* To trim initial, if any *)
    let k = ref 0 in
    for i = 0 to !clen - 1 do
      while s.[!k] = c && !last_was_c do incr k; done;
      last_was_c := s.[!k] = c;
      Bytes.set b i s.[!k];
      incr k
    done;
    Bytes.unsafe_to_string b

  let all_digits s = (* String.for_all :-( *)
    try
      for i = 0 to String.length s - 1 do
        (match s.[i] with '0' .. '9' -> () | _ -> raise_notrace Exit)
      done;
      true
    with Exit -> false

  let name_of_string s =
    if String.equal s "" then "-" else
    let tr = function
    | 'A' .. 'Z' as c -> Char.lowercase_ascii c
    | 'a' .. 'z' | '0' .. '9' as c -> c
    | c when Char.code c <= 0x7F (* US-ASCII byte *) -> '-'
    | c -> c (* UTF-8 byte *)
    in
    let s = trim_and_compress '-' (String.map tr s) in
    if all_digits s || s = "" then s ^ "-" else s
end

module Id = struct

  (* Errors *)

  type error = [`Overflow | `Syntax]

  let error_message = function
  | `Overflow -> "id overflow"
  | `Syntax -> "id syntax error"

  let error_to_resp e =
    Http.Response.empty ~reason:(error_message e) Http.Status.bad_request_400

  (* Identifiers *)

  type t = int

  let[@inline] is_digit = function '0' .. '9' -> true | _ -> false
  let[@inline] digit_value c = Char.code c - 0x30
  let of_string s =
    if s = "" then Error `Syntax else
    if s = "0" then Ok 0 else
    if s.[0] = '0' then Error `Syntax (* disallows leading zeros *) else
    let rec overflow_or_syntax_error k max =
      if k > max then Error `Overflow else
      if not (is_digit s.[k]) then Error `Syntax else
      overflow_or_syntax_error (k + 1) max
    in
    let rec loop k acc max =
      if k > max then Ok acc else
      let c = s.[k] in
      if not (is_digit c) then Error `Syntax else
      let acc = acc * 10 + (digit_value c) in
      if acc < 0
      then overflow_or_syntax_error (k + 1) max
      else loop (k + 1) acc max
    in
    loop 0 0 (String.length s - 1)

  let to_string id =
    if id < 0
    then invalid_arg (Printf.sprintf "%d: negative identifier" id)
    else string_of_int id

  let decode s = Result.map_error error_to_resp (of_string s)
end
