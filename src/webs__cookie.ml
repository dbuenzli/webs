(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type name = string
type attributes =
  { domain : string option;
    http_only : bool;
    max_age : int option;
    path : Webs__path.t;
    same_site : string;
    secure : bool; }

let default_attributes =
  { domain = None; http_only = true; max_age = None; path = [];
    same_site = "strict"; secure = true; }

let attributes
    ?init:(a = default_attributes) ?(domain = a.domain)
    ?(http_only = a.http_only) ?(max_age = a.max_age)
    ?(path = a.path) ?(same_site = a.same_site) ?(secure = a.secure) ()
  =
  { domain; http_only; max_age; path; same_site; secure }

let encode_attributes a =
  let max_age = match a.max_age with
  | None -> "" | Some a -> ";max-age=" ^ string_of_int a
  in
  let domain = match a.domain with None -> "" | Some d -> ";domain=" ^ d in
  let path =
    if a.path = [] then "" else
    ";path=" ^ (Webs__path.encode a.path) in
  let secure = if a.secure then ";Secure" else "" in
  let http_only = if a.http_only then ";httponly" else "" in
  let same_site = ";samesite=" ^ a.same_site in
  String.concat "" [max_age; domain; path; secure; http_only; same_site]

let encode ?(attributes = default_attributes) ~name value =
  String.concat "" [name; "="; value; encode_attributes attributes]

let decode_list s =
  (* Very lax parsing, pretty sure someone will complain at some point.
     https://www.rfc-editor.org/rfc/rfc6265#section-4.2 *)
  let rec loop acc = function
  | [] -> Ok (List.rev acc)
  | c :: cs ->
      match String.index_opt c '=' with
      | None -> Webs__base.Fmt.error "%S: illegal cookie pair" c
      | Some i ->
          let n = Webs__base.string_subrange ~last:(i - 1) c in
          let v = Webs__base.string_subrange ~first:(i + 1) c in
          let v =
            if v = "" then "" else
            let len = String.length v - 1 in
            if v.[0] = '\"' && v.[len - 1] = '\"' && len > 1
            then Webs__base.string_subrange ~first:1 ~last:(len - 2) v else
            v
          in
          loop ((n, v) :: acc) cs
  in
  loop [] (Webs__headers.values_of_string ~sep:';' s)
