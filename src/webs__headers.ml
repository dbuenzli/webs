(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let err_header_undefined n = Webs__base.Fmt.str "header %s undefined" n
let err_headers_length_conflicts = "conflicting body length specification"
let err_headers_length = "cannot determine body length"
let err_empty_multi_value = "multi value cannot be the empty list"

(* Header names *)

module Name = struct
  type t = string
  let make s = try Webs__base.lower_token_of_string s with
  | Failure e -> invalid_arg e

  let unsafe_of_string = Fun.id
  let encode = Fun.id
  let decode s = try Ok (Webs__base.lower_token_of_string s) with
  | Failure e -> Error e

  let equal = String.equal
  let compare = String.compare
  let pp = Webs__base.Fmt.string
end

(* Headers *)

type t = string Webs__base.String_map.t (* always lowercased by Name.make *)
let empty = Webs__base.String_map.empty
let define = Webs__base.String_map.add
let define_if_some n o hs = match o with None -> hs | Some v -> define n v hs
let define_if_undefined n v hs =
  let def = function None -> Some v | def -> def in
  Webs__base.String_map.update n def hs

let undefine = Webs__base.String_map.remove
let _append_value sep n v hs = match Webs__base.String_map.find_opt n hs with
| None -> Webs__base.String_map.add n v hs
| Some v' -> Webs__base.String_map.add n (String.concat sep [v; v']) hs

let append_value n v hs = _append_value "," n v hs
let append_set_cookie v hs = _append_value "\x00" "set-cookie" v hs
let override hs ~by =
  let merge_right _ _ v = Some v in
  Webs__base.String_map.union merge_right hs by

(* Lookups *)

let find ?(lowervalue = false) n hs =
  if lowervalue
  then
    Option.map Webs__base.string_lowercase
      (Webs__base.String_map.find_opt n hs)
  else Webs__base.String_map.find_opt n hs

let find_or_error ?lowervalue n hs = match find ?lowervalue n hs with
| None -> Webs__base.Fmt.error "%a: No such header" Name.pp n
| Some v -> Ok v

let get ?lowervalue n hs = match find ?lowervalue n hs with
| None -> invalid_arg (err_header_undefined n)
| Some v -> v

let fold = Webs__base.String_map.fold

(* Header values *)

let values_of_set_cookie_value s = String.split_on_char '\x00' s
let values_of_string ?(sep = ',') s =
  List.rev @@ List.rev_map Webs__base.trim_ows (String.split_on_char sep s)

let values_to_string ?(sep = ',') = function
| [] -> invalid_arg err_empty_multi_value
| vs -> String.concat (String.make 1 sep) vs

let value_is_token = Webs__base.is_token
let value_trim_ows = Webs__base.trim_ows

(* Converting *)

let header_of_string s =
  let crlf = String.length s in
  match
    Webs__base.decode_header_field (Bytes.unsafe_of_string s) ~first:0 ~crlf
  with
  | exception Failure e -> Error e
  | n, v -> Ok (Name.unsafe_of_string n, v)


(* Predicates and comparisons *)

let is_empty = Webs__base.String_map.is_empty
let mem = Webs__base.String_map.mem
let equal = Webs__base.String_map.equal String.equal
let compare = Webs__base.String_map.compare String.compare

(* Formatting *)

let pp ppf hs =
  let pp_header ppf (n, v)  =
    if not (Name.equal n "set-cookie")
    then Webs__base.Fmt.(field n qstring) ppf v else
    let cs = values_of_set_cookie_value v in
    let pp_set_cookie = Webs__base.Fmt.(field "set-cookie" qstring) ppf in
    List.iter pp_set_cookie cs
  in
  Webs__base.Fmt.list pp_header ppf (Webs__base.String_map.bindings hs)

(* Standard header names *)

let accept = "accept"
let accept_charset = "accept-charset"
let accept_encoding = "accept-encoding"
let accept_language = "accept-language"
let accept_ranges = "accept-ranges"
let age = "age"
let allow = "allow"
let authorization = "authorization"
let cache_control = "cache-control"
let connection = "connection"
let content_disposition = "content-disposition"
let content_encoding = "content-encoding"
let content_language = "content-language"
let content_length = "content-length"
let content_location = "content-location"
let content_range = "content-range"
let content_type = "content-type"
let cookie = "cookie"
let date = "date"
let etag = "etag"
let expect = "expect"
let expires = "expires"
let from = "from"
let host = "host"
let if_match = "if-match"
let if_modified_since = "if-modified-since"
let if_none_match = "if-none-match"
let if_range = "if-range"
let if_unmodified_since = "if-unmodified-since"
let last_modified = "last-modified"
let location = "location"
let max_forwards = "max-forwards"
let origin = "origin"
let pragma = "pragma"
let proxy_authenticate = "proxy-authenticate"
let proxy_authorization = "proxy-authorization"
let range = "range"
let referer = "referer"
let retry_after = "retry-after"
let sec_fetch_site = "sec-fetch-site"
let sec_fetch_mode = "sec-fetch-mode"
let sec_fetch_user = "sec-fetch-user"
let sec_fetch_dest = "sec-fetch-dest"
let server = "server"
let set_cookie = "set-cookie"
let te = "te"
let trailer = "trailer"
let transfer_encoding = "transfer-encoding"
let upgrade = "upgrade"
let user_agent = "user-agent"
let vary = "vary"
let via = "via"
let warning = "warning"
let www_authenticate = "www-authenticate"

(* Header specific headers *)

let request_body_length hs =
  let len = find content_length hs in
  let tr = find transfer_encoding ~lowervalue:true hs in
  match len, tr with
  | Some _, Some _ -> Error err_headers_length_conflicts (* §3.3.3 3. *)
  | None, None -> Ok (`Length 0) (* §3.3.3 6. *)
  | Some l, None -> Result.map (fun l -> `Length l) (Webs__digits.decode l)
  | None, Some tes ->
      (* §3.3.3 3. *)
      let tes = values_of_string tes in
      let chunked = String.equal "chunked" (List.hd (List.rev tes)) in
      if chunked then Ok `Chunked else Error err_headers_length

let decode_host scheme hs =
  let find_hostname_port scheme host = match String.rindex_opt host ':' with
  | None -> Ok (host, Webs__scheme.tcp_port scheme)
  | Some j ->
      let port = String.sub host (j + 1) (String.length host - j - 1) in
      if port = ""
      then Ok (String.sub host 0 j, (Webs__scheme.tcp_port scheme)) else
      match int_of_string_opt port with
      | Some port -> Ok (String.sub host 0 j, port)
      | None -> (* IPv6 without port gets here *)
          Ok (host, (Webs__scheme.tcp_port scheme))
  in
  match find_or_error host hs with
  | Error _ as e -> e
  | Ok  host -> find_hostname_port scheme host

let for_connector headers body =
  let define_content_type c =
    let c = match c with
    | None -> Webs__body.content_type body
    | Some c -> c
    in
    if c = Webs__media_type.none then None else Some c
  in
  let define_content_length = function
  | Some "" -> None
  | Some _ as v -> v
  | None ->
      match Webs__body.content_length body with
      | None -> None
      | Some l -> Some (Int.to_string l)
  in
  headers
  |> Webs__base.String_map.update content_type define_content_type
  |> Webs__base.String_map.update content_length define_content_length
