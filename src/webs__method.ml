(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t =
[ `GET | `HEAD | `POST | `PUT | `DELETE | `CONNECT | `OPTIONS | `TRACE
| `PATCH | `Other of string ]

(* Converting *)

let of_token = function
| "GET" -> `GET | "HEAD" -> `HEAD | "POST" -> `POST | "PUT" -> `PUT
| "DELETE" -> `DELETE | "CONNECT" -> `CONNECT | "OPTIONS" -> `OPTIONS
| "TRACE" -> `TRACE | "PATCH" -> `PATCH | s -> `Other s

let decode s = match of_token s with
| `Other s as o ->
    if Webs__base.is_token s then Ok o else Error (Webs__base.err_token s)
| m -> Ok m

let encode = function
| `GET -> "GET" | `HEAD -> "HEAD" | `POST -> "POST" | `PUT -> "PUT"
| `DELETE -> "DELETE" | `CONNECT -> "CONNECT" | `OPTIONS -> "OPTIONS"
| `TRACE -> "TRACE" | `PATCH -> "PATCH"
| `Other s when Webs__base.is_token s -> s
| `Other s -> invalid_arg (Webs__base.err_token s)

(* Predicates and comparisons *)

let equal = Repr.equal
let compare = Repr.compare

(* Constraints *)

type 'a constraint' = t * 'a

let constrain ~allowed m =
  let rec loop mr = function
  | m :: ms -> if (fst m) = mr then Ok (snd m) else loop mr ms
  | [] -> Error allowed
  in
  loop m allowed

let connect = `CONNECT, `CONNECT
let delete = `DELETE, `DELETE
let get = `GET, `GET
let head = `HEAD, `HEAD
let options = `OPTIONS, `OPTIONS
let other s o = `Other s, o
let patch = `PATCH, `PATCH
let post = `POST, `POST
let put = `PUT, `PUT
let trace = `TRACE, `TRACE

(* Formatting *)

let pp ppf m = Webs__base.Fmt.string ppf (encode m)
