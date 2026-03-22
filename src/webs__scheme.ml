(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = Http | Https

let tcp_port = function Http -> 80 | Https -> 443

(* Converting *)

let encode = function Http -> "http" | Https -> "https"
let decode_of_url u = match Webs__url.scheme u with
| Some "http" -> Ok Http
| Some "https" -> Ok Https
| None | Some "" -> Webs__base.Fmt.error "No scheme found in URL %s" u
| Some s -> Webs__base.Fmt.error "Scheme %s in URL is not an HTTP URL scheme" s

(* Predicates and comparisons *)

let equal = Repr.equal
let compare = Repr.compare

(* Formatting *)

let pp ppf s = Webs__base.Fmt.string ppf (encode s)
