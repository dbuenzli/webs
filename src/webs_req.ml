(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* HTTP requests *)

type body = unit -> (bytes * int * int) option

type t =
  { version : Webs_http.version;
    meth : Webs_http.meth;
    path : Webs_http.path;
    query : string option;
    headers : Webs_http.headers;
    info : Hmap.t;
    body_len : int option;
    body : unit -> (bytes * int * int) option }

let v ?(info = Hmap.empty) version meth ~path ?query headers ?body_len body =
  { meth; path; query; version; headers; info; body_len; body; }

let version r = r.version
let meth r = r.meth
let path r = r.path
let query r = r.query
let headers r = r.headers
let info r = r.info
let body_len r = r.body_len
let body r = r.body

let with_headers r headers = { r with headers }
let with_body r ~body_len body = { r with body_len; body }
let with_path r path = { r with path }
let with_info r info = { r with info }

let pp_body_len ppf = function
| None -> Format.fprintf ppf "unknown"
| Some len -> Format.fprintf ppf "%d" len

let pp_query ppf = function
| None -> Format.fprintf ppf "none"
| Some q -> Format.fprintf ppf "%S" q

let pp ppf r =
  Format.fprintf ppf
    "@[<1>(request@ @[(version %a)@]@ @[<1>(method %a)@]@ \
                    @[<1>(path %a)@]@ @[<1>(query %a)@]@  \
                    %a@ \
                    @[<1>(body-len %a)@])@]"
    Webs_http.pp_version r.version
    Webs_http.pp_meth r.meth
    (Webs_http.pp_path ~human:false ()) r.path
    pp_query r.query
    Webs_http.pp_headers r.headers
    pp_body_len r.body_len

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
