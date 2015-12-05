(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* HTTP requests *)

type body = unit -> (bytes * int * int) option

type t =
  { version : Webs_http.version;
    meth : Webs_http.meth;
    path : Webs_http.path;
    query : string option;
    headers : Webs_http.headers;
    dict : Webs_dict.t;
    body_len : int option;
    body : unit -> (bytes * int * int) option }

let v ?(dict = Webs_dict.empty) version meth ~path ?query headers
    ?body_len body =
  { meth; path; query; version; headers; dict; body_len; body; }

let version r = r.version
let meth r = r.meth
let path r = r.path
let query r = r.query
let headers r = r.headers
let dict r = r.dict
let body_len r = r.body_len
let body r = r.body

let with_headers r headers = { r with headers }
let with_body r ~body_len body = { r with body_len; body }
let with_path r path = { r with path }
let with_dict r dict = { r with dict }

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
                    %a@ %a@ \
                    @[<1>(body-len %a)@])@]"
    Webs_http.pp_version r.version
    Webs_http.pp_meth r.meth
    (Webs_http.pp_path ~human:false ()) r.path
    pp_query r.query
    Webs_http.pp_headers r.headers
    Webs_dict.pp r.dict
    pp_body_len r.body_len

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
