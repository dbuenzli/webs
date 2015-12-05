(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Response bodies *)

type consumer = (bytes * int * int) option -> unit
type body =
  | Stream of (consumer -> unit)
  | File of (int * int) option * string

let stream_body producer = Stream producer

let string_body s =
  Stream begin fun yield ->
    (* We could use Bytes.unsafe_of_string since the consumer
       is not supposed to mutate the bytes. But let's not do
       that for now. *)
    yield (Some (Bytes.of_string s, 0, String.length s));
    yield None
  end

let empty_body = Stream (fun yield -> yield None)
let file_body ?range name = File (range, name)

let pp_body ppf = function
| Stream _ -> Format.fprintf ppf "stream"
| File (r, n) ->
    let pp_range ppf = function
    | None -> ()
    | Some (pos, len) ->
        Format.fprintf ppf "@[<1>(pos@ %d)@]@ \
                            @[<1>(len@ %d)@]" pos len
    in
    Format.fprintf ppf "@[<1>(body@ %S@ %a)@]" n pp_range r

(* Responses *)

type t =
  { version : Webs_http.version;
    status : Webs_http.status;
    headers : Webs_http.headers;
    body : body; }

let v ?(version = (1,1)) status headers body =
  { version; status; headers; body }

let version r = r.version
let status r = r.status
let headers r = r.headers
let body r = r.body

let with_status r status = { r with status }
let with_headers r headers = { r with headers }
let with_body r body = { r with body }

let pp ppf r =
  Format.fprintf ppf
    "@[<1>(response@ @[<1>(status %a)@]@ %a@ %a@]"
    Webs_http.pp_status r.status
    Webs_http.pp_headers r.headers
    pp_body r.body

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
