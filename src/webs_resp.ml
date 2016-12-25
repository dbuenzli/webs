(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
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
