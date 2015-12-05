(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Webs

let echo r = failwith "TODO"
(*
  let b = Buffer.create 512 in
  let add = Buffer.add_string in
  let add_header (n, v) =
    add b "\n "; add b n; add b " = \""; add b v; add b "\"";
  in
  add b "request_remote_addr = \""; add b r.request_remote_addr;
  add b "\"\nrequest_protocol = \""; add b r.request_protocol;
  add b "\"\nrequest_is_https = "; add b (string_of_bool r.request_is_https);
  add b "\nrequest_method = \""; add b r.request_method;
  add b "\"\nrequest_uri = \""; add b r.request_uri;
  add b "\"\nrequest_headers = ";
  List.iter add_header (List.sort compare r.request_headers);
  add b "\nrequest_body = ";
  begin match body with
  | None -> add b "None"
  | Some (t, l, _) ->
	    add b "\""; add b t; add b "\", ";
	   add b (string_of_int l); add b " bytes\n";
	   add b " TODO print body\n";
  end;
  HTTP.(s200_ok,
        [h content_type "text/plain"], `R_builder (Buffer.contents b))
*)

let log_req ppf s = failwith "TODO"
let log_resp ppf s = failwith "TODO"
let log ppf s = failwith "TODO"


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
