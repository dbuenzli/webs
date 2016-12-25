(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
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
