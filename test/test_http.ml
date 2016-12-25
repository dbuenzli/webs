(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing
open Webs

let test_version =
  test "Webs.HTTP.{decode,encode}_version" @@ fun () ->
  let pp_ipair ppf (x,y) = Format.fprintf ppf "(%d,%d)" x y in
  let eq_dec = eq_option ~eq:(=) ~pp:pp_ipair in
  eq_dec (HTTP.decode_version "HTTP/0.9") (Some (0, 9));
  eq_dec (HTTP.decode_version "HTTP/1.0") (Some (1, 0));
  eq_dec (HTTP.decode_version "HTTP/1.1") (Some (1, 1));
  eq_dec (HTTP.decode_version "HTTP/1.2") (Some (1, 2));
  eq_dec (HTTP.decode_version "HTTP/2.2") (Some (2, 2));
  eq_dec (HTTP.decode_version "HTTP /1.1") None;
  eq_dec (HTTP.decode_version "HTTP/1.1 ") None;
  eq_dec (HTTP.decode_version "HTTP/1.10") None;
  eq_dec (HTTP.decode_version "HTTP/10.1") None;
  eq_str (HTTP.encode_version (0, 9)) "HTTP/0.9";
  eq_str (HTTP.encode_version (1, 0)) "HTTP/1.0";
  eq_str (HTTP.encode_version (1, 1)) "HTTP/1.1";
  eq_str (HTTP.encode_version (2, 2)) "HTTP/2.2";
  app_invalid ~pp:pp_str HTTP.encode_version (2,10);
  ()

let test_method =
  test "Webs.HTTP.{decode,encode}_meth" @@ fun () ->
  let eq_meth = eq_option ~eq:(=) ~pp:HTTP.pp_meth in
  eq_meth (HTTP.decode_meth "GET") (Some `GET);
  eq_meth (HTTP.decode_meth "HEAD") (Some `HEAD);
  eq_meth (HTTP.decode_meth "POST") (Some `POST);
  eq_meth (HTTP.decode_meth "PUT") (Some `PUT);
  eq_meth (HTTP.decode_meth "DELETE") (Some `DELETE);
  eq_meth (HTTP.decode_meth "CONNECT") (Some `CONNECT);
  eq_meth (HTTP.decode_meth "OPTIONS") (Some `OPTIONS);
  eq_meth (HTTP.decode_meth "TRACE") (Some `TRACE);
  eq_meth (HTTP.decode_meth "PATCH") (Some `PATCH);
  eq_meth (HTTP.decode_meth "Get") (Some (`Other "Get"));
  eq_meth (HTTP.decode_meth " GET") None;
  eq_meth (HTTP.decode_meth " Get") None;
  eq_meth (HTTP.decode_meth "Get,Get") None;
  eq_str (HTTP.encode_meth `GET) "GET";
  eq_str (HTTP.encode_meth `HEAD) "HEAD";
  eq_str (HTTP.encode_meth `POST) "POST";
  eq_str (HTTP.encode_meth `PUT) "PUT";
  eq_str (HTTP.encode_meth `DELETE) "DELETE";
  eq_str (HTTP.encode_meth `CONNECT) "CONNECT";
  eq_str (HTTP.encode_meth `OPTIONS) "OPTIONS";
  eq_str (HTTP.encode_meth `TRACE) "TRACE";
  eq_str (HTTP.encode_meth `PATCH) "PATCH";
  eq_str (HTTP.encode_meth (`Other "Get")) "Get";
  app_invalid ~pp:pp_str HTTP.encode_meth (`Other " Get");
  app_invalid ~pp:pp_str HTTP.encode_meth (`Other "Get,Get");
  ()

let test_headers_case =
  test "Webs.HTTP.headers case" @@ fun () ->
  let hs = HTTP.H.(empty |> def (name "ha") "ho") in
  eq_bool HTTP.H.(is_def (name "Ha") hs) true;
  ()

let test_path =
  test "Webs.HTTP.{decode,encode}_path" @@ fun () ->
  let eq_path = eq_option ~eq:(=) ~pp:(HTTP.pp_path ()) in
  eq_path (HTTP.decode_path "/") (Some [""]);
  eq_path (HTTP.decode_path "//") (Some ["";""]);
  eq_path (HTTP.decode_path "/a/b/c") (Some ["a"; "b"; "c";]);
  eq_path (HTTP.decode_path "/a/b//c") (Some ["a"; "b"; ""; "c";]);
  eq_path (HTTP.decode_path "/a/b/c/") (Some ["a"; "b"; "c";""]);
  eq_path (HTTP.decode_path "/a/b//c") (Some ["a"; "b"; ""; "c";]);
  eq_path (HTTP.decode_path "/a/b/c/%20") (Some ["a"; "b"; "c"; " "]);
  eq_path (HTTP.decode_path "/a/b//c//") (Some ["a"; "b"; ""; "c"; ""; ""]);
  eq_path (HTTP.decode_path "/a/%2F/b") (Some ["a"; "/"; "b";]);
  eq_path (HTTP.decode_path "/a//b") (Some ["a"; ""; "b";]);
  eq_path (HTTP.decode_path "/r%C3%A9volte") (Some ["r\xC3\xA9volte";]);
  eq_path (HTTP.decode_path "/r%c3%a9volte") (Some ["r\xC3\xA9volte";]);
  eq_path (HTTP.decode_path "/a/not%2520/b") (Some ["a"; "not%20"; "b"]) ;
  eq_path (HTTP.decode_path "/a/b/c/ ") None;
  eq_path (HTTP.decode_path " /a/b/c/") None;
  eq_path (HTTP.decode_path "/a/?/c/") None;
  eq_path (HTTP.decode_path "/a/#/c/") None;
  eq_path (HTTP.decode_path "/a/[/c/") None;
  eq_path (HTTP.decode_path "/a/%") None;
  eq_path (HTTP.decode_path "/a/%!") None;
  eq_path (HTTP.decode_path "/a/%F!") None;
  eq_str (HTTP.encode_path [""]) "/";
             eq_str (HTTP.encode_path [""; ""]) "//";
  eq_str (HTTP.encode_path ["a";"b";"c"]) "/a/b/c";
  eq_str (HTTP.encode_path ["a";"b";"";"c";]) "/a/b//c";
  eq_str (HTTP.encode_path ["a";"b";"c";""]) "/a/b/c/";
  eq_str (HTTP.encode_path ["a";"b";"c";" "]) "/a/b/c/%20";
  eq_str (HTTP.encode_path ["a";"b";"c";"";""]) "/a/b/c//";
  eq_str (HTTP.encode_path ["a"; "b/"; "c"]) "/a/b%2F/c";
  eq_str (HTTP.encode_path ["r\xC3\xC9volte"]) "/r%C3%C9volte";
  eq_str (HTTP.encode_path ["a"; "not%20"; "b"]) "/a/not%2520/b";
  eq_str (HTTP.encode_path ["a"; "/"; "b"]) "/a/%2F/b";
  eq_str (HTTP.encode_path ["a"; "a,b;c=3"; "c"]) "/a/a,b;c=3/c";
  app_invalid ~pp:pp_str HTTP.encode_path [];
  ()

let test_digits =
  test "Webs.HTTP.{decode,encode}_digits" @@ fun () ->
  let overflow = (Format.asprintf "%d0" max_int) in
  let eq_digits = eq_option ~eq:(=) ~pp:Format.pp_print_int in
  eq_digits (HTTP.decode_digits "0") (Some 0);
  eq_digits (HTTP.decode_digits "42") (Some 42);
  eq_digits (HTTP.decode_digits "042") (Some 42);
  eq_digits (HTTP.decode_digits "1024") (Some 1024);
  eq_digits (HTTP.decode_digits overflow) None;
  eq_digits (HTTP.decode_digits "") None;
  eq_digits (HTTP.decode_digits "-1") None;
  eq_str (HTTP.encode_digits 0) "0";
  eq_str (HTTP.encode_digits 42) "42";
  eq_str (HTTP.encode_digits 1024) "1024";
  app_invalid ~pp:pp_str HTTP.encode_digits (-1);
  app_invalid ~pp:pp_str HTTP.encode_digits min_int;
  ()

let suite =
  suite "Testing Webs.HTTP" @@ fun () ->
  test_version ();
  test_method ();
  test_headers_case ();
  test_path ();
  test_digits ();
  ()

let () = suite ()

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
