(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs

let log = print_endline

let raises_invalid f = try f (); assert false with Invalid_argument _ -> ()

let test_version () =
  log "Webs.Http.Version.{encode,decode}";
  assert (Http.Version.decode "HTTP/0.9" = Ok (0, 9));
  assert (Http.Version.decode "HTTP/1.0" = Ok (1, 0));
  assert (Http.Version.decode "HTTP/1.1" = Ok (1, 1));
  assert (Http.Version.decode "HTTP/1.2" = Ok (1, 2));
  assert (Http.Version.decode "HTTP/2.2" = Ok (2, 2));
  assert (Result.is_error @@ Http.Version.decode "HTTP /1.1");
  assert (Result.is_error @@ Http.Version.decode "HTTP/1.1 ");
  assert (Result.is_error @@ Http.Version.decode "HTTP/1.10");
  assert (Result.is_error @@ Http.Version.decode "HTTP/10.1");
  assert (Http.Version.encode (0, 9) = "HTTP/0.9");
  assert (Http.Version.encode (1, 0) = "HTTP/1.0");
  assert (Http.Version.encode (1, 1) = "HTTP/1.1");
  assert (Http.Version.encode (2, 2) = "HTTP/2.2");
  ()

let test_method () =
  log "Webs.Http.Meth.{encode,decode}";
  assert (Http.Meth.decode "GET" = Ok `GET);
  assert (Http.Meth.decode "HEAD" = Ok `HEAD);
  assert (Http.Meth.decode "POST" = Ok `POST);
  assert (Http.Meth.decode "PUT" = Ok `PUT);
  assert (Http.Meth.decode "DELETE" = Ok `DELETE);
  assert (Http.Meth.decode "CONNECT" = Ok `CONNECT);
  assert (Http.Meth.decode "OPTIONS" = Ok `OPTIONS);
  assert (Http.Meth.decode "TRACE" = Ok `TRACE);
  assert (Http.Meth.decode "PATCH" = Ok `PATCH);
  assert (Http.Meth.decode "Get" = Ok (`Other "Get"));
  assert (Result.is_error @@ Http.Meth.decode " GET");
  assert (Result.is_error @@ Http.Meth.decode " Get");
  assert (Result.is_error @@ Http.Meth.decode "Get,Get");
  assert (Http.Meth.encode `GET = "GET");
  assert (Http.Meth.encode `HEAD = "HEAD");
  assert (Http.Meth.encode `POST = "POST");
  assert (Http.Meth.encode `PUT = "PUT");
  assert (Http.Meth.encode `DELETE = "DELETE");
  assert (Http.Meth.encode `CONNECT = "CONNECT");
  assert (Http.Meth.encode `OPTIONS = "OPTIONS");
  assert (Http.Meth.encode `TRACE = "TRACE");
  assert (Http.Meth.encode `PATCH = "PATCH");
  assert (Http.Meth.encode (`Other "Get") = "Get");
  raises_invalid (fun () -> Http.Meth.encode (`Other " Get"));
  raises_invalid (fun () -> Http.Meth.encode (`Other "Get,Get"));
  ()

let test_headers_case () =
  log "Webs.Http.headers case";
  let hs = Http.H.(empty |> set (Http.Name.v "ha") "ho") in
  assert (Http.H.(mem (Http.Name.v "Ha") hs));
  ()

let test_path () =
  log "Webs.Http.Path.{encode,decode}";
  assert (Http.Path.decode "/" = Ok [""]);
  assert (Http.Path.decode "//" = Ok ["";""]);
  assert (Http.Path.decode "/a/b/c" = Ok ["a"; "b"; "c";]);
  assert (Http.Path.decode "/a/b//c" = Ok ["a"; "b"; ""; "c";]);
  assert (Http.Path.decode "/a/b/c/" = Ok ["a"; "b"; "c";""]);
  assert (Http.Path.decode "/a/b//c" = Ok ["a"; "b"; ""; "c";]);
  assert (Http.Path.decode "/a/b/c/%20" = Ok ["a"; "b"; "c"; " "]);
  assert (Http.Path.decode "/a/b//c//" = Ok ["a"; "b"; ""; "c"; ""; ""]);
  assert (Http.Path.decode "/a/%2F/b" = Ok ["a"; "/"; "b";]);
  assert (Http.Path.decode "/a//b" = Ok ["a"; ""; "b";]);
  assert (Http.Path.decode "/r%C3%A9volte" = Ok ["r\xC3\xA9volte";]);
  assert (Http.Path.decode "/r%c3%a9volte" = Ok ["r\xC3\xA9volte";]);
  assert (Http.Path.decode "/a/not%2520/b" = Ok ["a"; "not%20"; "b"]) ;
  assert (Result.is_error @@ Http.Path.decode "/a/b/c/ ");
  assert (Result.is_error @@ Http.Path.decode " /a/b/c/");
  assert (Result.is_error @@ Http.Path.decode "/a/?/c/");
  assert (Result.is_error @@ Http.Path.decode "/a/#/c/");
  assert (Result.is_error @@ Http.Path.decode "/a/[/c/");
  assert (Http.Path.decode "/a/%" = Ok ["a"; "%"]);
  assert (Http.Path.decode "/a/%!" = Ok ["a"; "%!"]);
  assert (Http.Path.decode "/a/%F!" = Ok ["a"; "%F!"]);
  assert (Http.Path.encode [""] = "/");
  assert (Http.Path.encode [""; ""] = "//");
  assert (Http.Path.encode ["a";"b";"c"] = "/a/b/c");
  assert (Http.Path.encode ["a";"b";"";"c";] = "/a/b//c");
  assert (Http.Path.encode ["a";"b";"c";""] = "/a/b/c/");
  assert (Http.Path.encode ["a";"b";"c";" "] = "/a/b/c/%20");
  assert (Http.Path.encode ["a";"b";"c";"";""] = "/a/b/c//");
  assert (Http.Path.encode ["a"; "b/"; "c"] = "/a/b%2F/c");
  assert (Http.Path.encode ["r\xC3\xC9volte"] = "/r%C3%C9volte");
  assert (Http.Path.encode ["a"; "not%20"; "b"] = "/a/not%2520/b");
  assert (Http.Path.encode ["a"; "/"; "b"] = "/a/%2F/b");
  assert (Http.Path.encode ["a"; "a,b;c=3"; "c"] = "/a/a,b;c=3/c");
  assert (Http.Path.encode [] = "");
  ()

let test_digits () =
  log "Webs.Http.H.digits_{of,to}_string";
  let overflow = (Format.asprintf "%d0" max_int) in
  assert (Http.H.digits_of_string "0" = Ok 0);
  assert (Http.H.digits_of_string "42" = Ok 42);
  assert (Http.H.digits_of_string "042" = Ok 42);
  assert (Http.H.digits_of_string "1024" = Ok 1024);
  assert (Result.is_error @@ Http.H.digits_of_string overflow);
  assert (Result.is_error @@ Http.H.digits_of_string "");
  assert (Result.is_error @@ Http.H.digits_of_string "-1");
  assert (Http.H.digits_to_string 0 = "0");
  assert (Http.H.digits_to_string 42 = "42");
  assert (Http.H.digits_to_string 1024 = "1024");
  raises_invalid (fun () -> Http.H.digits_to_string (-1));
  raises_invalid (fun () -> Http.H.digits_to_string min_int);
  ()

let main () =
  test_version ();
  test_method ();
  test_headers_case ();
  test_path ();
  test_digits ();
  print_endline "All tests succeeded."

let () = main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers

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
