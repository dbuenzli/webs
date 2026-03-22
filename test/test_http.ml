(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Webs

let test_method =
  Test.test "Http.Method.{encode,decode}" @@ fun () ->
  let snap = Snap.result (module Http.Method) in
  let test = Test.eq (Test.T.result ~ok:(module Http.Method)) in
  test (Http.Method.decode "GET") (Ok `GET);
  test (Http.Method.decode "HEAD") (Ok `HEAD);
  test (Http.Method.decode "POST") (Ok `POST);
  test (Http.Method.decode "PUT") (Ok `PUT);
  test (Http.Method.decode "DELETE") (Ok `DELETE);
  test (Http.Method.decode "CONNECT") (Ok `CONNECT);
  test (Http.Method.decode "OPTIONS") (Ok `OPTIONS);
  test (Http.Method.decode "TRACE") (Ok `TRACE);
  test (Http.Method.decode "PATCH") (Ok `PATCH);
  test (Http.Method.decode "Get") (Ok (`Other "Get"));
  snap (Http.Method.decode " GET")
  @> __POS_OF__ (Error "\" GET\" is not an HTTP token");
  snap (Http.Method.decode " Get")
  @> __POS_OF__ (Error "\" Get\" is not an HTTP token");
  snap (Http.Method.decode "Get,Get")
  @> __POS_OF__ (Error "\"Get,Get\" is not an HTTP token");
  Snap.string (Http.Method.encode `GET) @> __POS_OF__ ("GET");
  Snap.string (Http.Method.encode `HEAD) @> __POS_OF__ ("HEAD");
  Snap.string (Http.Method.encode `POST) @> __POS_OF__ ("POST");
  Snap.string (Http.Method.encode `PUT) @> __POS_OF__ ("PUT");
  Snap.string (Http.Method.encode `DELETE) @> __POS_OF__ ("DELETE");
  Snap.string (Http.Method.encode `CONNECT) @> __POS_OF__ ("CONNECT");
  Snap.string (Http.Method.encode `OPTIONS) @> __POS_OF__ ("OPTIONS");
  Snap.string (Http.Method.encode `TRACE) @> __POS_OF__ ("TRACE");
  Snap.string (Http.Method.encode `PATCH) @> __POS_OF__ ("PATCH");
  Snap.string (Http.Method.encode (`Other "Get")) @> __POS_OF__ ("Get");
  Test.invalid_arg (fun () -> Http.Method.encode (`Other " Get"));
  Test.invalid_arg (fun () -> Http.Method.encode (`Other "Get,Get"));
  ()

let test_headers_case =
  Test.test "Http.headers case" @@ fun () ->
  let hs = Http.Headers.empty |> Http.Headers.(define (Name.make "ha") "ho") in
  Test.holds (Http.Headers.(mem (Http.Headers.Name.make "Ha") hs));
  ()

let test_path_encode_decode =
  Test.test  "Http.Path.{encode,decode}" @@ fun () ->
  assert (Http.Path.decode "/" = Ok [""]);
  assert (Http.Path.decode "//" = Ok ["";""]);
  assert (Http.Path.decode "//a" = Ok ["";"a"]);
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

let test_path_strip_prefix =
  Test.test "Http.Path.strip_prefix" @@ fun () ->
  assert (Http.Path.strip_prefix ~prefix:[""] [] = []);
  assert (Http.Path.strip_prefix ~prefix:[""] [""] = [""]);
  assert (Http.Path.strip_prefix ~prefix:[""] ["a"] = ["a"]);
  assert (Http.Path.strip_prefix ~prefix:[""] ["a"; "b"] = ["a"; "b"]);
  assert (Http.Path.strip_prefix ~prefix:[""] [""; "a"; "b"] = [""; "a"; "b"]);
  assert (Http.Path.strip_prefix ~prefix:[] [] = []);
  assert (Http.Path.strip_prefix ~prefix:[""] [] = []);
  assert (Http.Path.strip_prefix ~prefix:["a"] [] = []);
  assert (Http.Path.strip_prefix ~prefix:["a"; "b"] [] = []);
  assert (Http.Path.strip_prefix ~prefix:[] ([""]) = []);
  assert (Http.Path.strip_prefix ~prefix:[] (["a"]) = []);
  assert (Http.Path.strip_prefix ~prefix:[] (["a"; "b"]) = []);
  (* /a *)
  assert (Http.Path.strip_prefix ~prefix:["a"] [""] = []);
  assert (Http.Path.strip_prefix ~prefix:["a"] [""; "a"] = []);
  assert (Http.Path.strip_prefix ~prefix:["a"] ["a"] = [""]);
  assert (Http.Path.strip_prefix ~prefix:["a"] ["a"; ""] = [""]);
  assert (Http.Path.strip_prefix ~prefix:["a"] ["b"] = []);
  assert (Http.Path.strip_prefix ~prefix:["a"] ["b"; ""] = []);
  assert (Http.Path.strip_prefix ~prefix:["a"] ["b"; "c"] = []);
  assert (Http.Path.strip_prefix ~prefix:["a"] ["a"; "b"] = ["b"]);
  assert (Http.Path.strip_prefix ~prefix:["a"] ["a"; "b"; ""] = ["b"; ""]);
  assert (Http.Path.strip_prefix ~prefix:["a"] ["a"; "b"; "c"] = ["b"; "c"]);
  assert (Http.Path.strip_prefix ~prefix:["a"] ["a"; ""; "b"] = [""; "b"]);
  (* /a/ *)
  assert (Http.Path.strip_prefix ~prefix:["a"; ""] [""] = []);
  assert (Http.Path.strip_prefix ~prefix:["a"; ""] [""; "a"] = []);
  assert (Http.Path.strip_prefix ~prefix:["a"; ""] ["a"] = []);
  assert (Http.Path.strip_prefix ~prefix:["a"; ""] ["b"] = []);
  assert (Http.Path.strip_prefix ~prefix:["a"; ""] ["a"; ""] = [""]);
  assert (Http.Path.strip_prefix ~prefix:["a"; ""] ["a"; "b"] = ["b"]);
  assert (Http.Path.strip_prefix ~prefix:["a"; ""] ["a"; "b"; ""] = ["b"; ""]);
  assert (Http.Path.strip_prefix ~prefix:["a"] ["a"; ""; "b"] = [""; "b"]);
  assert (Http.Path.strip_prefix ~prefix:["a"; ""] ["a"; "b"; "c"] = ["b";"c"]);
  ()

let test_path_filepath_ext =
  Test.test "Http.Path.filepath_ext" @@ fun () ->
  assert (Http.Path.filepath_ext "" = "");
  assert (Http.Path.filepath_ext "/" = "");
  assert (Http.Path.filepath_ext "/.bla" = "");
  assert (Http.Path.filepath_ext "/a.bla" = ".bla");
  assert (Http.Path.filepath_ext ".bla" = "");
  assert (Http.Path.filepath_ext "a.bla" = ".bla");
  assert (Http.Path.filepath_ext "a.bla/" = ".bla");
  assert (Http.Path.filepath_ext "a.bla/a" = "");
  assert (Http.Path.filepath_ext "/a.bla/a" = "");
  assert (Http.Path.filepath_ext "/a.bla/a.ext" = ".ext");
  ()

let test_path_concat =
  Test.test "Http.Path.concat" @@ fun () ->
  assert (Http.Path.concat [] [] = []);
  assert (Http.Path.concat [""] [] = [""]);
  assert (Http.Path.concat [] [""] = [""]);
  assert (Http.Path.concat [""] [""] = [""]);
  assert (Http.Path.concat [] ["a"] = ["a"]);
  assert (Http.Path.concat [] ["a"; "b"] = ["a"; "b"]);
  assert (Http.Path.concat ["a"] [] = ["a"]);
  assert (Http.Path.concat ["a"] [""] = ["a"; ""]);
  assert (Http.Path.concat ["a"; ""] [""] = ["a"; ""]);
  assert (Http.Path.concat ["a"; "b"] ["c"; "d"] = ["a"; "b"; "c"; "d"]);
  assert (Http.Path.concat ["a"; "b"; ""] [] = ["a"; "b"; ""]);
  assert (Http.Path.concat ["a"; "b"; ""] ["c"; "d"] = ["a"; "b"; "c"; "d"]);
  assert (Http.Path.concat ["a"; "b"; ""] [""] = ["a"; "b"; ""]);
  assert (Http.Path.concat ["a"; "b"; ""] [""; "c"] = ["a"; "b"; ""; "c"]);
  ()

let test_path_undot_and_compress =
  Test.test "Http.Path.undot_and_compress" @@ fun () ->
  assert (Http.Path.undot_and_compress ["a"; "b"; "."] = ["a"; "b"; ""]);
  assert (Http.Path.undot_and_compress
            ["a"; "."; "b"; "."; "."] = ["a"; "b"; ""]);
  assert (Http.Path.undot_and_compress [".."] = [""]);
  ()

let test_path_relativize =
  Test.test "Http.Path.relativize" @@ fun () ->
  let str l = "/" ^ String.concat "/" l and rel_str l = String.concat "/" l in
  let concat_rel ~root rel = match root, rel with
  | [], _ | _, [] -> assert false
  | root, rel ->
      (* This should mimic an HTML href concat. *)
      match List.rev root with
      | _ :: r -> List.rev_append r rel | r -> List.rev_append r rel
  in
  let test ?(trace = false) root path expect =
    let rel = Http.Path.relative ~src:root ~dst:path in
    let cat = Http.Path.undot_and_compress (concat_rel ~root rel) in
    if rel = expect && path = cat then begin
      if trace then
        Format.printf
          "@[<v>@,root: %s@,path: %s@,cat : %s@,rel : %s@]@."
          (str root) (str path) (str cat) (rel_str rel);
    end else begin
      Format.printf
        "@[<v>@,root: %s@,trgt: %s@,cat : %s@,rel : %s@,exp : %s@]@."
        (str root) (str path) (str cat) (rel_str rel) (rel_str expect);
      assert false;
    end
  in
  test [""] [""] ["."];
  test [""] ["a"] ["a"];
  test [""] ["a"; ""] ["a"; ""];
  test [""] ["a"; "b"] ["a"; "b"];
  test [""] ["a"; "b"; ""] ["a"; "b"; ""];
  test [""] ["a"; "b"; "c"] ["a"; "b"; "c"];
  test [""] ["b"] ["b"];
  test [""] ["b"; ""] ["b"; ""];
  test [""] ["b"; "c"] ["b"; "c"];

  test ["a"] [""] ["."];
  test ["a"] ["a"] ["a"];
  test ["a"] ["a"; ""] ["a"; ""];
  test ["a"] ["a"; "b"] ["a"; "b"];
  test ["a"] ["a"; "b"; ""] ["a"; "b"; ""];
  test ["a"] ["a"; "b"; "c"] ["a"; "b"; "c"];
  test ["a"] ["b"] ["b"];
  test ["a"] ["b"; ""] ["b"; ""];
  test ["a"] ["b"; "c"] ["b"; "c"];

  test ["a"; ""] [""] [".."];
  test ["a"; ""] ["a"] [".."; "a"];
  test ["a"; ""] ["a"; ""] ["."];
  test ["a"; ""] ["a"; "b"] ["b"];
  test ["a"; ""] ["a"; "b"; ""] ["b"; ""];
  test ["a"; ""] ["a"; "b"; "c"] ["b"; "c"];
  test ["a"; ""] ["b"] [".."; "b"];
  test ["a"; ""] ["b"; ""] [".."; "b"; ""];
  test ["a"; ""] ["b"; "c"] [".."; "b"; "c"];

  test ["a"; "b"] [""] [".."];
  test ["a"; "b"] ["a"] [".."; "a"];
  test ["a"; "b"] ["a"; ""] [".."; "a"; ""];
  test ["a"; "b"] ["a"; "b"] ["b"];
  test ["a"; "b"] ["a"; "b"; ""] ["b"; ""];
  test ["a"; "b"] ["a"; "b"; "c"] ["b"; "c"];
  test ["a"; "b"] ["b"] [".."; "b"];
  test ["a"; "b"] ["b"; ""] [".."; "b"; ""];
  test ["a"; "b"] ["b"; "c"] [".."; "b"; "c"];

  test ["a"; "b"; ""] [""] [".."; ".."];
  test ["a"; "b"; ""] ["a"] [".."; ".."; "a"];
  test ["a"; "b"; ""] ["a"; ""] [".."; ".."; "a"; ""];
  test ["a"; "b"; ""] ["a"; "b"] [".."; "b"];
  test ["a"; "b"; ""] ["a"; "b"; ""] ["."];
  test ["a"; "b"; ""] ["a"; "b"; "c"] ["c"];
  test ["a"; "b"; ""] ["b"] [".."; ".."; "b"];
  test ["a"; "b"; ""] ["b"; ""] [".."; ".."; "b"; ""];
  test ["a"; "b"; ""] ["b"; "c"] [".."; ".."; "b"; "c"];

  test ["a"; "b"; "c"] [""] [".."; ".."];
  test ["a"; "b"; "c"] ["a"] [".."; ".."; "a"];
  test ["a"; "b"; "c"] ["a"; ""] [".."; ".."; "a"; ""];
  test ["a"; "b"; "c"] ["a"; "b"] [".."; "b"];
  test ["a"; "b"; "c"] ["a"; "b"; ""] [".."; "b"; ""];
  test ["a"; "b"; "c"] ["a"; "b"; "c"] ["c"];
  test ["a"; "b"; "c"] ["b"] [".."; ".."; "b"];
  test ["a"; "b"; "c"] ["b"; ""] [".."; ".."; "b"; ""];
  test ["a"; "b"; "c"] ["b"; "c"] [".."; ".."; "b"; "c"];
  ()

let test_digits =
  Test.test "Http.Digit.{decode,encode}" @@ fun () ->
  let overflow = (Format.asprintf "%d0" max_int) in
  let snap = Snap.(result T.int) in
  snap (Http.Digits.decode "0") @> __POS_OF__ (Ok 0);
  snap (Http.Digits.decode "42") @> __POS_OF__ (Ok 42);
  snap (Http.Digits.decode "042") @> __POS_OF__ (Ok 42);
  snap (Http.Digits.decode "1024") @> __POS_OF__ (Ok 1024);
  snap (Http.Digits.decode overflow)
  @> __POS_OF__ (Error "sequence of digits overflows");
  snap (Http.Digits.decode "")
  @> __POS_OF__ (Error "empty string");
  snap (Http.Digits.decode "-1")
  @> __POS_OF__ (Error "'-' is not a digit");
  Snap.string (Http.Digits.encode 0) @> __POS_OF__ ("0");
  Snap.string (Http.Digits.encode 42) @> __POS_OF__ ("42");
  Snap.string (Http.Digits.encode 1024) @> __POS_OF__ ("1024");
  Test.invalid_arg (fun () -> Http.Digits.encode (-1));
  Test.invalid_arg (fun () -> Http.Digits.encode min_int);
  ()

let test_etags =
  Test.test "Http.Etag.{decode,decode_cond}" @@ fun () ->
  let test = Test.eq (Test.T.result ~ok:(module Http.Etag)) in
  let etags t = Http.Etag.make ~weak:false t, Http.Etag.make ~weak:true t in
  let empty, w_empty = etags "" in
  let xyzzy, w_xyzzy = etags "xyzzy" in
  let r2d2xxxx, w_r2d2xxxx = etags "r2d2xxxx" in
  let c3piozzzz, w_c3piozzzz = etags "c3piozzzz" in
  test (Http.Etag.decode {|"xyzzy"|}) (Ok xyzzy);
  test (Http.Etag.decode {|W/"xyzzy"|}) (Ok w_xyzzy);
  test (Http.Etag.decode {|""|})  (Ok empty);
  assert (Http.Etag.decode_cond {|"xyzzy"|} = Ok (`Etags [xyzzy]));
  assert (Http.Etag.decode_cond {|"xyzzy", "r2d2xxxx", "c3piozzzz"|}
          = Ok (`Etags [xyzzy; r2d2xxxx; c3piozzzz]));
  assert (Http.Etag.decode_cond {|W/"xyzzy", W/"r2d2xxxx", W/"c3piozzzz"|}
          = Ok (`Etags [w_xyzzy; w_r2d2xxxx; w_c3piozzzz]));
  assert (Http.Etag.decode_cond "*" = Ok `Any);
  assert (Result.is_error @@ Http.Etag.decode_cond " * ");
  assert (Result.is_error @@ Http.Etag.decode_cond " *.");
  assert (Result.is_error @@ Http.Etag.decode_cond "");
  ()

let test_ranges =
  Test.test "Http.Range.decode" @@ fun () ->
  let test = Test.eq (Test.T.result ~ok:(module Http.Range)) in
  let snap = Snap.result (module Http.Range) in
  let r0_499 = Http.Range.Range { first = 0; last = 499} in
  let r500_999 = Http.Range.Range { first = 500; last = 999} in
  let last500 = Http.Range.Last  500 in
  let fst9500 = Http.Range.First 9500 in
  let ok l = Ok (Http.Range.Bytes l) in
  test (Http.Range.decode "bytes=0-499") (ok [r0_499]);
  test (Http.Range.decode "bytes=0-499,500-999") (ok [r0_499;r500_999]);
  test (Http.Range.decode "bytes=-500") (ok [last500]);
  test (Http.Range.decode "bytes=9500-") (ok [fst9500]);
  test
    (Http.Range.decode "bytes=0-0,-1")
    (ok [Http.Range.Range {first = 0; last = 0}; Last 1]);
  snap (Http.Range.decode "bytes=2-1")
  @> __POS_OF__ (Error "invalid range");
  snap (Http.Range.decode "by tes=1-2")
  @> __POS_OF__ (Error "\"by tes\" is not an HTTP token");
  test (Http.Range.decode "unit=1-2") (Ok (Other ("unit", "1-2")));
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
