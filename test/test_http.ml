(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
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
  log "Webs.Http.Method.{encode,decode}";
  assert (Http.Method.decode "GET" = Ok `GET);
  assert (Http.Method.decode "HEAD" = Ok `HEAD);
  assert (Http.Method.decode "POST" = Ok `POST);
  assert (Http.Method.decode "PUT" = Ok `PUT);
  assert (Http.Method.decode "DELETE" = Ok `DELETE);
  assert (Http.Method.decode "CONNECT" = Ok `CONNECT);
  assert (Http.Method.decode "OPTIONS" = Ok `OPTIONS);
  assert (Http.Method.decode "TRACE" = Ok `TRACE);
  assert (Http.Method.decode "PATCH" = Ok `PATCH);
  assert (Http.Method.decode "Get" = Ok (`Other "Get"));
  assert (Result.is_error @@ Http.Method.decode " GET");
  assert (Result.is_error @@ Http.Method.decode " Get");
  assert (Result.is_error @@ Http.Method.decode "Get,Get");
  assert (Http.Method.encode `GET = "GET");
  assert (Http.Method.encode `HEAD = "HEAD");
  assert (Http.Method.encode `POST = "POST");
  assert (Http.Method.encode `PUT = "PUT");
  assert (Http.Method.encode `DELETE = "DELETE");
  assert (Http.Method.encode `CONNECT = "CONNECT");
  assert (Http.Method.encode `OPTIONS = "OPTIONS");
  assert (Http.Method.encode `TRACE = "TRACE");
  assert (Http.Method.encode `PATCH = "PATCH");
  assert (Http.Method.encode (`Other "Get") = "Get");
  raises_invalid (fun () -> Http.Method.encode (`Other " Get"));
  raises_invalid (fun () -> Http.Method.encode (`Other "Get,Get"));
  ()

let test_headers_case () =
  log "Webs.Http.headers case";
  let hs = Http.Headers.(empty |> def (Http.Name.v "ha") "ho") in
  assert (Http.Headers.(mem (Http.Name.v "Ha") hs));
  ()

let test_path () =
  log "Webs.Http.Path.{encode,decode,strip_prefix,filpath_ext}";
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
  log "Webs.Http.Path.strip_prefix";
  assert (Http.Path.strip_prefix ~prefix:[] [] = None);
  assert (Http.Path.strip_prefix ~prefix:[] (["a"]) = None);
  assert (Http.Path.strip_prefix ~prefix:[] (["a"; "b"]) = None);
  assert (Http.Path.strip_prefix ~prefix:[""] [] = None);
  assert (Http.Path.strip_prefix ~prefix:["a"] [] = None);
  assert (Http.Path.strip_prefix ~prefix:["a"; "b"] [] = None);
  assert (Http.Path.strip_prefix ~prefix:[""] [""] = Some [""]);
  assert (Http.Path.strip_prefix ~prefix:[""] ["a"; "b"] = Some ["a"; "b"]);
  assert (Http.Path.strip_prefix ~prefix:[""] [""; "a"; "b"] =
                                              Some [""; "a"; "b"]);
  assert (Http.Path.strip_prefix ~prefix:["a"] ["a"] = Some [""]);
  assert (Http.Path.strip_prefix ~prefix:["a"] ["a"; ""] = Some [""]);
  assert (Http.Path.strip_prefix ~prefix:["a"] ["a"; ""; "b"] = Some [""; "b"]);
  assert (Http.Path.strip_prefix ~prefix:["a"] ["a"; "b"] = Some ["b"]);
  assert (Http.Path.strip_prefix ~prefix:["a"] ["a"; "b"; ""] = Some ["b"; ""]);
  assert (Http.Path.strip_prefix ~prefix:["a"] ["a"; "b"; "c"] = Some ["b"; "c"]);
  assert (Http.Path.strip_prefix ~prefix:["a"] [""] = None);
  assert (Http.Path.strip_prefix ~prefix:["a"] ["b"] = None);
  assert (Http.Path.strip_prefix ~prefix:["a"] [""; "a"] = None);
  assert (Http.Path.strip_prefix ~prefix:["a"; ""] ["a"] = None);
  assert (Http.Path.strip_prefix ~prefix:["a"; ""] ["a"; ""] = Some [""]);
  assert (Http.Path.strip_prefix ~prefix:["a"] ["a"; ""; "b"] = Some [""; "b"]);
  assert (Http.Path.strip_prefix ~prefix:["a"; ""] ["a"; "b"] = Some ["b"]);
  assert (Http.Path.strip_prefix ~prefix:["a"; ""] ["a"; "b"; ""] =
                                                   Some ["b"; ""]);
  assert (Http.Path.strip_prefix ~prefix:["a"; ""] ["a"; "b"; "c"] =
                                                   Some ["b"; "c"]);
  assert (Http.Path.strip_prefix ~prefix:["a"; ""] [""] = None);
  assert (Http.Path.strip_prefix ~prefix:["a"; ""] ["b"] = None);
  assert (Http.Path.strip_prefix ~prefix:["a"; ""] [""; "a"] = None);
  log "Webs.Http.Path.filepath_ext";
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
  log "Webs.Http.Path.concat";
  assert (Http.Path.concat [] [] = []);
  assert (Http.Path.concat [""] [] = [""]);
  assert (Http.Path.concat [] [""] = [""]);
  assert (Http.Path.concat [""] [""] = [""]);
  assert (Http.Path.concat [] ["a"] = ["a"]);
  assert (Http.Path.concat ["a"] [] = ["a"]);
  assert (Http.Path.concat ["a"] [""] = ["a"; ""]);
  assert (Http.Path.concat ["a"; ""] [""] = ["a"; ""]);
  assert (Http.Path.concat ["a"; "b"; ""] [] = ["a"; "b"; ""]);
  assert (Http.Path.concat ["a"; "b"; ""] [""] = ["a"; "b"; ""]);
  assert (Http.Path.concat ["a"; "b"; ""] [""; "c"] = ["a"; "b"; ""; "c"]);
  log "Webs.Http.Path.undot_and_compress";
  assert (Http.Path.undot_and_compress ["a"; "b"; "."] = ["a"; "b"; ""]);
  assert (Http.Path.undot_and_compress
            ["a"; "."; "b"; "."; "."] = ["a"; "b"; ""]);
  assert (Http.Path.undot_and_compress [".."] = [""]);
  log "Webs.Http.Path.relativize";
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

let test_digits () =
  log "Webs.Http.Digit.{decode,encode}";
  let overflow = (Format.asprintf "%d0" max_int) in
  assert (Http.Digits.decode "0" = Ok 0);
  assert (Http.Digits.decode "42" = Ok 42);
  assert (Http.Digits.decode "042" = Ok 42);
  assert (Http.Digits.decode "1024" = Ok 1024);
  assert (Result.is_error @@ Http.Digits.decode overflow);
  assert (Result.is_error @@ Http.Digits.decode "");
  assert (Result.is_error @@ Http.Digits.decode "-1");
  assert (Http.Digits.encode 0 = "0");
  assert (Http.Digits.encode 42 = "42");
  assert (Http.Digits.encode 1024 = "1024");
  raises_invalid (fun () -> Http.Digits.encode (-1));
  raises_invalid (fun () -> Http.Digits.encode min_int);
  ()

let test_ranges () =
  log "Webs.Http.Range.decode";
  let r0_499 = `Range (0, 499) in
  let r500_999 = `Range (500, 999) in
  let last500 = `Last 500 in
  let fst9500 = `First 9500 in
  let ok l = Ok (`Bytes l) in
  assert (Http.Range.decode "bytes=0-499" = ok [r0_499]);
  assert (Http.Range.decode "bytes=0-499,500-999" = ok [r0_499;r500_999]);
  assert (Http.Range.decode "bytes=-500" = ok [last500]);
  assert (Http.Range.decode "bytes=9500-" = ok [fst9500]);
  assert (Http.Range.decode "bytes=0-0,-1" = ok [`Range (0, 0); `Last 1]);
  assert (Result.is_error @@ Http.Range.decode "bytes=2-1");
  assert (Result.is_error @@ Http.Range.decode "by tes=1-2");
  assert (Http.Range.decode "unit=1-2" = Ok (`Other ("unit", "1-2")));
  ()

let test_etags () =
  let etags t = Http.Etag.v ~weak:false t, Http.Etag.v ~weak:true t in
  let empty, w_empty = etags "" in
  let xyzzy, w_xyzzy = etags "xyzzy" in
  let r2d2xxxx, w_r2d2xxxx = etags "r2d2xxxx" in
  let c3piozzzz, w_c3piozzzz = etags "c3piozzzz" in
  log "Webs.Http.Etag.decode";
  assert (Http.Etag.decode {|"xyzzy"|} = Ok xyzzy);
  assert (Http.Etag.decode {|W/"xyzzy"|} = Ok w_xyzzy);
  assert (Http.Etag.decode {|""|} = Ok empty);
  log "Webs.Http.Etag.decode_cond";
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

let main () =
  test_version ();
  test_method ();
  test_headers_case ();
  test_path ();
  test_digits ();
  test_etags ();
  test_ranges ();
  print_endline "All tests succeeded."

let () = main ()
