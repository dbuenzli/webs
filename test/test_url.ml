(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

let eq_kind = Test.Eq.make ~pp:Webs_url.pp_kind ()

let test_components () =
  Test.test "Webs_url.{kind,scheme,authority,path,query,fragment}" @@ fun () ->
  let test url k s a p q f ~__POS__  =
    let k' = Webs_url.kind url in
    let s' = Webs_url.scheme url in
    let a' = Webs_url.authority url in
    let p' = Webs_url.path url in
    let q' = Webs_url.query url in
    let f' = Webs_url.fragment url in
    Test.eq ~__POS__ eq_kind k' k;
    Test.option ~__POS__ ~some:Test.Eq.string s' s;
    Test.option ~__POS__ ~some:Test.Eq.string a' a;
    Test.option ~__POS__ ~some:Test.Eq.string p' p;
    Test.option ~__POS__ ~some:Test.Eq.string q' q;
    Test.option ~__POS__ ~some:Test.Eq.string f' f;
  in
  test "http://example.org:80/hey-hopla/bli" `Abs
    (Some "http") (Some "example.org:80") (Some "/hey-hopla/bli") None None
    ~__POS__ ;
  test "" (`Rel `Empty) None None None None None
    ~__POS__;
  test "//example.org:80/hey-hopla/bli" (`Rel `Scheme)
    None (Some "example.org:80") (Some "/hey-hopla/bli") None None
    ~__POS__;
  test "/huhuhu:80/hey-hopla/bli" (`Rel `Abs_path)
    None None (Some "/huhuhu:80/hey-hopla/bli") None None
    ~__POS__;
  test "huhuhu/hey-hopla/bli" (`Rel `Rel_path)
    None None (Some "huhuhu/hey-hopla/bli") None None
    ~__POS__;
  test "hopla://example.org/?#" `Abs
    (Some "hopla") (Some "example.org") (Some "/") (Some "") (Some "")
    ~__POS__;
  test "https://example.org/hey?bla#blu" `Abs
    (Some "https") (Some "example.org") (Some "/hey") (Some "bla") (Some "blu")
    ~__POS__;
  test "https://example.org?bla#blu" `Abs
    (Some "https") (Some "example.org") None (Some "bla") (Some "blu")
    ~__POS__;
  test "https://example.org/bla#?blu" `Abs
    (Some "https") (Some "example.org") (Some "/bla") None (Some "?blu")
    ~__POS__;
  ()

let test_absolute () =
  Test.test "Webs_url.append" @@ fun () ->
  let test root rel res ~__POS__ =
    Test.string (Webs_url.append root rel) res ~__POS__
  in
  (* `Abs *)
  test "https://example.org" "https://ocaml.org" "https://ocaml.org" ~__POS__;
  (* `Rel Scheme *)
  test "https://example.org" "//example.org/b" "https://example.org/b" ~__POS__;
  test "ftp://example.org/hi" "//example.org/b" "ftp://example.org/b" ~__POS__;
  (* `Rel `Rel_path *)
  test "https://example.org/hi" "b" "https://example.org/b" ~__POS__;
  test "https://example.org/hi" "b/a" "https://example.org/b/a" ~__POS__;
  test "https://example.org/hi/" "b/a" "https://example.org/hi/b/a" ~__POS__;
  test "https://example.org" "b/a" "https://example.org/b/a" ~__POS__;
  test "https://example.org/" "b/a" "https://example.org/b/a" ~__POS__;
  (* `Rel `Abs_path *)
  test "https://example.org/hi/ha" "/b/a" "https://example.org/b/a" ~__POS__;
  test "https://example.org/" "/b/a" "https://example.org/b/a" ~__POS__;
  test "https://example.org" "/b/a" "https://example.org/b/a" ~__POS__;
  (* `Rel `Empty *)
  test "https://example.org/hey" "" "https://example.org/hey" ~__POS__;
  test "https://example.org/" "" "https://example.org/" ~__POS__;
  test "https://example.org" "" "https://example.org" ~__POS__;
  ()

let test_update () =
  Test.test "Webs_url.update" @@ fun () ->
  let upd ?s ?a ?p ?q ?f u u' ~__POS__ =
    let u'' =
      Webs_url.update ?scheme:s ?authority:a ?path:p ?query:q ?fragment:f u
    in
    Test.string u' u'' ~__POS__
  in
  upd ~s:None "https://example.org/hey?bla" "//example.org/hey?bla"
    ~__POS__;
  upd ~s:None ~a:None "https://example.org/hey?bla" "/hey?bla"
    ~__POS__;
  upd ~s:(Some "urn") ~a:None "https://example.org/hey?bla" "urn:/hey?bla"
    ~__POS__;
  upd ~f:(Some "f") "https://example.org" "https://example.org#f"
    ~__POS__;
  upd ~f:(Some "f") "https://example.org/" "https://example.org/#f"
    ~__POS__;
  upd ~q:(Some "q")
    "https://example.org/#?trick" "https://example.org/?q#?trick"
    ~__POS__;
  ()

let main () =
  Test.main @@ fun () ->
  test_components ();
  test_absolute ();
  test_update ();
  ()

let () = if !Sys.interactive then () else exit (main ())
