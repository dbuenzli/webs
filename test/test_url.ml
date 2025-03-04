(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

let eq_kind = Test.T.make ~pp:Webs.Url.pp_kind ()

let test_components =
  Test.test "Webs.Url.{kind,scheme,authority,path,query,fragment}" @@ fun () ->
  let test url k s a p q f ~__POS__  =
    let k' = Webs.Url.kind url in
    let s' = Webs.Url.scheme url in
    let a' = Webs.Url.authority url in
    let p' = Webs.Url.path url in
    let q' = Webs.Url.query url in
    let f' = Webs.Url.fragment url in
    Test.eq ~__POS__ eq_kind k' k;
    Test.(option T.string) ~__POS__ s' s;
    Test.(option T.string) ~__POS__ a' a;
    Test.(option T.string) ~__POS__ p' p;
    Test.(option T.string) ~__POS__ q' q;
    Test.(option T.string) ~__POS__ f' f;
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

let test_append =
  Test.test "Webs.Url.append" @@ fun () ->
  let test root rel res ~__POS__ =
    Test.string (Webs.Url.append root rel) res ~__POS__
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

let test_update =
  Test.test "Webs.Url.update" @@ fun () ->
  let upd ?s ?a ?p ?q ?f u u' ~__POS__ =
    let u'' =
      Webs.Url.update ?scheme:s ?authority:a ?path:p ?query:q ?fragment:f u
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

let test_authority =
  Test.test "Webs.Url.Authority.*" @@ fun () ->
  let test authority u h p ~__POS__  =
    let u' = Webs.Url.Authority.userinfo authority in
    let h' = Webs.Url.Authority.host authority in
    let p' = Webs.Url.Authority.port authority in
    Test.(option T.string) u' u ~__POS__;
    Test.string h' h ~__POS__;
    Test.(option T.int) p' p ~__POS__;
  in
  test "user:pass@example.org:3434"
    (Some "user:pass") "example.org" (Some 3434) ~__POS__;
  test "user:pass@:3434"
    (Some "user:pass") "" (Some 3434) ~__POS__;
  test "@:3434"
    (Some "") "" (Some 3434) ~__POS__;
  test "@:"
    (Some "") "" None ~__POS__;
  test "example.org:a"
    None "example.org:a" None ~__POS__;
  test ""
    None "" None ~__POS__;
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
