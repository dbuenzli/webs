(*---------------------------------------------------------------------------
   Copyright (c) 2024 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

let eq_error enc =
  let pp ppf e = Format.pp_print_string ppf (Webs_base64.error_message enc e) in
  Test.Eq.make ~equal:( = ) ~pp ()

let test_spec enc_spec decode encode =
  let error = eq_error `Base64 in
  let result ?__POS__ = Test.result' ?__POS__ ~ok:Test.Eq.string ~error in
  fun enc ~__POS__:pos ~p:dec_pad ~u:dec_unpadded ->
  Test.block ~__POS__:pos @@ fun () ->
  result (decode `Padded enc) dec_pad ~__POS__;
  begin match dec_pad with
  | Ok dec -> Test.string enc (encode `Padded dec) ~__POS__;
  | Error _ -> ()
  end;
  result (decode `Unpadded enc) dec_unpadded ~__POS__;
  begin match dec_unpadded with
  | Ok dec -> Test.string enc (encode `Unpadded dec) ~__POS__;
  | Error _ -> ()
  end;
  ()

let test_common ?__POS_:pos spec =
  Test.block ?__POS__:pos @@ fun () ->
  spec "" ~__POS__
    ~p:((Ok "") : (string, Webs_base64.error) result)
    ~u:((Ok "") : (string, Webs_base64.error) result);
  spec "MA" ~__POS__
    ~p:(Error (Invalid_length 2))
    ~u:(Ok "0");
  spec "MA=" ~__POS__
    ~p:(Error (Invalid_length 3))
    ~u:(Error (Invalid_letter ('=', 2)));
  spec "MA==" ~__POS__
    ~p:(Ok "0")
    ~u:(Error (Invalid_letter ('=', 3))) ;
  spec "MA===" ~__POS__
    ~p:(Error (Invalid_length 5))
    ~u:(Error (Invalid_length 5));
  spec "MA======" ~__POS__
    ~p:(Error (Invalid_letter ('=', 3)))
    ~u:(Error (Invalid_letter ('=', 3)));
  spec "MDE" ~__POS__
    ~p:(Error (Invalid_length 3))
    ~u:(Ok "01");
  spec "MDE=" ~__POS__
    ~p:(Ok "01")
    ~u:(Error (Invalid_letter ('=', 3)));
  spec "MDEy" ~__POS__
    ~p:(Ok "012")
    ~u:(Ok "012");
  spec "MDEyMw==" ~__POS__
    ~p:(Ok "0123")
    ~u:(Error (Invalid_letter ('=', 7)));
  spec "MDEyMw" ~__POS__
    ~p:(Error (Invalid_length 6))
    ~u:(Ok "0123");
  spec "MDEyMzQ=" ~__POS__
    ~p:(Ok "01234")
    ~u:(Error (Invalid_letter ('=', 7)));
  spec "MDEyMzQ" ~__POS__
    ~p:(Error (Invalid_length 7))
    ~u:(Ok "01234");
  spec "MDEyMzQ1" ~__POS__
    ~p:(Ok "012345")
    ~u:(Ok "012345");
  (* Test vectors from https://eprint.iacr.org/2022/361.pdf *)
  spec "SGVsbG8=" ~__POS__
    ~p:(Ok "Hello")
    ~u:(Error (Invalid_letter ('=', 7)));
  spec "SGVsbG8" ~__POS__
    ~p:(Error (Invalid_length 7))
    ~u:(Ok "Hello");
  spec "SGVsbG9=" ~__POS__
    ~p:(Error Non_canonical_encoding)
    ~u:(Error (Invalid_letter ('=', 7)));
  spec "SGVsbG9" ~__POS__
    ~p:(Error (Invalid_length 7))
    ~u:(Error Non_canonical_encoding);
  spec "SGVsbA==" ~__POS__
    ~p:(Ok "Hell")
    ~u:(Error (Invalid_letter ('=', 7)));
  spec "SGVsbA=" ~__POS__
    ~p:(Error (Invalid_length 7))
    ~u:(Error (Invalid_letter ('=', 6)));
  spec "SGVsbA" ~__POS__
    ~p:(Error (Invalid_length 6))
    ~u:(Ok "Hell");
  spec "SGVsbA====" ~__POS__
    ~p:(Error (Invalid_length 10))
    ~u:(Error (Invalid_letter ('=', 7 (* due to implementation not 6 *))));
  ()

let test_base64 () =
  Test.test "Webs_base64.{decode,encode}" @@ fun () ->
  let spec = test_spec `Base64 Webs_base64.decode' Webs_base64.encode in
  test_common spec;
  spec "8J+Qq/CfkKs=" ~__POS__
    ~p:(Ok "🐫🐫")
    ~u:(Error (Invalid_letter ('=', 11)));
  spec "8J+Qq/CfkKs" ~__POS__
    ~p:(Error (Invalid_length 11))
    ~u:(Ok "🐫🐫");
  ()

let test_base64url () =
  Test.test "Webs_base64.{decode,encode}_base64url" @@ fun () ->
  let spec =
    test_spec
      `Base64url Webs_base64.decode_base64url' Webs_base64.encode_base64url
  in
  test_common spec;
  spec "8J-Qq_CfkKs=" ~__POS__
    ~p:(Ok "🐫🐫")
    ~u:(Error (Invalid_letter ('=', 11)));
  spec "8J-Qq_CfkKs" ~__POS__
    ~p:(Error (Invalid_length 11))
    ~u:(Ok "🐫🐫");
  ()

let main () =
  Test.main @@ fun () ->
  test_base64 ();
  test_base64url ();
  ()

let () = if !Sys.interactive then () else exit (main ())
