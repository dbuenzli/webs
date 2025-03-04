(*---------------------------------------------------------------------------
   Copyright (c) 2024 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

let test_get_random =
  Test.test "Webs_cryptorand.get_random" @@ fun () ->
  Test.string "" (Webs_cryptorand.get_random 0) ~__POS__;
  Test.int 512 (String.length (Webs_cryptorand.get_random 512)) ~__POS__;
  ()

let test_get_entropy =
  Test.test "Webs_cryptorand.get_entropy" @@ fun () ->
  Test.int 256 (String.length (Webs_cryptorand.get_entropy 256)) ~__POS__;
  Test.invalid_arg (fun () -> Webs_cryptorand.get_entropy 257) ~__POS__;
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
