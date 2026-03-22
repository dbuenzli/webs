(*---------------------------------------------------------------------------
   Copyright (c) 2024 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

let test_get_random =
  Test.test "Webs_crypto_random.get" @@ fun () ->
  Test.string "" (Webs_crypto_random.get 0) ~__POS__;
  Test.int 512 (String.length (Webs_crypto_random.get 512)) ~__POS__;
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
