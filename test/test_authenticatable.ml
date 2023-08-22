(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs

let alter_data s =
  let d = Bytes.of_string (Http.Base64.url_decode s |> Result.get_ok) in
  Bytes.set d (Bytes.length d - 1) '!';
  Http.Base64.url_encode (Bytes.unsafe_to_string d)

let alter_expires s =
  let d = Bytes.of_string (Http.Base64.url_decode s |> Result.get_ok) in
  Bytes.set d 32 '9';
  Http.Base64.url_encode (Bytes.unsafe_to_string d)

let test_authenticatable () =
  print_endline "Test Webs_authenticatable.";
  let k0 = Webs_authenticatable.Private_key.random_hs256 () in
  let k1 = Webs_authenticatable.Private_key.random_hs256 () in
  let data = "Try changing that." in
  let expire = Some 3 in
  let m0 = Webs_authenticatable.encode ~private_key:k0 ~expire data in
  let m1 = Webs_authenticatable.encode ~private_key:k1 ~expire:None data in
  let m0_a0 = alter_data m0 in
  let m0_a1 = alter_expires m0 in
  let decode = Webs_authenticatable.decode in
  assert (decode ~private_key:k0 ~now:None m0 = Error (`Missing_now_for 3));
  assert (decode ~private_key:k0 ~now:(Some 1) m0 = Ok (Some 3, data));
  assert (decode ~private_key:k0 ~now:(Some 3) m0 = Error (`Expired 3));
  assert (decode ~private_key:k0 ~now:(Some 4) m0 = Error (`Expired 3));
  assert (decode ~private_key:k0 ~now:(Some 1) m0_a0 = Error `Authentication);
  assert (decode ~private_key:k0 ~now:(Some 1) m0_a1 = Error `Authentication);
  assert (decode ~private_key:k0 ~now:None m0_a0 = Error `Authentication);
  assert (decode ~private_key:k0 ~now:None m0_a1 = Error `Authentication);
  assert (decode ~private_key:k1 ~now:None m0 = Error `Authentication);
  assert (decode ~private_key:k1 ~now:(Some 1) m0 = Error `Authentication);
  assert (decode ~private_key:k1 ~now:None m1 = Ok (None, data));
  assert (decode ~private_key:k1 ~now:(Some 1) m1 = Ok (None, data));
  assert (decode ~private_key:k1 ~now:(Some 9) m1 = Ok (None, data));
  ()

let main () =
  test_authenticatable ();
  print_endline "All tests succeeded."

let () = main ()
