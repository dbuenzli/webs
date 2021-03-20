(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs_kit

let hash_vecs =
  [ "",
    "E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855";
    "a",
    "CA978112CA1BBDCAFAC231B39A23DC4DA786EFF8147C4E72B9807785AFEE48BB";
    "abc",
    "BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD";
    "message digest",
    "F7846F55CF23E14EEBEAB5B4E1550CAD5B509E3348FBC4EFA3A1413D393CB650";
    "abcdefghijklmnopqrstuvwxyz",
    "71C480DF93D6AE2F1EFAD1447C66C9525E316218CF51FC8D9ED832F2DAF18B73";
    "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
    "248D6A61D20638B8E5C026930C3E6039A33CE45964FF2167F6ECEDD419DB06C1";
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
    "DB4BFCBD4DA0CD85A60C3C37D3FBD8805C77F15FC6B1FDFE614EE0A7C8FDB4C0";
    "1234567890123456789012345678901234567890\
     1234567890123456789012345678901234567890",
    "F371BC4A311F2B009EEF952DD83CA80E2B60026C8E935592D0F9C308453C813E"
  ]

let hmac_vecs = (* from https://tools.ietf.org/html/rfc4231 *)
  [ "\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\
     \x0b\x0b\x0b\x0b",
    "\x48\x69\x20\x54\x68\x65\x72\x65",
    "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7";
    (**)
    "\x4a\x65\x66\x65",
    "\x77\x68\x61\x74\x20\x64\x6f\x20\x79\x61\x20\x77\x61\x6e\x74\x20\
     \x66\x6f\x72\x20\x6e\x6f\x74\x68\x69\x6e\x67\x3f",
    "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843";
    (**)
    "\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\
     \xaa\xaa\xaa\xaa",
    "\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\
     \xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\
     \xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\
     \xdd\xdd",
    "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe";
    (**)
    "\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10\
     \x11\x12\x13\x14\x15\x16\x17\x18\x19",
    "\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\
     \xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\
     \xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\
     \xcd\xcd",
    "82558a389a443c0ea4cc819899f2083a85f0faa3e578f8077a2e3ff46729665b";
    (**)
    "\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\
     \xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\
     \xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\
     \xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\
     \xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\
     \xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\
     \xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\
     \xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\
     \xaa\xaa\xaa",
    "\x54\x65\x73\x74\x20\x55\x73\x69\x6e\x67\x20\x4c\x61\x72\x67\x65\
     \x72\x20\x54\x68\x61\x6e\x20\x42\x6c\x6f\x63\x6b\x2d\x53\x69\x7a\
     \x65\x20\x4b\x65\x79\x20\x2d\x20\x48\x61\x73\x68\x20\x4b\x65\x79\
     \x20\x46\x69\x72\x73\x74",
    "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54";
    (**)
     "\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\
      \xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\
      \xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\
      \xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\
      \xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\
      \xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\
      \xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\
      \xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\xaa\
      \xaa\xaa\xaa",
     "\x54\x68\x69\x73\x20\x69\x73\x20\x61\x20\x74\x65\x73\x74\x20\x75\
      \x73\x69\x6e\x67\x20\x61\x20\x6c\x61\x72\x67\x65\x72\x20\x74\x68\
      \x61\x6e\x20\x62\x6c\x6f\x63\x6b\x2d\x73\x69\x7a\x65\x20\x6b\x65\
      \x79\x20\x61\x6e\x64\x20\x61\x20\x6c\x61\x72\x67\x65\x72\x20\x74\
      \x68\x61\x6e\x20\x62\x6c\x6f\x63\x6b\x2d\x73\x69\x7a\x65\x20\x64\
      \x61\x74\x61\x2e\x20\x54\x68\x65\x20\x6b\x65\x79\x20\x6e\x65\x65\
      \x64\x73\x20\x74\x6f\x20\x62\x65\x20\x68\x61\x73\x68\x65\x64\x20\
      \x62\x65\x66\x6f\x72\x65\x20\x62\x65\x69\x6e\x67\x20\x75\x73\x65\
      \x64\x20\x62\x79\x20\x74\x68\x65\x20\x48\x4d\x41\x43\x20\x61\x6c\
      \x67\x6f\x72\x69\x74\x68\x6d\x2e",
     "9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2"
  ]

let pbkdf2_hmac_vecs = (* from https://stackoverflow.com/a/5136918 *)
(* password, salt, count, key_length, output *)
  [ "password", "salt", 1, 32,
    "\x12\x0f\xb6\xcf\xfc\xf8\xb3\x2c\
      \x43\xe7\x22\x52\x56\xc4\xf8\x37\
      \xa8\x65\x48\xc9\x2c\xcc\x35\x48\
      \x08\x05\x98\x7c\xb7\x0b\xe1\x7b";
    (**)
    "password", "salt", 2, 32,
    "\xae\x4d\x0c\x95\xaf\x6b\x46\xd3\
      \x2d\x0a\xdf\xf9\x28\xf0\x6d\xd0\
      \x2a\x30\x3f\x8e\xf3\xc2\x51\xdf\
      \xd6\xe2\xd8\x5a\x95\x47\x4c\x43";
    (**)
    "password", "salt", 4096, 32,
    "\xc5\xe4\x78\xd5\x92\x88\xc8\x41\
      \xaa\x53\x0d\xb6\x84\x5c\x4c\x8d\
      \x96\x28\x93\xa0\x01\xce\x4e\x11\
      \xa4\x96\x38\x73\xaa\x98\x13\x4a";
    (**)
    "password", "salt", 16777216, 32,
    "\xcf\x81\xc6\x6f\xe8\xcf\xc0\x4d\
     \x1f\x31\xec\xb6\x5d\xab\x40\x89\
     \xf7\xf1\x79\xe8\x9b\x3b\x0b\xcb\
     \x17\xad\x10\xe3\xac\x6e\xba\x46";
    (**)
    "passwordPASSWORDpassword", "saltSALTsaltSALTsaltSALTsaltSALTsalt", 4096,
    40,
    "\x34\x8c\x89\xdb\xcb\xd3\x2b\x2f\
     \x32\xd8\x14\xb8\x11\x6e\x84\xcf\
     \x2b\x17\x34\x7e\xbc\x18\x00\x18\
     \x1c\x4e\x2a\x1f\xb8\xdd\x53\xe1\
     \xc6\x35\x51\x8c\x7d\xac\x47\xe9";
    (**)
    "pass\x00word", "sa\x00lt", 4096, 16,
    "\x89\xb6\x9d\x05\x16\xf8\x29\x89\
     \x3c\x69\x62\x26\x65\x0a\x86\x87"; ]

let test_hash () =
  print_endline "Testing hash vectors.";
  let assert_vec (msg, h_hex) =
    let h = Sha_256.of_hex h_hex |> Result.get_ok in
    let h' = Sha_256.hash msg in
    let h'_hex = Sha_256.to_hex h' in
    assert (Sha_256.equal h h');
    assert (String.equal (String.lowercase_ascii h_hex) h'_hex);
    ()
  in
  List.iter assert_vec hash_vecs

let test_hmac () =
  print_endline "Testing hmac vectors.";
  let assert_vec (k, msg, h_hex) =
    let h = Sha_256.of_hex h_hex |> Result.get_ok in
    let h' = Sha_256.hmac ~key:k msg in
    let h'_hex = Sha_256.to_hex h' in
    assert (Sha_256.equal h h');
    assert (String.equal (String.lowercase_ascii h_hex) h'_hex);
    ()
  in
  List.iter assert_vec hmac_vecs

let test_pbkdf2_hmac () =
  print_endline "Testing pbkdf2-hmac vectors.";
  let assert_vec (pass, salt, iterations, key_len, key) =
    let key' = Sha_256.pbkdf2_hmac ~key_len ~iterations ~pass ~salt () in
    assert (key = key')
  in
  List.iter assert_vec pbkdf2_hmac_vecs

let main () =
  test_hash ();
  test_hmac ();
  test_pbkdf2_hmac ();
  print_endline "All tests succeeded."

let () = main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers

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