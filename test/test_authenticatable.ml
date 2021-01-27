(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs
open Webs_kit

let alter_data s =
  let d = Bytes.of_string (Http.Base64.decode s |> Result.get_ok) in
  Bytes.set d (Bytes.length d - 1) '!';
  Http.Base64.encode (Bytes.unsafe_to_string d)

let alter_expires s =
  let d = Bytes.of_string (Http.Base64.decode s |> Result.get_ok) in
  Bytes.set d 32 '9';
  Http.Base64.encode (Bytes.unsafe_to_string d)

let test_authenticatable () =
  print_endline "Test Authenticatable.";
  let k0 = Authenticatable.random_key () in
  let k1 = Authenticatable.random_key () in
  let data = "Try changing that." in
  let expire = Some 3 in
  let m0 = Authenticatable.encode ~key:k0 ~expire data in
  let m1 = Authenticatable.encode ~key:k1 ~expire:None data in
  let m0_a0 = alter_data m0 in
  let m0_a1 = alter_expires m0 in
  let decode = Authenticatable.decode in
  assert (decode ~key:k0 ~now:1 m0 = Ok (Some 3, data));
  assert (decode ~key:k0 ~now:3 m0 = Error `Expired);
  assert (decode ~key:k0 ~now:4 m0 = Error `Expired);
  assert (decode ~key:k0 ~now:1 m0_a0 = Error `Authentication);
  assert (decode ~key:k0 ~now:1 m0_a1 = Error `Authentication);
  assert (decode ~key:k1 ~now:1 m0 = Error `Authentication);
  assert (decode ~key:k1 ~now:1 m1 = Ok (None, data));
  assert (decode ~key:k1 ~now:9 m1 = Ok (None, data));
  ()

let main () =
  test_authenticatable ();
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
