(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Dictionaries see http://mlton.org/PropertyList *)

(* Keys *)

module Key = struct
  let univ (type s) () =
    let module M = struct exception E of s option end in
    (fun x -> M.E (Some x)), (function M.E x -> x | _ -> None)

  let key_id =
    let count = ref (-1) in
    fun () -> incr count; !count

  type 'a key =
    { id : int;
      name : string;
      to_univ : 'a -> exn;
      of_univ : exn -> 'a option; }

  let create () (type v) =
    let id = key_id () in
    let to_univ, of_univ = univ () in
    { id; name = ""; to_univ; of_univ }

  type t = V : 'a key -> t
  let compare (V k0) (V k1) = (compare : int -> int -> int) k0.id k1.id
end

type 'a key = 'a Key.key
let key = Key.create

(* Dictionaries *)

module M = (Map.Make (Key) : Map.S with type key = Key.t)
type t = exn M.t

let empty = M.empty
let is_empty = M.is_empty
let mem k d = M.mem (Key.V k) d
let add k v d  = M.add (Key.V k) (k.Key.to_univ v) d
let rem k d = M.remove (Key.V k) d
let find k d = try k.Key.of_univ (M.find (Key.V k) d) with Not_found -> None
let get k d = match find k d with
| Some v -> v
| None -> invalid_arg "key unbound in dictionary"

let pp ppf d = Format.fprintf ppf "@[(dict TODO)@]"

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
