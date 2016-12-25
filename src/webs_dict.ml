(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
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
   Copyright (c) 2015 Daniel C. Bünzli

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
