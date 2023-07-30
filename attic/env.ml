(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Env = struct

  (* Type identifiers see http://alan.petitepomme.net/cwn/2015.03.24.html#1 *)
  module Tid : sig
    type 'a t
    type ('a, 'b) eq = Eq : ('a, 'a) eq
    val create : unit -> 'a t
    val equal : 'a t -> 'b t -> ('a, 'b) eq option
  end = struct
    module Id = struct type _ t = .. end
    module type ID = sig type t type _ Id.t += Id : t Id.t end

    type 'a t = (module ID with type t = 'a)
    let create () (type s) =
      let module Id = struct type t = s type _ Id.t += Id : t Id.t end in
      (module Id : ID with type t = s)

    type ('a, 'b) eq = Eq : ('a, 'a) eq
    let equal (type i0) (type i1) (i0 : i0 t) (i1 : i1 t) : (i0, i1) eq option
      =
      let module I0 = (val i0 : ID with type t = i0) in
      let module I1 = (val i1 : ID with type t = i1) in
      match I0.Id with I1.Id -> Some Eq | _ -> None
  end

  module Key = struct
    type t = V : 'a typed -> t
    and 'a typed = { uid : int; tid : 'a Tid.t; untyped : t }
    let compare (V k0) (V k1) = (compare : int -> int -> int) k0.uid k1.uid
    let uid = let id = ref (-1) in fun () -> incr id; !id
    let create () =
      let rec k = { uid = uid (); tid = Tid.create (); untyped = V k } in k
  end

  module M = Map.Make (Key)
  type 'a key = 'a Key.typed
  type binding = B : 'a key * 'a -> binding
  type t = binding M.t

  let key = Key.create
  let is_empty = M.is_empty
  let empty = M.empty
  let mem k m = M.mem k.Key.untyped m
  let add k v m = M.add k.Key.untyped (B (k, v)) m
  let remove k m = M.remove k.Key.untyped m
  let find : type a. a key -> t -> a option =
  fun k m -> match M.find_opt k.untyped m with
  | None -> None
  | Some (B (k', v)) ->
      match Tid.equal k.Key.tid k'.Key.tid with
      | None -> None | Some Tid.Eq -> Some v

  let get k m = match find k m with
  | Some v -> v | None -> invalid_arg "key not found in environment"

  let override m ~by = let right _ _ v = Some v in M.union right m by
end
