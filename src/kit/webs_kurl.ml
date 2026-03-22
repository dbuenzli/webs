(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs

let strf = Printf.sprintf
module Imap = Map.Make (Int)

(* Bare URL requests *)

type bare =
  { method' : Http.Method.t;
    path : Http.Path.t;
    query : Http.Query.t;
    ext : string; }

let bare ?(ext = "") ?(query = Http.Query.empty) method' path =
  { method'; path; query; ext }

module Bare = struct
  type t = bare
  let v = bare
  let method' u = u.method'
  let path u = u.path
  let query u = u.query
  let ext u = u.ext
  let with_path path u = { u with path }
  let of_req ?ext r =
    let none = Http.Query.empty in
    let query =
      Option.fold ~none ~some:Http.Query.decode (Http.Request.query r)
    in
    bare ?ext ~query (Http.Request.method' r) (Http.Request.path r)

  let of_req_referer ?ext ?method' r =
    match Http.Headers.(find_or_error referer (Http.Request.headers r)) with
    | Error _ as e -> e
    | Ok ref ->
        match Http.Path.and_query_string_of_request_target ref with
        | Error e -> Error (strf "referer: %s" e)
        | Ok (path, q) ->
            let prefix = Http.Request.service_path r in
            match Http.Path.strip_prefix ~prefix path with
            | [] -> Error "Cannot strip service path from referer"
            | path ->
                let none = Http.Query.empty in
                let query = Option.fold ~none ~some:Http.Query.decode q in
                let m =
                  Option.value ~default:(Http.Request.method' r) method'
                in
                Ok (bare ?ext ~query m path)

  let pp ppf b =
    let pp_field f pp_v ppf v =
      Format.fprintf ppf "@[<h>(%s %a)@]" f pp_v v
    in
    let pp_cut = Format.pp_print_cut in
    Format.pp_open_vbox ppf 1;
    pp_field "method" Http.Method.pp ppf b.method'; pp_cut ppf ();
    pp_field "path" Http.Path.pp_dump ppf b.path; pp_cut ppf ();
    pp_field "query" Http.Query.pp ppf b.query; pp_cut ppf ();
    pp_field "ext" Format.pp_print_string ppf b.ext;
    Format.pp_close_box ppf ()
end

(* Decoder helpers *)

let allow allowed u =
  match Http.Method.constrain ~allowed (Bare.method' u) with
  | Ok _ as v -> v
  | Error ms ->
      Http.Response.method_not_allowed_405 ~allowed:(List.map fst ms) ()

(* URL request kind *)

type 'a enc = 'a -> bare
type 'a dec = bare -> ('a option, Http.Response.t) result
let ok v = Ok (Some v)
let no_match = Ok None

type root_is = [`Dir of string option | `File]
type 'a kind =
  { name : string; uid : int; root_is : root_is; enc : 'a enc; dec : 'a dec; }

let uid = let id = ref (-1) in fun () -> incr id; !id
let kind ?(root_is = `File) ?(name = "") enc dec =
  let uid = uid () in { name; uid; root_is; enc; dec }

module Kind = struct
  type nonrec root_is = root_is
  type 'a t = 'a kind
  let v = kind
  let name k = k.name
  let uid k = k.uid
  let root_is k = k.root_is
  let enc k = k.enc
  let dec k = k.dec
  let equal s0 s1 = Int.equal s0.uid s1.uid
  let bare ?root_is ?name () =
    let enc = Fun.id in
    let dec u = Ok (Some u) in
    kind ?root_is ?name enc dec
end

let any = Kind.bare ~name:"any" ~root_is:(`Dir (Some "index")) ()

(* Kinded url requests *)

type t = V : 'a kind * 'a -> t
let v k u = V (k, u)

(* Services *)

type 'a service = Service : 'b kind * ('b -> 'a) -> 'a service
let service k f = Service (k, f)
let map_service g (Service (k, f)) = Service (k, fun v -> g (f v))

(* Service trees *)

(* For simplifying path matching in service tree and URL formatter we
   never allow path with empty segments or the empty list as binding paths.

   Besides in the trie, the root path is mapped internally to the empty list
   and when computing the remainder of path matching we never allow it to be
   the empty list, we always map it to the root.

   This explains some of the shady "patches" that occur in Trie. *)

module Trie : sig
  type 'a t
  val is_empty : 'a t -> bool
  val empty : 'a t
  val services : 'a t -> 'a service list
  val add : Http.Path.t -> 'a service -> 'a t -> 'a t
  val rem : Http.Path.t -> 'a service -> 'a t -> 'a t
  val rem_all : Http.Path.t -> 'a t -> 'a t * 'a service list
  val find_exact : Http.Path.t -> 'a t -> 'a t
  val find_first :
    Http.Path.t -> 'a t -> [`More | `Last] * 'a t * Http.Path.t(* remainder *)

  val fold_paths :
    (Http.Path.t -> 'a service list -> 'b -> 'b) -> 'a t -> 'b -> 'b
end = struct
  module Smap = Map.Make (String)
  type 'a t = { services : 'a service list; succs : 'a t Smap.t }
  let empty = { services = []; succs = Smap.empty }
  let is_empty t = t.services = [] && Smap.is_empty t.succs
  let services t = t.services

  let[@inline] normalize_path p = if p = [""] (* root *) then [] else p
  let[@inline] denormalize_path p = if p = [] then [""] (* root *) else p

  let add p s t =
    let rec loop t s = function
    | [] -> { t with services = s :: t.services }
    | seg :: segs ->
        let t' = match Smap.find_opt seg t.succs with
        | None -> empty | Some s -> s
        in
        let succs = Smap.add seg (loop t' s segs) t.succs in
        { t with succs }
    in
    loop t s (normalize_path p)

  let rem p s t =
    let rec loop t (Service (k, _)) = function
    | [] ->
        let is_not_s (Service (k', _)) = not (Kind.equal k k') in
        let services = List.filter is_not_s t.services in
        { t with services }
    | seg :: segs ->
        match Smap.find_opt seg t.succs with
        | None -> t
        | Some t' ->
            let t'' = loop t' s segs in
            let succs = match is_empty t'' with
            | true -> Smap.remove seg t.succs
            | false -> Smap.add seg t'' t.succs
            in
            { t with succs }
    in
    loop t s (normalize_path p)

  let rem_all p t =
    let rec loop t = function
    | [] -> { t with services = [] }, t.services
    | seg :: segs ->
        match Smap.find_opt seg t.succs with
        | None -> t, []
        | Some t' ->
            let t'', services = loop t' segs in
            let succs = match is_empty t'' with
            | true -> Smap.remove seg t.succs
            | false -> Smap.add seg t'' t.succs
            in
            { t with succs }, services
    in
    loop t (normalize_path p)

  let find_exact p t =
    let rec loop t = function
    | [] -> t
    | seg :: segs ->
        match Smap.find_opt seg t.succs with
        | None -> empty
        | Some t -> loop t segs
    in
    loop t (normalize_path p)

  let find_first p t = (* stops at the first non-empty node. *)
    let rec loop t = function
    | [] -> `Last, t, [""] (* Give it a root. *)
    | seg :: segs as p ->
        match Smap.find_opt seg t.succs with
        | None -> `Last, t, p
        | Some t -> if t.services = [] then loop t segs else `More, t, segs
    in
    loop t (normalize_path p)

  let fold_paths f t acc =
    let rec loop rev_segs f t acc =
      let fold_succ seg = loop (seg :: rev_segs) f in
      let acc' =
        let p = denormalize_path (List.rev rev_segs) in
        f p t.services acc
      in
      Smap.fold fold_succ t.succs acc'
    in
    loop [] f t acc
end

let check_bind_path = function
| [""] -> ()
| [] -> invalid_arg "bind path is empty"
| p when List.exists (String.equal "") p ->
    let p = Http.Path.encode p in
    invalid_arg (strf "bind path %s has empty segments" p)
| p -> ()

let err_already_bound p kind p' =
  let n = if kind.name <> "" then " " ^ kind.name else "" in
  let p = Http.Path.encode p and p' = Http.Path.encode p' in
  invalid_arg (strf "cannot bind kind%s to %s: already bound at %s" p n p')

type 'a tree =
  { kind_paths : Http.Path.t Imap.t; (* mapped by kind id *)
    trie : 'a Trie.t }

let empty () = { kind_paths = Imap.empty; trie = Trie.empty }

let bind p (Service (k, _) as s) t =
  check_bind_path p;
  match Imap.find_opt k.uid t.kind_paths with
  | Some p' -> err_already_bound p k p'
  | None ->
      { kind_paths = Imap.add k.uid p t.kind_paths;
        trie = Trie.add p s t.trie; }

let unbind_service (Service (k, _) as s) t =
  match Imap.find_opt k.uid t.kind_paths with
  | None -> t
  | Some prefix ->
      { kind_paths = Imap.remove k.uid t.kind_paths;
        trie = Trie.rem prefix s t.trie; }

let unbind_path p t =
  let unbind_c acc (Service (k, _)) = Imap.remove k.uid acc in
  let trie, services = Trie.rem_all p t.trie in
  let kind_paths = List.fold_left unbind_c t.kind_paths services in
  { kind_paths; trie }

let service_path (Service (k, _)) t = Imap.find_opt k.uid t.kind_paths
let path_services p t = Trie.services (Trie.find_exact p t.trie)
let fold_paths f t acc  = Trie.fold_paths f t.trie acc

(* Request handling *)

let try_service u (Service (k, func)) = match k.dec u with
| Ok (Some v) -> Some (Ok (Some (func v)))
| Ok None -> None
| Error _ as v -> Some v

let find_service t u =
  let rec loop u trie p =
    let kont, trie, p = Trie.find_first p trie in
    let services = Trie.services trie in
    let u = Bare.with_path p u in
    match List.find_map (try_service u) services with
    | Some res -> res
    | None -> match kont with `More -> loop u trie p | `Last -> Ok None
  in
  loop u t.trie (Bare.path u)

(* URL request formatter *)

type fmt =
  { disable_rel : bool;
    use_exts : bool;
    scheme : string;
    authority : string;
    root : Http.Path.t;
    kind_paths : Http.Path.t Imap.t; (* mapped by kind id *)  }

module Fmt = struct
  type kurl = t
  type t = fmt

  let empty
      ?(disable_rel = false) ?(use_exts = false) ?(scheme = "")
      ?(authority = "") ~root ()
    =
    let kind_paths = Imap.add any.uid [""] Imap.empty in
    { disable_rel; use_exts; scheme; authority; root; kind_paths }

  let with_fmt ?disable_rel ?use_exts ?scheme ?authority ?root uf =
    { disable_rel = Option.value ~default:uf.disable_rel disable_rel;
      use_exts = Option.value ~default:uf.use_exts use_exts;
      scheme = Option.value ~default:uf.scheme scheme;
      authority = Option.value ~default:uf.authority authority;
      root = Option.value ~default:uf.root root;
      kind_paths = uf.kind_paths }

  let scheme uf = uf.scheme
  let authority uf = uf.authority
  let root uf = uf.root
  let disable_rel uf = uf.disable_rel
  let use_exts uf = uf.use_exts

  (* Binding *)

  let imap_add p k m = match Imap.find_opt k.uid m with
  | Some p' -> err_already_bound p k p'
  | None -> Imap.add k.uid p m

  let bind p k uf =
    check_bind_path p;
    { uf with kind_paths = imap_add p k uf.kind_paths }

  let bind_tree (t : 'a tree) uf = match Imap.is_empty uf.kind_paths with
  | true -> { uf with kind_paths = t.kind_paths }
  | false ->
      let add_kind p acc (Service (k, _)) = imap_add p k acc in
      let add_kinds p ss acc = List.fold_left (add_kind p) acc ss in
      let kind_paths = fold_paths add_kinds t uf.kind_paths in
      { uf with kind_paths }

  let find sub uf = Imap.find_opt sub.uid uf.kind_paths

  (* Formatting *)

  let encode_url ?(full = false) uf u =
    let url = Http.Path.encode u.path in
    let url = if uf.use_exts then url ^ u.ext else url in
    let url =
      if not full then url else
      let is_empty s = String.equal s "" in
      let cat ss = String.concat "" ss in
      match is_empty uf.scheme with
      | true -> if is_empty uf.authority then url else cat [uf.authority; url]
      | false ->
          match is_empty uf.authority with
          | true when uf.scheme = "file" -> cat ["file://"; url]
          | true -> cat [uf.scheme; ":"; url]
          | false -> cat [uf.scheme; "://"; uf.authority; url]
    in
    let url =
      if Http.Query.is_empty u.query
      then url else String.concat "?" [url; Http.Query.encode u.query]
    in
    url

  let encode_rel_url uf u =
    let url = encode_url ~full:false uf u in
    if url = "" then "" else String.sub url 1 (String.length url - 1)

  (* Absolute *)

  let err_kind_not_bound k =
    let n = if k.name <> "" then " " ^ k.name else "" in
    invalid_arg (strf "url kind%s not bound in formatter" n)

  let bare uf (V (k, u)) =
    let u = k.enc u in
    let path, ext = match find k uf with
    | None -> err_kind_not_bound k
    | Some bind (* has no trailing slash *) ->
        match u.path with
        | [] | [""] ->
            begin match k.root_is with
            | `File -> bind, u.ext
            | `Dir (Some seg) when uf.use_exts ->
                Http.Path.concat bind [seg], u.ext
            | `Dir _ -> Http.Path.concat bind [""], "" (* drop ext *)
            end
        | p -> Http.Path.concat bind p, u.ext
    in
    let path = Http.Path.concat uf.root path in
    let path = if path = [] then [""] else path in
    Bare.v u.method' path ~query:u.query ~ext

  let req ?full uf u =
    let u = bare uf u in
    u.method', encode_url ?full uf u

  let url ?full uf u = encode_url ?full uf (bare uf u)

  (* Relative *)

  let rel_bare uf ~src ~dst =
    if uf.disable_rel then bare uf dst else
    let src = bare uf src in
    let dst = bare uf dst in
    let path = Http.Path.relative ~src:src.path ~dst:dst.path in
    Bare.v dst.method' path ~query:dst.query ~ext:dst.ext

  let rel_req uf ~src ~dst =
    if uf.disable_rel then req uf dst else
    let dst = rel_bare uf ~src ~dst in
    dst.method', encode_rel_url uf dst

  let rel_url uf ~src ~dst =
    if uf.disable_rel then url uf dst else
    encode_rel_url uf (rel_bare uf ~src ~dst)
end
