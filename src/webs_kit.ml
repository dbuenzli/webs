(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs

let strf = Printf.sprintf
let ( let* ) = Result.bind

module Gateway = struct
  let send_file ~header _ file =
    let headers = Http.Headers.(def header file empty) in
    Ok (Resp.v Http.ok_200 ~headers ~explain:(header :> string))

  let x_accel_redirect = Http.Name.v "x-accel-redirect"
  let x_sendfile = Http.Name.v "x-sendfile"
end

module Res = struct
  module Named = struct

    (* Resolution *)

    let resolve
        ?(eq = Stdlib.( = )) ~get_res ~res_name ~res_url ~req_name ~req_id ()
      =
      let redirect name id =
        let url = res_url name id in
        let explain = "to " ^ url in
        Error (Resp.redirect ~explain Http.moved_permanently_301 url)
      in
      match get_res req_id with
      | Error _ as e -> e
      | Ok res ->
          let res_name = res_name res in
          match req_name with
          | None -> redirect res_name req_id
          | Some n -> if eq res_name n then Ok res else redirect res_name req_id

    (* Names *)

    type name = string

    let trim_and_compress c s = (* trims and compresses [c] chars in [s]. *)
      let len = String.length s in
      let clen = ref len in
      let last_was_c = ref false in
      if s.[0] = c then decr clen;
      for i = 0 to len - 1 do
        let is_c = s.[i] = c in
        if is_c && !last_was_c then decr clen;
        last_was_c := is_c;
      done;
      if s.[len - 1] = c then decr clen;
      if !clen = len then s else
      if !clen <= 0 then "" else
      let b = Bytes.create !clen in
      last_was_c := true; (* To trim initial, if any *)
      let k = ref 0 in
      for i = 0 to !clen - 1 do
        while s.[!k] = c && !last_was_c do incr k; done;
        last_was_c := s.[!k] = c;
        Bytes.set b i s.[!k];
        incr k
      done;
      Bytes.unsafe_to_string b

    let all_digits s = (* String.for_all :-( *)
      try
        for i = 0 to String.length s - 1 do
          (match s.[i] with '0' .. '9' -> () | _ -> raise_notrace Exit)
        done;
        true
      with Exit -> false

    let name_of_string s =
      if String.equal s "" then "-" else
      let tr = function
      | 'A' .. 'Z' as c -> Char.lowercase_ascii c
      | 'a' .. 'z' | '0' .. '9' as c -> c
      | c when Char.code c <= 0x7F (* US-ASCII byte *) -> '-'
      | c -> c (* UTF-8 byte *)
      in
      let s = trim_and_compress '-' (String.map tr s) in
      if all_digits s || s = "" then s ^ "-" else s
  end

  module Id = struct

    (* Errors *)

    type error = [`Overflow | `Syntax]

    let error_message = function
    | `Overflow -> "id overflow"
    | `Syntax -> "id syntax error"

    let error_to_resp e = Resp.v ~reason:(error_message e) Http.bad_request_400

    (* Identifiers *)

    type t = int

    let[@inline] is_digit = function '0' .. '9' -> true | _ -> false
    let[@inline] digit_value c = Char.code c - 0x30
    let of_string s =
      if s = "" then Error `Syntax else
      if s = "0" then Ok 0 else
      if s.[0] = '0' then Error `Syntax (* disallows leading zeros *) else
      let rec overflow_or_syntax_error k max =
        if k > max then Error `Overflow else
        if not (is_digit s.[k]) then Error `Syntax else
        overflow_or_syntax_error (k + 1) max
      in
      let rec loop k acc max =
        if k > max then Ok acc else
        let c = s.[k] in
        if not (is_digit c) then Error `Syntax else
        let acc = acc * 10 + (digit_value c) in
        if acc < 0
        then overflow_or_syntax_error (k + 1) max
        else loop (k + 1) acc max
      in
      loop 0 0 (String.length s - 1)

    let to_string id =
      if id < 0 then invalid_arg (strf "%d: negative identifier" id) else
      string_of_int id

    let decode s = Result.map_error error_to_resp (of_string s)
  end
end

module Kurl = struct
  let strf = Printf.sprintf
  module Imap = Map.Make (Int)

  (* Bare URL requests *)

  type bare =
    { meth : Http.meth; path : Http.path; query : Http.query; ext : string; }

  let bare ?(ext = "") ?(query = Http.Query.empty) meth path =
    { meth; path; query; ext }

  module Bare = struct
    type t = bare
    let v = bare
    let meth u = u.meth
    let path u = u.path
    let query u = u.query
    let ext u = u.ext
    let with_path path u = { u with path }
    let of_req ?ext r =
      let none = Http.Query.empty in
      let query = Option.fold ~none ~some:Http.Query.decode (Req.query r) in
      bare ?ext ~query (Req.meth r) (Req.path r)

    let of_req_referer ?ext ?meth r =
      match Http.Headers.find Http.referer (Req.headers r) with
      | None -> Error ("referer: not found in request")
      | Some ref ->
          match Http.Path.and_query_string_of_request_target ref with
          | Error e -> Error (strf "referer: %s" e)
          | Ok (p, q) ->
              let none = Http.Query.empty in
              let query = Option.fold ~none ~some:Http.Query.decode q in
              let meth = Option.value ~default:(Req.meth r) meth in
              Ok (bare ?ext ~query meth p)

    let pp ppf b =
      let pp_field f pp_v ppf v =
        Format.fprintf ppf "@[<h>(%s %a)@]" f pp_v v
      in
      let pp_cut = Format.pp_print_cut in
      Format.pp_open_vbox ppf 1;
      pp_field "meth" Http.Meth.pp ppf b.meth; pp_cut ppf ();
      pp_field "path" Http.Path.pp_dump ppf b.path; pp_cut ppf ();
      pp_field "query" Http.Query.pp ppf b.query; pp_cut ppf ();
      pp_field "ext" Format.pp_print_string ppf b.ext;
      Format.pp_close_box ppf ()
  end

  (* Decoder helpers *)

  module Allow = struct
    type 'a t = Http.meth * 'a
    let meths allowed u =
      let rec loop mr = function
      | m :: ms -> if (fst m) = mr then Ok (snd m) else loop mr ms
      | [] -> Resp.method_not_allowed_405 ~allowed:(List.map fst allowed) ()
      in
      loop (Bare.meth u) allowed

    let connect = `CONNECT, `CONNECT
    let delete = `DELETE, `DELETE
    let get = `GET, `GET
    let head = `HEAD, `HEAD
    let options = `OPTIONS, `OPTIONS
    let other s o = `Other s, o
    let patch = `PATCH, `PATCH
    let post = `POST, `POST
    let put = `PUT, `PUT
    let trace = `TRACE, `TRACE
  end

  (* URL request kind *)

  type 'a enc = 'a -> bare
  type 'a dec = bare -> ('a option, Resp.t) result
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
    { kind_paths : Http.path Imap.t; (* mapped by kind id *)
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
      root : Http.path;
      kind_paths : Http.path Imap.t; (* mapped by kind id *)  }

  module Fmt = struct
    type kurl = t
    type t = fmt

    let empty
        ?(disable_rel = false) ?(use_exts = false) ?(scheme = "")
        ?(authority = "") ~root ()
      =
      { disable_rel; use_exts; scheme; authority; root; kind_paths = Imap.empty}

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
      Bare.v u.meth path ~query:u.query ~ext

    let req ?full uf u =
      let u = bare uf u in
      u.meth, encode_url ?full uf u

    let url ?full uf u = encode_url ?full uf (bare uf u)

    (* Relative *)

    let rel_bare uf ~src ~dst =
      if uf.disable_rel then bare uf dst else
      let src = bare uf src in
      let dst = bare uf dst in
      let path = Http.Path.relative ~src:src.path ~dst:dst.path in
      Bare.v dst.meth path ~query:dst.query ~ext:dst.ext

    let rel_req uf ~src ~dst =
      if uf.disable_rel then req uf dst else
      let dst = rel_bare uf ~src ~dst in
      dst.meth, encode_rel_url uf dst

    let rel_url uf ~src ~dst =
      if uf.disable_rel then url uf dst else
      encode_rel_url uf (rel_bare uf ~src ~dst)
  end
end

module Sha_256 = struct
  type t = string
  let length h = String.length h
  let equal_key k0 k1 = (* constant time compare *)
    (* FIXME note sure about that maybe we should rather defer to C and
       assembly here. *)
    Sys.opaque_identity @@
    let err_key_size k0l k1l = strf "key length differ (%d and %d)" k0l k1l in
    let k0l = String.length k0 in
    let k1l = String.length k1 in
    if k0l <> k1l then invalid_arg (err_key_size k0l k1l) else
    let[@inline] byte s i = Char.code (String.unsafe_get s i) in
    let c = ref 0 in
    for i = 0 to k0l - 1 do c := !c lor (byte k0 i lxor byte k1 i) done;
    (!c = 0)

  external hash : string -> string = "ocaml_webs_sha_256"

  let hmac ~key msg = (* see https://tools.ietf.org/html/rfc2104 *)
    let hmac_B = 64 in
    let pad_and_xor ~key ~xor =
      let key_len = String.length key in
      let key = Bytes.unsafe_of_string key in
      let b = Bytes.create hmac_B in
      for i = 0 to hmac_B - 1 do
        let d = if i < key_len then Bytes.get_uint8 key i else 0 in
        Bytes.set_uint8 b i (d lxor xor)
      done;
      Bytes.unsafe_to_string b
    in
    let key = if String.length key > hmac_B then hash key else key in
    let key_xor_opad = pad_and_xor ~key ~xor:0x5c in
    let key_xor_ipad = pad_and_xor ~key ~xor:0x36 in
    hash (key_xor_opad ^ (hash (key_xor_ipad ^ msg)))

  let equal = equal_key
  let to_bytes = Fun.id
  let of_bytes s = if String.length s <> 32 then Error () else Ok s
  let to_hex h =
    let[@inline] hex_digit n =
      Char.unsafe_chr (if n < 10 then 0x30 + n else 0x57 + n)
    in
    let rec loop max h i hex k = match i > max with
    | true -> Bytes.unsafe_to_string hex
    | false ->
        let byte = Char.code h.[i] in
        Bytes.set hex k (hex_digit (byte lsr 4));
        Bytes.set hex (k + 1) (hex_digit (byte land 0xF));
        loop max h (i + 1) hex (k + 2)
    in
    let len = String.length h in
    let hex = Bytes.create (2 * len) in
    loop (len - 1) h 0 hex 0

  let of_hex hex =
    let exception Illegal of int in
    let hex_value s i = match s.[i] with
    | '0' .. '9' as c -> Char.code c - 0x30
    | 'A' .. 'F' as c -> 10 + (Char.code c - 0x41)
    | 'a' .. 'f' as c -> 10 + (Char.code c - 0x61)
    | _ -> raise_notrace (Illegal i)
    in
    match String.length hex with
    | len when len <> 64 -> Error len
    | len ->
        let rec loop max h i hex k = match i > max with
        | true -> Ok (Bytes.unsafe_to_string h)
        | false ->
            let hi = hex_value hex k and lo = hex_value hex (k + 1) in
            Bytes.set h i (Char.chr @@ (hi lsl 4) lor lo);
            loop max h (i + 1) hex (k + 2)
        in
        let s_len = len / 2 in
        let s = Bytes.create s_len in
        try loop (s_len - 1) s 0 hex 0 with Illegal i -> Error i

  let pp ppf h = Format.pp_print_string ppf (to_hex h)

  let pbkdf2_hmac ~key_len ~iterations ~pass ~salt () =
    let check_positive s v =
      if v <= 0 then invalid_arg (strf "%s not positive (%d)" s v)
    in
    check_positive "key_len" key_len;
    check_positive "iterations" iterations;
    let div_round_up x y = (x + y - 1) / y in
    let max = Int64.(mul (sub (shift_left 1L 32) 1L) 32L) in
    match Int64.unsigned_to_int max with
    | None -> invalid_arg (strf "key_len too long (%d)" key_len)
    | Some _ ->
        (* TODO avoid the hmac allocs *)
        let hlen = 32 in
        let l = div_round_up key_len hlen in
        let t = Bytes.create (l * hlen) in
        let salt_len = String.length salt in
        let u0 =
          let u0 = Bytes.create (salt_len + 4) in
          Bytes.blit_string salt 0 u0 0 (String.length salt); u0
        in
        let init_u0 i = Bytes.set_int32_be u0 salt_len (Int32.of_int i) in
        let uj = Bytes.create hlen in
        for i = 1 to l do
          init_u0 i;
          let u1 = hmac ~key:pass (Bytes.unsafe_to_string u0) in
          let ti_start = (i - 1) * hlen in
          Bytes.blit_string u1 0 t ti_start hlen;
          Bytes.blit_string u1 0 uj 0 hlen;
          for j = 2 to iterations do
            let unext = hmac ~key:pass (Bytes.unsafe_to_string uj) in
            Bytes.blit_string unext 0 uj 0 hlen;
            for k = 0 to hlen - 1 do
              Bytes.set_uint8 t (ti_start + k)
                (Bytes.get_uint8 t (ti_start + k) lxor
                 Bytes.get_uint8 (Bytes.unsafe_of_string unext) k)
            done
          done
        done;
        if key_len = Bytes.length t
        then Bytes.unsafe_to_string t
        else Bytes.sub_string t 0 key_len
end

module Authenticatable = struct

  (* Time *)

  type time = int

  (* Keys *)

  type private_key = [ `Hs256 of string ]

  let random_private_key_hs256 ?(r = Random.State.make_self_init ()) () =
    let b = Bytes.create 64 in
    for i = 0 to 63 do Bytes.set_uint8 b i (Random.State.int r 256) done;
    `Hs256 (Bytes.unsafe_to_string b)

  let private_key_to_ascii_string = function
  | `Hs256 k -> "HS256:" ^ (Http.Base64.url_encode k)

  let private_key_of_ascii_string s = match String.index_opt s ':' with
  | None -> Error (strf "missing ':' separator")
  | Some i ->
      let scheme = Http.string_subrange ~last:(i - 1) s in
      let d = Http.string_subrange ~first:(i + 1) s in
      match scheme with
      | "HS256" ->
          let* k = Http.Base64.url_decode d |> Http.Base64.error_string in
          Ok (`Hs256 k)
      | s -> Error (strf "unknown scheme: %S" scheme)

  (* Authenticatable *)

  type t = string

  (* Encode *)

  let encode ~private_key:(`Hs256 key) ~expire:e data =
    let e = match e with None -> "" | Some t -> string_of_int t in
    let msg = String.concat ":" [e; data] in
    let hmac = "HS256:" ^ Sha_256.hmac ~key msg in
    Http.Base64.url_encode (hmac ^ msg)

  (* Decode *)

  type format_error =
  [ `Base64url of Http.Base64.error
  | `Scheme of string option ]

  let format_error_message = function
  | `Scheme None -> "scheme decode error"
  | `Scheme (Some ("HS256" as s)) -> strf "scheme %s decode error" s
  | `Scheme (Some s) -> strf "unknown scheme %S" s
  | `Base64url e -> Http.Base64.error_message e

  type error =
  [ `Authentication
  | `Expired of time
  | `Missing_now_for of time
  | `Format of format_error ]

  let error_message = function
  | `Authentication -> "data not authenticated by private key"
  | `Format e -> format_error_message e
  | `Expired t -> strf "data expired at %d" t
  | `Missing_now_for t -> strf "missing current time for data expiring at %d." t

  let error_string r = Result.map_error error_message r

  let hs256 = "HS256"

  let decode_hmac s = match Http.Base64.url_decode s with
  | Error e -> Error (`Base64url e)
  | Ok s ->
      if String.length s < 6 then Error (`Scheme None) else
      let algo = Http.string_subrange ~last:4 s in
      let is_hs256 = String.equal algo hs256 && s.[5] = ':' in
      if not is_hs256 then Error (`Scheme (Some algo)) else
      if String.length s < 39 then Error (`Scheme (Some hs256)) else
      let hmac = Http.string_subrange ~first:6 ~last:37 s in
      let msg = Http.string_subrange ~first:38 s in
      Ok (hmac, msg)

  let decode_msg msg = match String.index_opt msg ':' with
  | None -> Error (`Scheme (Some hs256))
  | Some i ->
      let expire = Http.string_subrange ~last:(i - 1) msg in
      let data = Http.string_subrange ~first:(i + 1) msg in
      if String.equal expire "" then Ok (None, data) else
      match int_of_string_opt expire with
      | None -> Error (`Scheme (Some hs256))
      | Some _ as t -> Ok (t, data)

  let decode ~private_key:(`Hs256 key) ~now s = match decode_hmac s with
  | Error e -> Error (`Format e)
  | Ok (hmac, msg) ->
      let hmac' = Sha_256.hmac ~key msg in
      if not (Sha_256.equal hmac hmac') then Error `Authentication else
      match decode_msg msg, now with
      | Error e, _ -> Error (`Format e)
      | Ok (Some t, data) as r, Some now when now < t -> r
      | Ok (Some t, data), Some _ -> Error (`Expired t)
      | Ok (Some t, data), None -> Error (`Missing_now_for t)
      | Ok (None, data) as r, _ -> r

  (* Untrusted decode *)

  type untrusted = [`Untrusted_hs256 of Sha_256.t * time option * string ]
  let untrusted_decode s =
    let* hmac, msg = decode_hmac s in
    let* expire, data = decode_msg msg in
    Ok (`Untrusted_hs256 (hmac, expire, data))
end

module Authenticated_cookie = struct

  (* Setting and clearing *)

  let set ~private_key ~expire ?atts ~name data resp =
    let value = Authenticatable.encode ~private_key ~expire data in
    let cookie = Http.Cookie.encode ?atts ~name value in
    let hs = Http.Headers.add_set_cookie cookie (Resp.headers resp) in
    Resp.with_headers hs resp

  let clear ?atts ~name resp =
    let atts = Http.Cookie.atts ?init:atts ~max_age:(Some ~-1) () in
    let cookie = Http.Cookie.encode ~atts ~name "" in
    let hs = Http.Headers.add_set_cookie cookie (Resp.headers resp) in
    Resp.with_headers hs resp

  (* Getting *)

  type error = [ Authenticatable.error | `Cookie of string ]

  let error_message = function
  | `Cookie s -> s
  | #Authenticatable.error as e -> Authenticatable.error_message e

  let error_string r = Result.map_error error_message r

  let find ~private_key ~now ~name r  = match Req.find_cookie ~name r with
  | Error e -> Error (`Cookie e)
  | Ok (None | Some "") -> Ok None
  | Ok (Some c) ->
      Result.map Option.some (Authenticatable.decode ~private_key ~now c)
end

module Session = struct

  (* Session state description *)

  type 'a state =
    { eq : 'a -> 'a -> bool;
      encode : 'a -> string;
      decode : string -> ('a, string) result; }

  module State = struct
    type 'a t = 'a state
    let v ~eq ~encode ~decode () = { eq; encode; decode }
    let eq s = s.eq
    let encode s = s.encode
    let decode s = s.decode
    let option_eq st s0 s1 = match s0, s1 with
    | None, None -> true | Some s0, Some s1 -> st.eq s0 s1 | _ -> false
  end

  (* Handler *)

  type ('a, 'e) handler =
    { load : 'a state -> Req.t -> ('a option, 'e) result;
      save : 'a state -> 'a option -> Resp.t -> Resp.t }

  module Handler = struct
    type ('a, 'e) t = ('a, 'e) handler
    let v ~load ~save () = { load; save }
    let load h = h.load
    let save h = h.save
  end

  type 'a resp = 'a option * Resp.t
  type nonrec 'a result = ('a resp, 'a resp) result

  let setup sd h service = fun req ->
    let r = h.load sd req in
    let s', resp = service r req in
    match r with
    | Ok s when State.option_eq sd s s' -> resp
    | Ok _ | Error _ -> h.save sd s' resp

  (* Client stored *)

  type client_stored_error =
  [ Authenticated_cookie.error | `State_decode of string ]

  let client_stored_error_message = function
  | `State_decode e -> strf "state decode: %s" e
  | #Authenticated_cookie.error as e -> Authenticated_cookie.error_message e

  let client_stored_error_string r =
    Result.map_error client_stored_error_message r

  let client_stored ~private_key ?atts ~name () =
    let load sd r =
      match Authenticated_cookie.find ~private_key ~now:None ~name r with
      | Ok (Some (_, s)) ->
          begin match sd.decode s with
          | Error e -> Error (`State_decode e)
          | Ok v -> Ok (Some v)
          end
      | (Ok None | Error _ as v) -> v
    in
    let save sd s r = match s with
    | None -> Authenticated_cookie.clear ?atts ~name r
    | Some s ->
        let data = sd.encode s in
        Authenticated_cookie.set ?atts ~private_key ~expire:None ~name data r
    in
    Handler.v ~load ~save ()

  (* Result state injection *)

  let for_result st = function Ok v -> Ok (st, v) | Error e -> Error (st, e)
  let for_ok st = function Ok v -> Ok (st, v) | Error _ as e -> e
  let for_error st = function Ok _ as v -> v | Error e -> Error (st, e)
end

(* Basic authentication *)

module Basic_auth = struct
  type user = string
  type check =
    user:user -> pass:string -> (unit, [`User_unknown | `Wrong_password]) result

  let basic_authentication_of_string creds =
    match Http.Headers.values_of_string ~sep:' ' creds with
    | scheme :: cred :: _ ->
        begin match Http.string_lowercase scheme = "basic" with
        | false -> Error (strf "auth-scheme %s: unsupported" scheme)
        | true ->
            let* user_pass =
              Result.map_error (Fun.const "base64 decode error") @@
              Http.Base64.decode cred
            in
            match String.index_opt user_pass ':' with
            | None -> Error "no ':' found in basic credentials"
            | Some i ->
                let user = Http.string_subrange ~last:(i - 1) user_pass in
                let pass = Http.string_subrange ~first:(i + 1) user_pass in
                Ok (user, pass)
        end
    | _ -> Error ("Not a basic auth-scheme")


  let cancel = Resp.html Http.unauthorized_401 @@
{|<!DOCTYPE html>
<html lang="en">
  <head><meta charset="utf-8"><title>Login cancelled</title></head>
  <body><h1>Login cancelled</h1><a href="">Try again.</a></body>
</html>|}

  let enticate ?(cancel = fun _ -> cancel) ~check ~realm r =
    let error_401 ~explain =
      let auth = Printf.sprintf {|basic realm="%s", charset="utf-8"|} realm in
      let hs = Http.Headers.(def Http.www_authenticate auth empty) in
      let resp = Resp.with_status ~explain Http.unauthorized_401 (cancel r) in
      Error (Resp.override_headers hs resp)
    in
    match Http.Headers.find Http.authorization (Req.headers r) with
    | None -> error_401 ~explain:"No authorization header"
    | Some creds ->
        let* user, pass = match basic_authentication_of_string creds with
        | Ok _ as v -> v
        | Error explain -> Error (Resp.v ~explain Http.bad_request_400)
        in
        let* () = match check ~user ~pass with
        | Ok _ as v -> v
        | Error e ->
            let explain = match e with
            | `User_unknown -> strf "user %s: unknown" user
            | `Wrong_password -> strf "user %s: wrong password" user
            in
            error_401 ~explain
        in
        Ok (user, r)
end

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
