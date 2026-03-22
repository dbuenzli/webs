(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let err_equal_custom = "Cannot test custom bodies for equality"
let err_compare_custom = "Cannot order custom bodies"
let err_write_custom = "Don't know how to write custom body content"
let err_read_custom = "Don't know how to read custom body content"
let err_negative_len l = Webs__base.Fmt.str "Negative content_length (%d)" l

(* Body contents *)

type bytes_writer = eod:bool -> Bytes.Writer.t -> unit

type custom_content = ..
type content =
| Empty
| Bytes_reader of Bytes.Reader.t
| Bytes_writer of bytes_writer
| Custom of custom_content

(* Bodies *)

type t =
  { content : content;
    content_type : Webs__media_type.t;
    content_length : int option;
    finally : unit -> unit }

let make
    ?content_length ?(content_type = Webs__media_type.application_octet_stream)
    ?(finally = (fun () -> ())) content
  =
  let () = match content_length with
  | Some l when l < 0 -> invalid_arg (err_negative_len l)
  | _ -> ()
  in
  { content; content_type; content_length; finally }

let empty =
  let content_type = Webs__media_type.none in
  make ~content_length:0 ~content_type Empty

let of_custom_content ?content_length ?content_type ?finally c =
  make ?content_length ?content_type ?finally (Custom c)

let of_bytes_writer ?content_length ?content_type ?finally w =
  make ?content_length ?content_type ?finally (Bytes_writer w)

let of_bytes_reader ?content_length ?content_type ?finally r =
  make ?content_length ?content_type ?finally (Bytes_reader r)

let of_string ?content_type s =
  let writer ~eod w =
    Bytes.Writer.write_string w s;
    if eod then Bytes.Writer.write_eod w
  in
  let content_length = String.length s in
  make ~content_length ?content_type (Bytes_writer writer)

(* Properties *)

let content b = b.content
let content_type b = b.content_type
let content_length b = b.content_length
let finally b = b.finally

(* Consuming *)

let dismiss b = b.finally ()

let write ~eod w b =
  Fun.protect ~finally:b.finally @@ fun () -> match b.content with
  | Custom _ -> invalid_arg err_write_custom
  | Empty -> if eod then Bytes.Writer.write_eod w
  | Bytes_reader r -> Bytes.Writer.write_reader ~eod w r
  | Bytes_writer writes -> writes ~eod w

let to_string b =
  Fun.protect ~finally:b.finally @@ fun () -> match b.content with
  | Custom _ -> invalid_arg err_read_custom
  | Empty -> ""
  | Bytes_reader r -> Bytes.Reader.to_string r
  | Bytes_writer writes ->
      Bytes.Writer.writes_to_string (fun w -> writes ~eod:true w)

let to_bytes_reader b = match b.content with
| Custom _ -> invalid_arg err_read_custom
| Empty -> Bytes.Reader.empty ()
| Bytes_reader r -> r
| Bytes_writer writes ->
    Bytes.Reader.of_string @@
    Bytes.Writer.writes_to_string (fun w -> writes ~eod:true w)


(* Predicates and comparisons *)

let is_empty b = match b.content with Empty -> true | _ -> false
let is_custom b = match b.content with Custom _ -> true | _ -> false

let equal b0 b1 =
  if is_custom b0 || is_custom b1
  then invalid_arg err_equal_custom
  else Repr.equal (to_string b0) (to_string b1)

let compare b0 b1 =
  if is_custom b0 || is_custom b1
  then invalid_arg err_compare_custom
  else Repr.compare (to_string b0) (to_string b1)

(* Formatting *)

let pp ppf b =
  let pp_type ppf b =
    if b.content_type = Webs__media_type.none then () else
    Webs__base.Fmt.pf ppf " type:%s" b.content_type
  in
  let pp_length ppf b = match b.content_length with
  | None -> () | Some l -> Webs__base.Fmt.pf ppf " length:%d" l
  in
  begin match b.content with
  | Empty -> Webs__base.Fmt.string ppf "<empty"
  | Bytes_reader _ -> Webs__base.Fmt.string ppf "<Bytes.Reader.t"
  | Bytes_writer _ -> Webs__base.Fmt.string ppf "<bytes_writer"
  | Custom _ -> Webs__base.Fmt.string ppf "<custom"
  end;
  pp_type ppf b; pp_length ppf b; Webs__base.Fmt.string ppf ">"
