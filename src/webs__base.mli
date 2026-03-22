(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Basic definitions. *)

(** {1:preliminaries Preliminaries} *)

val string_lowercase : string -> string
val string_subrange : ?first:int -> ?last:int -> string -> string

module String_map : Map.S
  with type 'a t = 'a Map.Make(String).t
   and type key = string

module Fmt : sig
  type 'a t = Format.formatter -> 'a -> unit
  val str : ('a, Format.formatter, unit, string) format4 -> 'a
  val pf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
  val invalid_arg : ('a, Format.formatter, unit, 'b) format4 -> 'a
  val failwith : ('a, Format.formatter, unit, 'b) format4 -> 'a
  val error : ('a, Format.formatter, unit, ('b, string) result) format4 -> 'a
  val cut : unit t
  val nl : unit t
  val string : string t
  val qstring : string t
  val field : string -> 'a t -> 'a t
  val list : ?pp_sep:unit t -> 'a t -> 'a list t
  val exn_backtrace : kind:string -> (exn * Printexc.raw_backtrace) t
end

module Fpath : sig
  type t = string
  type file_ext = string
  val get_ext : string -> string
end

(** {1:basic_codec Basic HTTP codecing} *)

val err_empty_string : string
val err_token : string -> string

val crlf : string
val is_token_char : char -> bool
val is_token : string -> bool
val lower_token_of_string : string -> string

val is_ows : char -> bool
val trim_ows : string -> string
val decode_sp : bytes -> first:int -> max:int -> int

val is_digit : char -> bool
val digit_of_int : int -> char
val digit_to_int : char -> int

val decode_token : bytes -> first:int -> max:int -> int * string
val decode_field_value :  bytes -> first:int -> last:int -> string
val decode_header_field : bytes -> first:int -> crlf:int -> string * string
