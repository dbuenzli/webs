(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Message bodies.

    {b Important.} Bodies are ressources in the programming sense, they must
    be properly {{!consuming}consumed} or {{!dismiss}dismissed} if not used.

    @canonical Webs.Http.Body *)

open Bytesrw

(** {1:contents Body contents} *)

type bytes_writer = eod:bool -> Bytes.Writer.t -> unit
(** The type for functions writing on the given bytes writer. The bytes
    writer must write an {!Bytesrw.Bytes.Slice.eod} before returning
    if and only if [eod] is [true]. *)

type custom_content = ..
(** The type for custom body contents.

    This allows bodies to expose connector specific readable or
    writable representations. For example the
    {!Webs_unix.Fd.Writer} custom content defines a body content
    by a function that writes directly on an output file
    descriptor provided by the connector. *)

type content =
| Empty (** Empty body. *)
| Bytes_reader of Bytes.Reader.t (** Bytes reader, pulls bytes. *)
| Bytes_writer of bytes_writer (** Function that pushes bytes. *)
| Custom of custom_content (** Custom content. *)
(** The type for body contents. *)

(** {1:bodies Bodies} *)

type t
(** The type for bodies. *)

val make :
  ?content_length:int -> ?content_type:Webs__media_type.t ->
  ?finally:(unit -> unit) -> content -> t
(** [make c] is a body with content [c] and:
    {ul
    {- [content_type] the content type. Defaults to
       {!Media_type.application_octet_stream}.}
    {- [content_length] the content length in bytes, if known.
       Defaults to [None]}
    {- {!finally} is a function that is called after the body
       was {{!consuming}consumed} or {{!dismiss}dismissed}. It must not
       raise and support being called more than once.}}
    Raises [Invalid_argument] if [content_length] is negative. *)

val empty : t
(** [empty s] is a body with {!Empty} content. The {!content_type} is
    {!Media_type.none}, the {!content_length} is [0] and {!finally}
    is a nop. *)

val of_custom_content :
  ?content_length:int -> ?content_type:Webs__media_type.t ->
  ?finally:(unit -> unit) -> custom_content -> t
(** [of_custom_content c] is a body defined by the custom content [c]. *)

val of_bytes_writer :
  ?content_length:int -> ?content_type:Webs__media_type.t ->
  ?finally:(unit -> unit) -> bytes_writer -> t
(** [of_byte_writer w] is a body written by [w]. *)

val of_bytes_reader :
  ?content_length:int -> ?content_type:Webs__media_type.t ->
  ?finally:(unit -> unit) -> Bytes.Reader.t -> t
(** [of_byte_reader b] is a body from the given byte reader. *)

val of_string : ?content_type:Webs__media_type.t -> string -> t
(** [of_string s] is a body made of string [s] (uses a
    {!Byte_writer}). {!content_length} is set to the length of [s]. *)

(** {1:properties Properties} *)

val content : t -> content
(** [content b] is the content of [b]. *)

val content_type : t -> Webs__media_type.t
(** [content_type b] is the media type of [b]. *)

val content_length : t -> int option
(** [content_length b] is the content length of [b], if known. *)

val finally : t -> unit -> unit
(** [finally b] is the function called whenever the body contents has been
    consumed. Unless otherwise noted this is called automatically by
    {{!consuming}consuming functions}. *)

(** {1:consuming Consuming} *)

val dismiss : t -> unit
(** [dismiss b] must be called if the body will not be consumed.
    {!finally} gets called. *)

val write : eod:bool -> Bytes.Writer.t -> t -> unit
(** [write ~eod w body] writes the body on [w]. Raises [Invalid_argument]
    on {!Custom} content. {!finally} gets called in all cases. *)

val to_string : t -> string
(** [to_string b] reads the body to a string. Raises [Invalid_argument]
    on  {!Custom} content. {!finally} gets called in all cases. *)

val to_bytes_reader : t -> Bytes.Reader.t
(** [to_bytes_reader b] is a bytes reader on the body [b]. It works on
    {!Bytes_writer}s but entails a full copy in memory. Raises
    [Invalid_argument] on {!Custom} content.

    {b Important.} After the reader has returned {!Bytesrw.Bytes.Slice.eod} it
    is the client's duty to call [finally b ()]. *)

(** {1:predicates Predicates and comparisons} *)

val is_empty : t -> bool
(** [is_empty b] is [true] iff [content b] is {!Empty}. Note that
    this does not rule out a writer that doesn't write any data. *)

val is_custom : t -> bool
(** [is_custom b] is [true] iff [content b] is {!Custom}. *)

val equal : t -> t -> bool
(** [equal] tests bodies for equality by comparing the result of
    {!to_string} with {!Stdlib.Repr.equal}. Raises [Invalid_argument]
    if any of the argument is {!is_custom}. *)

val compare : t -> t -> int
(** [compare] totally orders bodies by totaly ordering the result of
    {!to_string} with {!Stdlib.Repr.compare}. Raises
    [Invalid_argument] if any of the argument is {!is_custom}. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats bodies for inspection. It guarantees not to
    touch the content. *)
