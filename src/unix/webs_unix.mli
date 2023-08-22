(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Webs [Unix] tooling. *)

open Webs

(** {1:fds File descriptors} *)

(** File descriptor tools. *)
module Fd : sig

  type Http.Body.custom_content +=
  | Writer of Unix.file_descr Http.Body.writer
  (** The type for direct file descriptor body writers.

      These writers write their bodies directly on the output file
      descriptor given to them. *)

  (** {1:rw Uninterrupted reads and writes} *)

  val read : Unix.file_descr -> bytes -> start:int -> length:int -> int
  (** [read fd b ~start ~len] reads at most [len] bytes of [fd] into [b]
      starting at [start]. Raises [Unix.Unix_error] but handles [Unix.EINTR]. *)

  val write : Unix.file_descr -> bytes -> start:int -> length:int -> unit
  (** [write fd b ~start ~len] writes [len] byte of [b] starting at [start]
      on [fd]. Raises [Unix.Unix_error] but handles [Unix.EINTR]. *)

  (** {1:body Body reader and writers} *)

  val body_byte_reader :
    max_request_body_byte_size:int -> content_length:int option ->
    Unix.file_descr -> bytes -> first_start:int -> first_len:int ->
    Http.Body.byte_reader
  (** [body_byte_reader fd b ~start ~start_len] is a body byte reader for
      [fd] using buffer [b] and assuming the first [first_len] bytes
      are already in [b] at [first_start]. *)

  val body_writer : Http.Body.t -> Unix.file_descr Http.Body.writer
  (** [body_writer b] is a body writer for [b] writing on a given
      file descriptors. This supports bodies with the following content:
      {ul
      {- {!Webs.Http.Body.Empty}}
      {- {!Webs.Http.Body.Byte_writer}}
      {- {!Webs.Http.Body.Byte_reader}}
      {- {!Webs.Http.Body.Custom} supporting {!Writer}}} *)

  (** {1:http11 HTTP/1.1 support} *)

  val read_http11_head_crlfs :
    max_bytes:int -> bytes -> Unix.file_descr -> int list * int * int
  (** [read_http11_crlfs] reads the status line and headers in [bytes]
      up-to [max_bytes]. Returns the list of [crlf] locations, the
      start of the body and how much was read from the body. Raises
      [Failure] and [Unix.Unix_error]*)

  val write_http11_request : Unix.file_descr -> Http.Request.t -> unit
  (** [write_http_request fd request] writes [request] as an an
      HTTP/1.1 request on [fd]. This calls
      {!Webs.Http.Headers.for_connector} on the headers and the value
      of {!Webs.Http.Request.version} is ignored. *)
end

(** {1:http_date HTTP-date} *)

val http_date_of_posix_time_s : float -> string
(** [http_date_of_posix_time_s t] is an
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-date-time-formats}
    [HTTP-date]} of posix time [t], like returned by {!Unix.gettimeofday}. *)

(** {1:missing Missing [Unix] bindings} *)

(** {2:sendfile [sendfile(2)]} *)

val sendfile :
  src:Unix.file_descr -> off:int -> length:int -> Unix.file_descr -> int
(** [sendfile ~src ~off ~len dst] writes [length] bytes starting at [off]
    in [src] to [dst]. Raises [Unix.Unix_error], you'll also need to
    do the [Unix.EINTR] dance. Raises [Sys_error] if unsupported on
    the platform. *)

val really_sendfile :
  src:Unix.file_descr -> off:int -> length:int -> Unix.file_descr -> unit
(** [really_sendfile] is like {!val-sendfile} except it does the
    [Unix.EINTR] dance and falls back to a user space copy if
    {!val-sendfile} is unsupported on the platform. *)

(** {2:monotonic Monotonic time} *)

(** Measuring time.

    Support to measure monotonic wall-clock time. *)
module Time : sig
   (** {1:span Monotonic time spans} *)

  type span
  (** The type for non-negative monotonic time spans. They represent
      the difference between two clock readings with nanosecond precision
      (1e-9s). *)

  (** Time spans *)
  module Span : sig

    (** {1:span Time spans} *)

    type t = span
    (** See {!type:span}. *)

    val zero : span
    (** [zero] is a span of 0ns. *)

    val one : span
    (** [one] is a span of 1ns. *)

    val max : span
    (** [max_span] is a span of [2^64-1]ns. *)

    val add : span -> span -> span
    (** [add s0 s1] is [s0] + [s1]. {b Warning.} Rolls over on overflow. *)

    val abs_diff : span -> span -> span
    (** [abs_diff s0 s1] is the absolute difference between [s0] and [s1]. *)

    (** {1:preds Predicates and comparisons} *)

    val equal : span -> span -> bool
    (** [equal s0 s1] is [s0 = s1]. *)

    val compare : span -> span -> int
    (** [compare s0 s1] orders span by increasing duration. *)

    (** {1:conv Conversions} *)

    val to_uint64_ns : span -> int64
    (** [to_uint64_ns s] is [s] as an {e unsigned} 64-bit integer nanosecond
        span. *)

    val of_uint64_ns : int64 -> span
    (** [of_uint64_ns u] is the {e unsigned} 64-bit integer nanosecond span [u]
        as a span. *)

    val pp_ms : Format.formatter -> span -> unit
    (** [pp_ms ppf s] prints [s] as an unsigned 64-bit integer millisecond
        span. *)

    val pp_ns : Format.formatter -> span -> unit
    (** [pp_ns ppf s] prints [s] as an unsigned 64-bit integer nanosecond
        span. *)
  end

  (** {1:monotonic_counters Monotonic wall-clock time counters} *)

  type counter
  (** The type for monotonic wall-clock time counters. *)

  val counter : unit -> counter
  (** [counter ()] is a counter counting from now on. *)

  val count : counter -> span
  (** [count c] is the monotonic time span elapsed since [c] was created. *)
end
