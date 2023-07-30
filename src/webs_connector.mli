(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Web service connector tools. *)

(** {1:log_msg Log messages}

    These message are emited by connector to track activity and
    report unexpected messages. *)

open Webs

type dur_ns = int64
(** The type for integer nanosecond duration. *)

type log_msg =
[ `Service_exn of exn * Stdlib.Printexc.raw_backtrace
| `Connector_exn of exn * Stdlib.Printexc.raw_backtrace
| `Connection_reset
| `Trace of dur_ns * Http.Request.t option * Http.Response.t option ]
(** The type for connector log messages. These *)

val no_log : log_msg -> unit
(** [no_log] is [Fun.const ()]. *)

val default_log :
  ?ppf:Format.formatter -> trace:bool -> unit -> (log_msg -> unit)
(** [default_log ~ppf ~trace] logs message on [ppf] (defaults to
    {!Format.err_formatter}) and [`Trace] messages
    iff [trace] is true. *)

val pp_log_msg : Format.formatter -> log_msg -> unit
(** [pp_log_msg] is a unspecified formatter for log messages. *)

val pp_exn_backtrace :
  kind:string ->
  Format.formatter -> exn * Printexc.raw_backtrace -> unit
(** [pp_exn_backtrace] is a formatter for exception backtraces. *)
