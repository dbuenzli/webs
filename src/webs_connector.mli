(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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
| `Trace of dur_ns * Http.Req.t option * Http.Resp.t option ]
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

(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers

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
