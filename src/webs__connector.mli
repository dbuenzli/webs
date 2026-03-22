(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tool for connectors.

    See the {{!page-connector_conventions}connector conventions}.

    @canonical Webs.Http.Connector *)

(** Connector log messages.

    This is a {e suggested} log message format for connectors.
    They can be emitted by connectors to track activity and report
    unexpected events. *)
module Log : sig

  type dur_ns = int64
  (** The type for integer nanosecond duration. *)

  type msg =
    [ `Service_exn of exn * Stdlib.Printexc.raw_backtrace
    | `Connector_exn of exn * Stdlib.Printexc.raw_backtrace
    | `Connection_reset
    | `Trace of dur_ns * Webs__request.t option * Webs__response.t option ]
  (** The type for connector log messages. *)

  val pp_msg : Format.formatter -> msg -> unit
  (** [pp_log_msg] is a unspecified formatter for log messages. *)

  val quiet : msg -> unit
  (** [quiet] is [Fun.const ()]. *)

  val default :
    ?ppf:Format.formatter -> trace:bool -> unit -> (msg -> unit)
    (** [default_log ~ppf ~trace] logs message on [ppf] (defaults to
        {!Format.err_formatter}) and [`Trace] messages iff [trace] is
        true. *)
end

(** Default values for connector properties. *)
module Default : sig
  val max_http_head_byte_size : int
  (** [max_http_head_byte_size] is 64k in bytes. It is the default
      maximal head size for HTTP request and response messages. *)

  val max_http_body_byte_size : int
  (** [max_http_byte_size] is 10Mo in bytes. It is the default maximal
      body size for HTTP message bodies. *)
end
