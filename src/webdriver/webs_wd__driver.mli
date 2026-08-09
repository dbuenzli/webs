(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Drivers. *)

open More

val websocket_handshake :
  max_recv_message_byte_size:int ->
  url:Webs.Url.t -> (Bytesrw_websocket.t, string) result
(** [websocket_handshake ~url] performs a websocket handshake on [url].

    {b TODO.} See if we can make that part of the configuration.
    And how this would fit into allowing one to integrate its concurrency
    story. *)

(** Driver configuration.

    @canonical Webs_driver.Wd.Connection.Config *)
module Config : sig

  (** {1:tracing Tracing} *)

  (** {2:protocol Protocol} *)

  type protocol_tracer = [ `Recv | `Send ] -> string -> unit
  (** The type for protocol tracers.

      Called with the raw JSON received and sent on the WebSocket.
      This can be quite large on screenshots so you may want to
      truncate it. *)

  val protocol_tracer_nop : protocol_tracer
  (** [tracer_nop] is a protocol tracer that does nothing. *)

  val protocol_tracer_default : protocol_tracer
  (** [tracer_default] logs the first 1024 bytes of the JSON
      when {!More.Log.val-level} is [Debug]. *)

  (** {2:drop Drops} *)

  type drop_tracer =
  [ `Event of string
  | `Response of [ `Command | `Error of string ] * int ] -> unit
  (** The type for dropped event or identified responses.

      Called with method names whenever
      no continuation is found for either a received event (may
      happen if no fiber is awaiting it) or for responses
      whose identifier is unknown (may happen if you get out of {!handle}
      with unresumed contiuations or if the driver misbehaves with
      command identifiers). *)

  val drop_tracer_nop : drop_tracer
  (** [drop_tracer_nop] is a missed tracer that does nothing. *)

  val drop_tracer_default : drop_tracer
  (** [drop_tracer_default] logs the drops when {!More.Log.val-level}
      is [Info]. *)

  (** {1:conf Configurations} *)

  type t
  (** The type for driver configuration. *)

  val make :
    ?args:Cmd.t -> ?drop_tracer:drop_tracer -> ?headless:bool -> ?port:int ->
    ?protocol_tracer:protocol_tracer ->
    ?websocket_max_recv_message_byte_size:int -> unit -> t
  (** [make] is a configuration with:
      {ul
      {- [args], if applicable additional command line arguments for a
         spawn-based driver. Defaults to {!Cmd.empty}.}
      {- [drop_tracer] is a drop tracer, defaults to
         {!drop_tracer_default}}
      {- [headless], if [true] (default) run without a graphical interface.}
      {- [port], hints a network port on which the WebSocket handshake
         should be performed. Defaults choses a random port above 1023.}
      {- [protocol_tracer] is a protocol tracer, defaults to
         {!protocol_tracer_default}.}
      {- [websocket_max_recv_message_byte_size] is used with
         {!Bytesrw_websocket.make}. In contrast to there, we use {!Int.max_int},
         that is effectively unlimited, for the default.}} *)

  val args : t -> Cmd.t
  (** [args c] additional arguments added to a spawn-based driver. *)

  val headless : t -> bool
  (** [headless c] is [true] if there is desire to run without a graphical
      interface. *)

  val port : t -> int
  (** [port c] is the suggested port on which to perform the WebSocket
      handshake. *)

  val protocol_tracer : t -> protocol_tracer
  (** [protocol_tracer c] is the protocol tracer of [c]. *)

  val drop_tracer : t -> drop_tracer
  (** [drop_tracer c] is the drop tracer of [c]. *)

  val websocket_max_recv_message_byte_size : t -> int
  (** [websocket_max_recv_message_byte_size c] is the maximal received message
      byte size on the websocket. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats configuration for inspection. *)
end

module Chrome : sig
  val spawn :
    Config.t -> (Webs.Url.t * (unit -> (unit, string) result), string) result
end

module Firefox : sig
  val spawn :
    Config.t -> (Webs.Url.t * (unit -> (unit, string) result), string) result
end
