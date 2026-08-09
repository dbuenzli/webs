(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open More
open Webs_wd__types

(** Command or protocol errors.

    @canonical Webs_webdriver.Wd.Error *)
module Error : sig

  module Response = Webs_wd__types.Error_response

  type t =
  | Response of Response.t
  (** An error from a command invocation. *)
  | Protocol of string
  (** A protocol error, this should not happen. Either a WebSocket
      or JSON codec error. *)
  | Connection_closing
  (** The is error is raised by command calls or {!Event.await} if
      {!Connection.close} is called on the connection. *)
  (** The type for WebDriver errors. *)

  val to_string : t -> string
  (** [to_string e] formats [e] with {!pp} to a string. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats errors for inspection. *)
end

module Exn : sig
  exception Error of Error.t
end

(** WebDriver connections.

    A WebDriver connection abstracts over driver process spawns (if any) and the
    WebSocket-based communication protocol.

    @canonical Webs_webdriver.Wd.Connection *)
module Connection : sig

  module Config = Webs_wd__driver.Config

  type custom =
    Config.t -> (Webs.Url.t * (unit -> (unit, string) result), string) result
  (** The type for custom drivers.

      Given a configuration which the driver {b should} try to abide
      by, possibly erroring if it can't, the function must return an URL on
      which to perform the WebSocket handshake and a function to invoke on
      {!close} to teardown the driver after the WebSocket has been closed. *)

  type driver =
  | Chrome
  | Edge
  | Firefox
  | Safari
  | Custom of custom (** *)
  (** The type for browser drivers. *)

  type t
  (** The type for WebDriver BiDi connections. *)

  val open' : ?config:Config.t -> driver -> (t, string) result
  (** [open' driver] opens a connection using driver [driver]. Connections
      are ressources, they must eventually be closed by {!close}.
      [config] defaults to {!Config.make}[ ()]. *)

  val close : t -> (unit, string) result
  (** [close c] closes the connection. The function does not raise. *)

  val handle : t -> (unit -> 'a) -> 'a
  (** [handle c f] handles the command and event asynchrony of the protocol
      in function [f], any use of a connection [c] must be wrapped by
      this function. *)

  val with_open : ?config:Config.t -> driver -> (t -> 'a) -> ('a, string) result
  (** [with_open driver f] executes [handle c (fun () -> f c)] with [c] an
      open connection on [driver]. If [f] raises
      {!Webs_webdriver.Wd.exception-Error} it is turned into an
      [Error] value. *)

  (** {1:properties Properties} *)

  val config : t -> Config.t
  (** [config c] is the configuration that was used when opening [c]. *)

  val driver : t -> driver
  (** [driver c] is the driver of the underlying connection. *)

  val url : t -> Webs.Url.t
  (** [url c] is the URL on which the WebSocket handshake was performed. *)
end

val call :
  ?exts:Exts.t -> Connection.t -> ('a Command.instance, 'b) Command.t ->
  'a -> 'b

val await_event : 'a Event.t -> 'a
