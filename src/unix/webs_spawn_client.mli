(*---------------------------------------------------------------------------
   Copyright (c) 2024 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Client request by spawning.

    This client connector makes client HTTP requests by spawning to
    externals tools with {!Unix.create_process}.

    It is convenient ([curl] is everywhere) and can bring you a long
    way. But it is only really suitable for scripting and lightweight
    program needs. Here are a few limitations:

    {ul
    {- You need to make sure your program has one of thse tools in your [PATH]:
       [curl]}
    {- It reads request and response bodies in memory. TODO we can
       easily do better when we integrate [bytesrw]}} *)

type tool = string
(** The type for executable tools. If the string has no file separator
    it is looked up in [PATH]. *)

type cmd =
  [ `Curl of tool * string list (** The tool and pre-specified arguments. *) ]
(** The type for cli tools to use. *)

val default_cmd : cmd
(** [default_cmd] is [`Curl ("curl", ["-s" (* silent *)])]. *)

val make :
  ?trace:(int -> string array -> unit) ->
  ?cmd:cmd -> ?insecure:bool -> unit -> (Webs.Http_client.t, string) result
(** [make ~search ~cmd ()] is an HTTP client using [cmd] (defaults to
    {!default_cmd}). If [insecure] is [true], TLS certificates are not
    checked (defaults to [false]). This errors if the tool of [cmd]
    can't be found in the [PATH].

    [trace] is called with the PID and the actual command line arguments
    used with {!Unix.open_process_args}. *)

(** {1:tracing Tracing} *)

val stderr_tracer : int -> string array -> unit
(** [stderr_tracer] can be used with the [trace] argument of
    {!make} it traces to [stderr]. *)

val pp_trace : Format.formatter -> (int * string array) -> unit
(** [pp ppf] formats traces on [ppf]. *)
