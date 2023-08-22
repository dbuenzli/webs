(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Socket connection listeners. *)

(** {1:listeners Listeners} *)

type t =
[ `Host of string * int (** Bind a socket on given hostname and port. *)
| `Sockaddr of Unix.sockaddr (** Bind a socket on given address. *)
| `Fd of Unix.file_descr
  (** Listen on that socket, the client is responsible for closing it. *) ]
(** The type for specifying listeners.  *)

val localhost_8000 : t
(** [localhost_8000] is [`Host ("localhost", 8000)]. *)

val to_fd : t -> (Unix.file_descr * bool, string) result
(** [to_fd l] is [Ok (fd, close)] with [fd] a file descriptor for the
    listener [l] and [close] is [true] if the client is in charge of
    closing it. Unless [l] was [`Fd], [fd] has
    {{!Unix.set_close_on_exec}close on exec} set to [true]. *)

val of_string : default_port:int -> string -> (t, string) result
(** [of_string s] parses a listener specification from [s]. The format
    is [ADDR[:PORT]] or [PATH] for a Unix domain
    socket. [default_port] is used if no port is specified. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats listeners for inspection. *)
