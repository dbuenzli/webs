(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command line interface support. *)

open Cmdliner

(** {1:options Options} *)

val listener :
  ?opts:string list -> ?docs:string -> ?default_port:int ->
  ?default_listener:Webs_listener.t -> unit -> Webs_listener.t Term.t
(** [listener] is an option for specifying a connection listener.
    {ul
    {- [default_port] is the default port when unspecified (defaults to
       [8000]).}
    {- [default_listen] is the default listener when unspecified
       (defaults to [`Host ("localhost", default_port)]).}
    {- [docs] is the manual section where the options are documented.}
    {- [opts] are the options to use (defaults to [["l";"listen"]]).}} *)

val service_path :
  ?opts:string list -> ?docs:string -> unit -> Webs.Http.Path.t option Term.t
(** [service_path ()] is an option for specifying an optional service path.
    {ul
    {- [docs] is the manual sectino where the options are documented}
    {- [opts] are the options to use (defaults to ["service-path"])}} *)

val docroot : ?opts:string list -> ?docs:string -> unit -> string option Term.t
(** [docroot] is an option for specifying an optional directory for
    serving files.
    {ul
    {- [docs] is the section where the option are documented.}
    {- [opts] are the options to use (defaults to [["d";"docroot"]]).}} *)

val max_connections : ?opts:string list -> ?docs:string -> unit -> int Term.t
(** [max_connections] is an option for specifying the maximal amount of
    concurrent connections served. Defaults to
    {!Webs_http11_gateway.default_max_connections}.
    {ul
    {- [docs] is the section where the option is documented.}
    {- [opts] are the options to use (defaults to
       [["m";"max-connections"]])}} *)
