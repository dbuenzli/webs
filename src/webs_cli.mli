(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Command line interface support. *)

open Cmdliner

(** {1:options Options} *)

val listener :
  ?opts:string list -> ?docs:string -> ?default_port:int ->
  ?default_listener:Webs_unix.listener -> unit -> Webs_unix.listener Term.t
(** [listener] is an option for specifying a connection listener.
    {ul
    {- [default_port] is the default port when unspecified (defaults to
       [8000]).}
    {- [default_listen] is the default listener when unspecified
       (defaults to [`Host ("localhost", default_port)]).}
    {- [docs] is the section where the option is documented.}
    {- [opts] are the options to use (defaults to [["l";"listen"]]).}} *)

val docroot : ?opts:string list -> ?docs:string -> unit -> string option Term.t
(** [docroot] is an option for specifying an optional document root.
    {ul
    {- [docs] is the section where the option is documented.}
    {- [opts] are the options to use (defaults to [["d";"docroot"]]).}} *)

val max_connections : ?opts:string list -> ?docs:string -> unit -> int Term.t
(** [max_connections] is an option for specifying the maximal amount of
    concurrent connections served. Defaults to
    {!Webs_httpc.default_max_connections}.
    {ul
    {- [docs] is the section where the option is documented.}
    {- [opts] are the options to use (defaults to [["m";"max-connections"]]. *)

(** {1:quick Quick service setup} *)

val conf_docroot : unit -> (string, string) result Term.t
(** [conf_docroot ()] requires a {!docroot} option,
    {{!Webs_unix.realpath}[realpath]}es is and logs it. *)

val quick_serve :
  ?version:string -> ?man:Cmdliner.Manpage.block list ->
  ?doc:string -> name:string -> Webs.service -> unit
(** [quick_serve ~name (Ok s)] runs [s] with the {!Webs_httpc} and handles
    a few default command line options. If you want to be able to specify
    a docroot on the command line you need to specify [with_docroot:true].
    There is no default for docroots it always needs to be specified
    explicitely. *)

val quick_serve' :
  ?version:string -> ?man:Cmdliner.Manpage.block list ->
  ?doc:string -> name:string -> conf:('a, string) result Term.t ->
  ('a -> Webs.service) -> unit
(** [quick_serve'] is like {!quick_serve} but additional configuration
    parameters can be parsed from the command line and be checked
    before installing the service. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers

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
