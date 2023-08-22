(*---------------------------------------------------------------------------
   Copyright (c) 2023 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Quick serves and fetches. *)

(** {1:serves Quick serves}

    A few functions to quickly run your service using the
    {!Webs_http11_gateway} connector (for now). Typical usage:
{[
open Webs

let service _ = Http.Response.empty Http.Status.not_found_404
let main () = Webs_quick.serve service
let () = if !Sys.interactive then () else exit (main ())
]}
*)

val serve :
  ?version:string -> ?man:Cmdliner.Manpage.block list ->
  ?doc:string -> ?name:string ->
  (Webs.Http.Request.t -> Webs.Http.Response.t) -> int
(** [serve service] parses a few default command line options from
    {!Webs_cli} and runs service [service] with the {!Webs_http11_gateway}
    connector. Use {!serve'} if you want to be able to specify
    additional configuration options.
    {ul
    {- [version] is a version number for your program. See
       {!Cmdliner.Cmd.info}.}
    {- [man] is a manpage for your program. See {!Cmdliner.Cmd.info}.}
    {- [doc] is the synopsis of your program. See {!Cmdliner.Cmd.info}.}
    {- [name] is the name of your program. Defaults to
       {!Filename.basename} of {!Sys.executable_name}.}}

    The function returns with an exit code that you can use with
    {!Stdlib.exit}. *)

(** {2:conf With configuration} *)

val conf_docroot : unit -> (string, string) result Cmdliner.Term.t
(** [conf_docroot ()] requires a {!Webs_cli.docroot} option (yes a
    contradiction), [realpath]es it and logs it. *)

val serve' :
  ?version:string -> ?man:Cmdliner.Manpage.block list ->
  ?doc:string -> ?name:string -> conf:('a, string) result Cmdliner.Term.t ->
  ('a -> Webs.Http.Request.t -> Webs.Http.Response.t) -> int
(** [serve'] is like {!serve} but additional configuration parameters
    can be parsed from the command line and be checked before
    installing the service. *)
