(*---------------------------------------------------------------------------
   Copyright (c) 2024 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Webs

val make :
  ?search:Cmd.tool_search -> ?cmd:Cmd.t -> ?insecure:bool -> unit ->
  (Http_client.t, string) result
(** [make ~search ~cmd ()] looks for [cmd] (defaults to [Cmd.tool "curl"])
    in [search] (defaults to [Os.Cmd.get ~search]). *)
