(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/log}Log} module.

    @canonical Webs_webdriver.Wd_log *)

val name : string
(** [name] is ["log"]. *)

(** {1:types Types} *)

module Entry : sig
  type level = [ `Debug | `Error | `Info | `Warn ]
  val level_jsont : [ `Debug | `Error | `Info | `Warn ] Jsont.t

  type method' = string
  type type' =
  [ `Console of method' * Webs_wd__script.Remote_value.t list
  | `Javascript
  | `Other of string ]

  type t
  val level : t -> level
  val source : t -> Webs_wd__script.Source.t
  val text : t -> string option
  val timetamp : t -> int
  val stacktrace : t -> Webs_wd__script.Stack_trace.t option
  val type' : t -> type'
  val jsont : t Jsont.t
end

(** {1:events Events} *)

val entry_added : Entry.t Webs_wd__types.Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-log-entryAdded}
    [log.entryAdded]} *)
