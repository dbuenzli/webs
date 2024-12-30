(*---------------------------------------------------------------------------
   Copyright (c) 2024 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Sloppy URL processing.

    URL standard{e s} are in a sorry state. This module takes a sloppy
    approach to URL processing. It only breaks URLs into their components
    and classifies them.

    {b Warning.} None of the functions here perform percent encoding or
    decoding. *)

(** {1:urls URLs} *)

type scheme = string
(** The type for schemes, without the [':'] separator. *)

type authority = string
(** The type for [HOST:PORT] authorities. *)

type path = string
(** The type for paths. *)

type query = string
(** The type for queries, without the ['?'] separator. *)

type fragment = string
(** The type for fragments, without the ['#'] seperator. *)

type t = string
(** The type for URLs. *)

val scheme : t -> scheme option
(** [scheme u] is the {!type-scheme} of [u], if any. *)

val authority : t -> authority option
(** [authority u] is the {!type-authority} of [u], if any. *)

val path : t -> path option
(** [path u] is the {!type-path} of [u], if any. *)

val query : t -> query option
(** [query u] is the {!type-query} of [u], if any. *)

val fragment : t -> fragment option
(** [fragment u] is the {!type-fragment} of [u], if any. *)

(** {1:kinds Kinds} *)

type relative_kind = [ `Scheme | `Abs_path | `Rel_path | `Empty ]
(** The type for kinds of relative references. Represents
    {{:https://www.rfc-editor.org/rfc/rfc3986.html#section-4.2}this
    alternation}. *)

type kind = [ `Abs | `Rel of relative_kind ]
(** The type for kinds of URLs. Represents this
    {{:https://www.rfc-editor.org/rfc/rfc3986.html#section-4.1} this
    alternation}. *)

val kind : t -> kind
(** [kind u] determines the kind of [u]. It decides that [u] is
    absolute if [u] starts with a
    {{:https://www.rfc-editor.org/rfc/rfc3986.html#section-3.1}scheme}
    and [:]. *)

(** {1:ops Operations} *)

val update :
  ?scheme:scheme option -> ?authority:string option ->
  ?path:path option -> ?query:query option -> ?fragment:fragment option ->
  t -> t
(** [update u] updates the specified components of [u]. If unspecified
    kept as in [u], if updated with [None] the component is deleted from [u]. *)

val append : t -> t -> t
(** [append root u] is [u] if [kind u] is [`Abs]. Otherwise
    uses [root] to make it absolute according to its {!relative_kind}.
    The result is guaranteed to be absolute if [root] is, the result
    may be surprising or non-sensical if [root] isn't (FIXME can't we
    characterize that more ?). *)

(*
val path_and_rest : t -> string option
(** [path_and_query u] extract a URL path and query part from
    [u]. *)

val drop_path_and_rest : t -> string
(** [drop_path_and_rest u] is [u] without the path and query. *)
*)

(** {1:scraping Scraping} *)

val list_of_text_scrape : ?root:t -> string -> t list
(** [list_of_text_scrape ?root s] roughly finds absolute and relative
    URLs in the ASCII compatible (including UTF-8) textual data [s] by
    looking in order:
    {ol
    {- For the next [href] or [src] substring then tries to parses the
       content of an HTML attribute. This may result in relative
       or absolute paths.}
    {- For next [http] substrings in [s] and then delimits an URL
       depending on the previous characters and checks that the delimited
       URL starts with [http://] or [https://].}}

    Relative URLs are {{!append}appended} to [root] if provided. Otherwise
    they are kept as is. The result may have duplicates.
*)

(** {1:formatting Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats an URL. For now this is just {!Format.pp_print_string}. *)

val pp_kind : Format.formatter -> kind -> unit
(** [pp_kind] formats an unspecified representation of kinds. *)
