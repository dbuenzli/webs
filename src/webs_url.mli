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
(** The type for schemes. *)

type authority = string
(** The type for authority. *)

type path = string
(** The type for paths. *)

type query = string
(** The type for queries (without the ['?'] seperator). *)

type fragment = string
(** The type for fragments (without the ['#'] seperator). *)

type t = string
(** The type for URLs. *)

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

val absolute : root:t -> t -> t
(** [absolute ~root url] is [url] if [kind url] is [`Abs]. Otherwise
    uses [root] to make it absolute according to its {!relative_kind}.
    The result is guaranteed to be absolute if [root] is, the result
    may be surprising or non-sensical if [root] isn't (FIXME maybe
    we should rather call that concat and make it like {!Fpath.concat}).
    {b Warning.} This doesn't resolve relative path segments. *)

(** {1:components Components} *)

val scheme : t -> scheme option
(** [scheme u] is the scheme from [u], if any. *)

val authority : t -> authority option
(** [authority u] extracts a URL authority ([HOST:PORT])
    from [u], if any. *)

val path : t -> path option
(** [path u] is the path of [u], if any. *)

val query : t -> query option
(** [query u] is the query of [u], if any. *)

val fragment : t -> fragment option
(** [fragment u] is the fragment of [u], if any. *)

val update :
  ?scheme:scheme option -> ?authority:string option ->
  ?path:path option -> ?query:query option -> ?fragment:fragment option ->
  t -> t
(** [update u] updates the specified components of [u]. If unspecified
    kept as in [u], if updated with [None] the component is deleted from [u]. *)

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
    URLs in [s] by looking in order:
    {ol
    {- For the next [href] or [src] substring then tries to parses the
       content of an HTML attribute. This may result in relative
       or absolute paths.}
    {- For next [http] substrings in [s] and then delimits an URL
       depending on the previous characters and checks that the delimited
       URL starts with [http://] or [https://].}}

    Relative URLs are made {!absolute} with [root] if provided. Otherwise
    they are kept as is. The result may have duplicates.
*)

(** {1:formatting Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats an URL. For now this is just {!Format.pp_print_string}. *)

val pp_kind : Format.formatter -> kind -> unit
(** [pp_kind] formats an unspecified representation of kinds. *)
