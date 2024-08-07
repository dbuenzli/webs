(*---------------------------------------------------------------------------
   Copyright (c) 2024 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Sloppy URL processing.

    URL standard{e s} are in a sorry state. This module takes a sloppy
    approach to URL processing. *)

(** {1:urls URLs} *)

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

val kind : string -> kind
(** [kind u] determines the kind of [u]. It decides that [u] is
    absolute if [u] starts with a
    {{:https://www.rfc-editor.org/rfc/rfc3986.html#section-3.1}scheme}
    and [:]. *)

(** {1:components Components} *)

val scheme : string -> string option
(** [scheme u] tries to exract a URL scheme from [u]. *)

val authority : string -> string option
(** [authority u] tries to extract a URL authority ([HOST:PORT])
    part from [u]. *)

val path_and_query : string -> string option
(** [path_and_query u] tries to extract a URL path and query part from
    [u]. *)

(** {1:scraping Scraping} *)

val list_of_text_scrape : string -> string list
(** [list_of_text_scrape s] roughly finds URLs and relative or absolute
    paths in [s] by looking in order:
    {ol
    {- For the next [href] or [src] substring then tries to parses the
       content of an HTML attribute. This may result in relative
       or absolute paths.}
    {- For next [http] substrings in [s] and then delimits an URL
       depending on the previous characters and checks that the delimited
       URL starts with [http://] or [https://].}} *)
