(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** WebDriver browser automation.

    This module implements the client side of the
    {{:https://www.w3.org/TR/webdriver-bidi/}
    WebDriver BiDi Protocol}.

    Read the {{!limitations}limitations} and the {{!todo}TODO}.

    Open the module to use it.

    {b Warning.} Any function taking a {{!Wd.Connection.t}connection} argument,
    except {!Wd.Connection.with_open}, may raise {!Wd.exception-Error}.
    Besides issuing commands or waiting for events must be performed under
    a {!Wd.Connection.handle} bracket. *)

(** {1:webdriver Webdriver} *)

(** Webdriver connections and commonalities. *)
module Wd : sig

  (** {1:connections WebDriver connections} *)

  module Error = Webs_wd__protocol.Error

  exception Error of Error.t
  (** The exception for WebDriver errors. *)

  module Connection = Webs_wd__protocol.Connection

  (** Events. *)
  module Event : sig
    type 'a t = 'a Webs_wd__types.Event.t
    (** The type for events with payload of type ['a] *)

    val name : 'a t -> string
    (** [name ev] is the name of the event. Can be used
        with {!Wd_session.subscribe}. *)

    val await : 'a t -> 'a
    (** [await ev] waits for the next occurence of [ev] (which may never
        happen). *)

    (**/**)
    val define : string -> params_jsont:'a Jsont.t -> 'a t
    (** [define name ~params_jsont] is an event whose method name is [name]
        and parameters are decoded with [params_jsont]. *)
  end

  (** {1:types Types} *)

  module Exts = Webs_wd__types.Exts
  module Empty_params = Webs_wd__types.Empty_params
  module Empty_result = Webs_wd__types.Empty_result

  (** The following types are defined here rather than in their own
      module to work around WebDriver module recursive definitions. *)

  module User_context = Webs_wd__types.User_context
  module Browsing_context = Webs_wd__types.Browsing_context

  (** {1:debug_helpers Debug helpers} *)

  val pp_json : 'a Jsont.t -> Format.formatter -> 'a -> unit
end

(** {1:bidi_modules {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules}WebDriver modules}} *)

module Wd_session = Webs_wd__session
module Wd_browser = Webs_wd__browser
module Wd_browsing_context = Webs_wd__browsing_context
module Wd_emulation = Webs_wd__emulation
module Wd_network = Webs_wd__network
module Wd_script = Webs_wd__script
module Wd_storage = Webs_wd__storage
module Wd_log = Webs_wd__log
module Wd_input = Webs_wd__input
module Wd_web_extension = Webs_wd__web_extension

(** {1:limitations Limitations}

    {ul
    {- Based on the 2026-05-11 specification, may need adjustements in the
       future, it seems no diffs are published :-(
       Also in case of problems consult the browser
       {{:https://wpt.fyi/results/webdriver/tests/bidi}
       implementation status}, not everything is implemented.}
    {- {b 32-bit platforms.}
       The representation of the specification's
       {{:https://www.w3.org/TR/webdriver-bidi/#cddl-type-js-int}integer and unsigned integers} by OCaml [int] values may quickly overflow on 32-bit
       platforms. Errors are reported but that may hamper using the module on
       these platforms.}
    {- {b Extensions.} In general our descriptions using [Jsont] types
       do not error on unknown members. Sometimes these are surfaced
       as {!Wd.Exts} types but not always, especially if not
       indicated in the specification.}
    {- {b Low-level.} The interface is purposedly low-level and matches
       what the protocol offers. Nicer abstractions should be built
       on top of this.}} *)

(** {1:todo TODO}

    {ul
    {- Add a driver configuration option to redirect std outputs
       of spawn}
    {- Add a driver discovery function or make the driver argument
       optional in {!Webs_webdriver.Wd.Connection}.}
    {- The driver spawn story is still a bit shady. Only FF is well defined
       for now, but it's already one browser we can use for end-to-end
       testing.}
    {- Concurrency story.}
    {- Adjust the event processing story. It's nice not to have
       direct style but the current scheme is too racy. Since subscription
       is explicit we could record the subscriptions and buffer those
       events who match them in a ring buffer. If we don't want to
       meddle with subscriptions we could perhaps have [await [`Next | `Step]]
       to indicate with [`Step] indicating we are interested in awaiting
       future events. Though meddling with subscriptions could allow
       to dicontinue awaits on unsubscribe.}
    {- The documentation both points into MDN and the spec which
       is a bit annoying. Very little is however documented on MDN
       should we only point on the spec (which is dry to the extreme)?}
    {- Get rid of [More]? Bof, it's already a lot of code (boilerplate though),
       not keen on adding more}
    {- A few internal cleanups if we get
       {{:https://github.com/dbuenzli/jsont/issues/15}this} in [jsont].
       Search the codebase for that URL.}
    {- Make a nicer high-level API for webapp snapshot end-to-end testing.
       But in a separate library.}
    {- Explain event processing, mention the drop log}
    {- Add a simple example}} *)
