(*---------------------------------------------------------------------------
   Copyright (c) 2016 The webs programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** HTML generation.

    [Webs_html] provides combinators to generate (possibly partial)
    HTML documents.

    Open this module to use it.

    {b Warning.} The module takes care of escaping the data you
    provide but it assumes strings are UTF-8 encoded, this is not
    checked by the module. Also be careful with {{!El.style}[style]}
    elements. *)

(** HTML element attributes. *)
module At : sig

  (** {1:names Names} *)

  type name = string
  (** The type for attribute names. *)

  (** Attribute names.

      See the {{!At.section-cons}attribute constructors} for documentation. *)
  module Name : sig
    val accesskey : name
    val action : name
    val autocomplete : name
    val autofocus : name
    val charset : name
    val checked : name
    val class' : name
    val content : name
    val defer : name
    val disabled : name
    val for' : name
    val height : name
    val href : name
    val id : name
    val lang : name
    val media : name
    val method' : name
    val name : name
    val placeholder : name
    val rel : name
    val required : name
    val src : name
    val tabindex : name
    val title : name
    val type' : name
    val value : name
    val wrap : name
    val width : name
  end

  (** {1:atts Attributes} *)

  type t
  (** The type for attributes. *)

  val v : name -> string -> t
  (** [v n value] is an attribute named [n] with value [value].

      See also (and favor) {{!section-cons}attribute constructors}. *)

  val true' : name -> t
  (** [true' n] is [v n ""]. This sets the
      {{:https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#boolean-attributes}boolean attribute}
      [n] to true. The attribute must be omitted to be false. *)

  val int : name -> int -> t
  (** [int n i] is [v n (string_of_int i)]. *)

  val add_if : bool -> t -> t list -> t list
  (** [add_if c att atts] is [att :: atts] if [c] is [true] and [atts]
        otherwise. *)

  val add_if_some : name -> string option -> t list -> t list
  (** [add_if_some n o atts] is [(v n value) :: atts] if [o] is [Some
      value] and [atts] otherwise. *)

  val to_pair : t -> string * string
  (** [to_pair at] is [(n,v)] the name and value of the attribute. *)

  (** {1:cons Constructors}

      See the
      {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes}MDN
      HTML attribute reference}.

      {b Convention.} Whenever an attribute name conflicts with
      an OCaml keyword we prime it, see for example {!class'}. *)

  type 'a cons = 'a -> t
  (** The type for attribute constructors with value of type ['a]. *)

  val accesskey : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/accesskey}accesskey} *)

  val action : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form#attr-action}action} *)

  val autocomplete : string cons
(** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/autocomplete}autocomplete} *)

  val autofocus : t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/autofocus}
      autofocus} *)

  val charset : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/charset}
      charset} *)

  val checked : t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/checked}
      checked} *)

  val class' : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/class}
      class} *)

  val content : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/content}
      content} *)

  val contenteditable : bool cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/contenteditable}contenteditable} *)

  val cols : int cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea#attr-cols}cols} *)

  val defer : t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/defer}
      defer} *)

  val disabled : t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/disabled}
      disabled} *)

  val dir : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/dir}dir} *)

  val draggable : bool cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/draggable}draggable} *)

  val for' : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/for'}
      for'} *)

  val height : int cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/height}
      height} *)

  val href : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/href}
      href} *)

  val hidden : t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/hidden}hidden} *)

  val id : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/id}
      id} *)

  val lang : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/lang}
      lang} *)

  val media : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/media}
      media} *)

  val name : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/name}
      name} *)

  val method' : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form#attr-method}method}. *)

  val placeholder : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/placeholder}
      placeholder} *)

  val rel : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/rel}
      rel} *)

  val required : t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/required}
      required} *)

  val rows : int cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea#attr-rows}rows} *)

  val spellcheck : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/spellcheck}spellcheck} *)

  val src : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/src}
      src} *)

  val tabindex : int cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/tabindex}
      tabindex} *)

  val title : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/title}
      title} *)

  val type' : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/type}
      type} *)

  val value : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/value}
      value} *)

  val wrap : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea#attr-wrap}wrap} *)

  val width : int cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/width}
      width} *)
end

(** HTML elements and parts. *)
module El : sig

  (** {1:part HTML parts} *)

  type html
  (** The type for HTML parts. A part is either a single element or
      character data or a list of parts. *)

  (** {2:els Elements} *)

  type name = string
  (** The type for element names. *)

  val el : ?at:At.t list -> name -> html list -> html
  (** [el ?at n cs] is an element with name [n], attributes [at]
      (defaults to [[]]) and children [cs].

      Except for {!At.class'} the list [at] must not define an
      attribute more than once; this is not checked by the module.
      The {!At.class'} is treated specially: multiple specifications
      are gathered to form a single, space separated, attribute value
      for the [class] HTML attribute.

      See also (and favor) {{!El.section-cons}element constructors}. *)

  (** {2:text Text} *)

  val txt : string -> html
  (** [txt d] is character data [d]. *)

  val sp : html
  (** [sp] is [El.txt " "]. *)

  val nbsp : html
  (** [nbsp] is [El.txt "\u{00A0}"]. *)

  (** {2:splice Splices} *)

  val splice : ?sep:html -> html list -> html
  (** [splice ?sep hs] when added to a list of children in {!v} splices
      HTML parts [hs] into the list, separating each part by
      [sep] (if any). *)

  val void : html
  (** [void] is [splice []]. *)

  (** {2:raw_data Raw data} *)

  val raw : string -> html
  (** [raw s] is the raw string [s] without escaping markup delimiters.
      [s] must be well-formed HTML otherwise invalid markup will be generated.
      This can be used to
      {ul
      {- Include foreign markup.}
      {- With the {!style} element, to avoid unpleasant surprises.}} *)

  (** {2:page Page}

      There's more than one way to generate a basic HTML page. The following
      provides good defaults for a minimal document. *)

  val page :
    ?lang:string -> ?generator:string -> ?styles:string list ->
    ?scripts:string list -> ?more_head:html -> title:string -> html -> html
  (** [page ~lang ~generator ~styles ~scripts ~more_head ~title body]
      is an {!El.val-html} element with an {!At.lang} attribute of [lang] (if
      specified and non-empty) containing a {!El.head} element (see below)
      followed by [body] which must be a {!El.body} element.

      The other arguments are used to define the children of the page's
      {!El.head} which are in order:
      {ol
      {- A charset {!El.meta} of UTF-8 (unconditional).}
      {- A generator {!El.meta} of [generator], if specified an non-empty.}
      {- A viewport {!El.meta} with [width=device-width, initial-scale=1]
         (unconditional).}
      {- A stylesheet {!El.link} of type [text/css] for each element
         of [styles], in order (defaults to [[]]).}
      {- A {!El.script} with {!At.defer} and of {!At.type'}
         [text/javascript] for each element of [scripts],
         in order (defaults to [[]]).}
      {- [more_head] (defaults to {!El.void}). Be {{!style}careful}
         with [style] tags.}
      {- The page has a title [title] which is {!String.trim}ed.
         If the result is empty falls back to ["Untitled"].
         See also {!title_of_filepath}.}} *)

  val title_of_filepath : string -> string
  (** [title_of_filepath f] is a {e non-empty} page title for filepath
      [f]. Either the basename of [f] without extension or if that
      results in ["index"] or [""] the basename of the parent
      directory without extension or if that results in [""],
      ["Untitled"]. Directory separators can be ['/'] or ['\\']
      regardless of the platform. *)

  (** {1:output Output} *)

  val buffer_add : doc_type:bool -> Buffer.t -> html -> unit
  (** [buffer_add ~doc_type b h] adds HTML part [h]. If
      [doc_type] is [true] an HTML doctype declaration is
      prepended. *)

  val to_string : doc_type:bool -> html -> string
  (** [to_string] is like {!buffer_add} but returns directly a string. *)

  (** Low level representation (unstable). *)
  module Low : sig
    type t =
    | El of name * At.t list * t list
    (** Element, name attributes and children. *)
    | Txt of string
    (** Character data. *)
    | Splice of t option * t list
    (** List of parts, separated by an optional separator. *)
    | Raw of string
    (** Raw output string. *)
    (** The low-level HTML part representation. *)

    val of_html : html -> t
    (** [of_html h] is a low-level representation for [h]. *)
  end

  (** {1:cons Element constructors}

      See the
      {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element}MDN
      HTML element reference}.

      {b Convention.} Whenever an element name conflicts with an OCaml
      keyword we prime it, see for example {!object'}. *)

  type cons = ?at:At.t list -> html list -> html
  (** The type for element constructors. This is simply {!v} with a
      pre-applied element name. *)

  type void_cons = ?at:At.t list -> unit -> html
  (** The type for void element constructors. This is simply {!el}
      with a pre-applied element name and without children. *)

  val a : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a}a} *)

  val abbr : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr}abbr} *)

  val address : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address}
      address} *)

  val area : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/area}
      area} *)

  val article : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article}
      article} *)

  val aside : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside}
      aside} *)

  val audio : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio}
      audio} *)

  val b : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b}b} *)

  val base : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/base}
      base} *)

  val bdi : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi}
      bdi} *)

  val bdo : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo}
      bdo} *)

  val blockquote : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote}
      blockquote} *)

  val body : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body}
      body} *)

  val br : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br}br} *)

  val button : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button}
      button} *)

  val canvas : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas}
      canvas} *)

  val caption : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption}
      caption} *)

  val cite : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite}
      cite} *)

  val code : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code}
      code} *)

  val col : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col}
      col} *)

  val colgroup : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup}
      colgroup} *)

  val command : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/command}
        command} *)

  val datalist : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist}
      datalist} *)

  val dd : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd}dd} *)

  val del : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del}
      del} *)

  val details : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details}
      details} *)

  val dfn : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn}
      dfn} *)

  val div : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div}
      div} *)

  val dl : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl}dl} *)

  val dt : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt}dt} *)

  val em : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em}em} *)

  val embed : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed}
      embed} *)

  val fieldset : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset}
      fieldset} *)

  val figcaption : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption}
      figcaption} *)

  val figure : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure}
      figure} *)

  val footer : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer}
      footer} *)

  val form : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form}
        form} *)

  val h1 : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1}h1} *)

  val h2 : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2}h2} *)

  val h3 : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3}h3} *)

  val h4 : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4}h4} *)

  val h5 : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5}h5} *)

  val h6 : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6}h6} *)

  val head : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/head}
      head} *)

  val header : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header}
      header} *)

  val hgroup : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hgroup}
        hgroup} *)

  val hr : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr}hr} *)

  val html : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/html}
        html} *)

  val i : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i}i} *)

  val iframe : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe}
        iframe} *)

  val img : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img}
        img} *)

  val input : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input}
        input} *)

  val ins : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins}
        ins} *)

  val kbd : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd}
        kbd} *)

  val keygen : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen}
        keygen} *)

  val label : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label}
        label} *)

  val legend : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend}
      legend} *)

  val li : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li}li} *)

  val link : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link}link} *)

  val map : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/map}map} *)

  val mark : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark}mark} *)

  val menu : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu}menu} *)

  val meta : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meta}meta} *)

  val meter : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter}
      meter} *)

  val nav : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav}nav} *)

  val noscript : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/noscript}
      noscript} *)

  val object' : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object}
      object} *)

  val ol : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol}ol} *)

  val optgroup : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup}
      optgroup} *)

  val option : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option}
      option} *)

  val output : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output}
      output} *)

  val p : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p}p} *)

  val param : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param}
      param} *)

  val pre : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre}
      pre} *)

  val progress : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress}
      progress} *)

  val q : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q}q} *)

  val rp : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp}rp} *)

  val rt : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt}rt} *)

  val ruby : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby}ruby} *)

  val s : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s}s} *)

  val samp : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp}
      samp} *)

  val script : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script}
      script} *)

  val section : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section}
      section} *)

  val select : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select}
      select} *)

  val small : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small}
      small} *)

  val source : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source}
      source} *)

  val span : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span}
      span} *)

  val strong : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong}
      strong} *)

  val style : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/style}
      style}

      {b Warning.} If your style element holds CSS use {!raw} content,
      otherwise the CSS selector [>] gets escaped. *)

  val sub : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub}
      sub} *)

  val summary : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary}
      summary} *)

  val sup : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup}
      sup} *)

  val table : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table}
      table} *)

  val tbody : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody}
      tbody} *)

  val td : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td}td} *)

  val textarea : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea}
      textarea} *)

  val tfoot : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot}
      tfoot} *)

  val th : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th}th} *)

  val thead : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead}
      thead} *)

  val time : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time}
      time} *)

  val title : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/title}
      title} *)

  val tr : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr}tr} *)

  val track : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track}
      track} *)

  val u : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u}u} *)

  val ul : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul}ul} *)

  val var : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var}
      var} *)

  val video : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video}
      video} *)

  val wbr : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr}
      wbr} *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2016 The webs programmers

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
