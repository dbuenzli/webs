(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Methods and method constraints.

    @canonical Webs.Http.Method *)

  (** {1:methods Methods} *)

type t =
  [ `GET
  (** {{:https://www.rfc-editor.org/rfc/rfc9110#GET}[GET]} *)
  | `HEAD
  (** {{:https://www.rfc-editor.org/rfc/rfc9110#HEAD}[HEAD]} *)
  | `POST
  (** {{:https://www.rfc-editor.org/rfc/rfc9110#POST}[POST]} *)
  | `PUT
  (** {{:https://www.rfc-editor.org/rfc/rfc9110#PUT}[PUT]} *)
  | `DELETE
  (** {{:https://www.rfc-editor.org/rfc/rfc9110#DELETE}[DELETE]} *)
  | `CONNECT
  (** {{:https://www.rfc-editor.org/rfc/rfc9110#CONNECT}[CONNECT]} *)
  | `OPTIONS
  (** {{:https://www.rfc-editor.org/rfc/rfc9110#OPTIONS}[OPTIONS]} *)
  | `TRACE
  (** {{:https://www.rfc-editor.org/rfc/rfc9110#TRACE}[TRACE]} *)
  | `PATCH
  (** {{:http://www.rfc-editor.org/rfc/rfc5789}[PATCH]} '*)
  | `Other of string
    (** Other {{:https://www.rfc-editor.org/rfc/rfc9110#name-tokens}token} *)
  ]
(** The type for
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-methods}
    request methods}. *)

(** {1:constraints Constraints} *)

type 'a constraint' = t * 'a
(** The type for constraining methods to ['a]. *)

val constrain :
  allowed:'a constraint' list -> t -> ('a, 'a constraint' list) result
(** [constrain ~allowed m] constrains [m] to [allowed]. This is [Ok m]
    if [m] is constrained by [allowed] and [Error allowed] otherwise. *)

val connect : [> `CONNECT] constraint'
(** [connect] adds [`CONNECT] to the constraint set. *)

val delete : [> `DELETE] constraint'
(** [delete] adds [`DELETE] to the constraint set. *)

val get : [> `GET] constraint'
(** [get] adds [`GET] to the constraint set. *)

val head : [> `HEAD] constraint'
(** [head] adds [`HEAD] to the constraint set. *)

val options : [> `OPTIONS] constraint'
(** [options] adds [`OPTIONS] to the constraint set. *)

val other : string -> 'a ->  'a constraint'
(** [other s v] adds a constraint for method [s] represented
    by [v] to the constraint set. *)

val patch : [> `PATCH] constraint'
(** [patch] adds [`PATCH] to the constraint set. *)

val post : [> `POST] constraint'
(** [post] adds [`POST] to the constraint set. *)

val put : [> `PUT] constraint'
(** [put] adds [`PUT] to the constraint set. *)

val trace : [> `TRACE] constraint'
(** [trace] adds [`TRACE] to the constraint set. *)

(** {1:converting Converting} *)

val decode : string -> (t, string) result
(** [decode s] decodes an
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-methods}HTTP method}
    from [s]. *)

val encode : t -> string
(** [encode m] encodes [m] to an
    {{:https://www.rfc-editor.org/rfc/rfc9110#name-methods}HTTP method}.

    @raise Invalid_argument if [m] is [`Other t] and [t] is not
    a {!Webs.Http.Headers.value_is_token}. *)

(** {1:predicates Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal] tests methods for equality. *)

val compare : t -> t -> int
(** [compare] is a total order on methods compatible with {!equal}. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats methods for inspection. *)
