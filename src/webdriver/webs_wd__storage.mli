(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/storage}Storage} module.

    @canonical Webs_webdriver.Wd_storage *)

open Webs_wd__types
open Webs_wd__protocol

val name : string
(** [name] is ["storage"]. *)

(** {1:types Types} *)

module Partition_key : sig
  type t
  val make :
    ?exts:Exts.t -> ?user_context:string ->
    ?source_origin:string -> unit -> t
  val user_context : t -> string option
  val source_origin : t -> string option
  val exts : t -> Exts.t
  val jsont : t Jsont.t
end

module Cookie_filter : sig
  type t
  val make :
    ?name:string -> ?value:Webs_wd__network.Bytes_value.t -> ?domain:string ->
    ?path:string -> ?size:int -> ?http_only:bool -> ?secure:bool ->
    ?same_site:Webs_wd__network.Same_site.t ->
    ?expiry:int -> ?exts:Exts.t -> unit -> t
  val name : t -> string option
  val value : t -> Webs_wd__network.Bytes_value.t option
  val domain : t -> string option
  val path : t -> string option
  val size : t -> int option
  val http_only : t -> bool option
  val secure : t -> bool option
  val same_site : t -> Webs_wd__network.Same_site.t option
  val expiry : t -> int option
  val exts : t -> Exts.t
  val jsont : t Jsont.t
end

module Browsing_context_partition_descriptor : sig
  type t
  val make : context:Browsing_context.t -> unit -> t
  val context : t -> Browsing_context.t
  val jsont : t Jsont.t
end

module Storage_key_partition_descriptor : sig
  type t
  val make :
    ?exts:Exts.t -> ?source_origin:string -> ?user_context:User_context.t ->
    unit -> t
  val user_context : t -> User_context.t option
  val source_origin : t -> string option
  val exts : t -> Exts.t
  val jsont : t Jsont.t
end

module Partition_descriptor : sig
  type t =
  [ `Browsing_context of Browsing_context_partition_descriptor.t
  | `Storage_key of Storage_key_partition_descriptor.t ]
  val jsont : t Jsont.t
end

module Partial_cookie : sig
  type t
  val make :
    name:string -> value:Webs_wd__network.Bytes_value.t -> domain:string ->
    ?path:string -> ?http_only:bool -> ?secure:bool ->
    ?same_site:Webs_wd__network.Same_site.t -> ?expiry:int ->
    ?exts:Webs_wd__types.Exts.t -> unit -> t
  val name : t -> string
  val value : t -> Webs_wd__network.Bytes_value.t
  val domain : t -> string
  val path : t -> string option
  val http_only : t -> bool option
  val secure : t -> bool option
  val same_site : t -> Webs_wd__network.Same_site.t option
  val expiry : t -> int option
  val exts : t -> Webs_wd__types.Exts.t
  val jsont : t Jsont.t
end

module Get_cookies_result : sig
  type t
  val cookies : t -> Webs_wd__network.Cookie.t list
  val partition_key : t -> Partition_key.t
  val jsont : t Jsont.t
end

module Set_cookie_result : sig
  type t
  val partition_key : t -> Partition_key.t
  val jsont : t Jsont.t
end

module Delete_cookies_result : sig
  type t
  val partition_key : t -> Partition_key.t
  val jsont : t Jsont.t
end

(** {1:commands Commands} *)

val get_cookies :
  Connection.t -> ?exts:Exts.t -> ?filter:Cookie_filter.t ->
  ?partition:Partition_descriptor.t -> unit -> Get_cookies_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-storage-getCookies}
    [storage.getCookies]} *)

val set_cookie :
  Connection.t -> ?exts:Exts.t -> cookie:Partial_cookie.t ->
  ?partition:Partition_descriptor.t -> unit -> Set_cookie_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-storage-setCookie}
    [storage.setCookie]}. *)

val delete_cookies :
  Connection.t -> ?exts:Exts.t -> ?filter:Cookie_filter.t ->
  ?partition:Partition_descriptor.t -> unit -> Delete_cookies_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-storage-deleteCookies}
    [storage.deleteCookies]} *)
