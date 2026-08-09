(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/webExtension}WebExtension} module.

    @canonical Webs_webdriver.Wd_web_extension *)

val name : string
(** [name] is [webExtension]. *)

(** {1:types Types} *)

open Webs_wd__types
open Webs_wd__protocol

module Extension : sig
  type t = string
  val jsont : t Jsont.t
end

module Extension_data : sig
  type t =
  | Path of string
  | Archive_path of string
  | Base64 of string
  val jsont : t Jsont.t
end

module Install_result : sig
  type t
  val extension : t -> Extension.t
  val jsont : t Jsont.t
end

(** {1:commands Commands} *)

val install :
  Connection.t -> ?exts:Exts.t -> extension_data:Extension_data.t -> unit ->
  Install_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-webExtension-install}
    [webExtension.install]} *)

val uninstall :
  Connection.t -> ?exts:Exts.t -> extension:Extension.t -> unit ->
  Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-webExtension-uninstall}
    [webExtension.uninstall]} *)
