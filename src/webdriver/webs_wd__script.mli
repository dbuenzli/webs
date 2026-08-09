(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/script}Script} module.

    @canonical Webs_webdriver.Wd_script *)

open Webs_wd__types
open Webs_wd__protocol

val name : string
(** [name] is [script]. *)

(** {1 Types} *)

module Serialization_options : sig
  type t
  val make :
    ?max_dom_depth:int option -> ?max_object_depth:int option ->
    ?include_shadow_tree:[ `None | `Open | `All ] -> unit -> t

  val jsont : t Jsont.t
end

module Result_ownership : sig
  type t = [ `None | `Root ]
  val jsont : t Jsont.t
end

module Channel : sig
  type t = string
  val jsont : t Jsont.t
end

module Channel_properties : sig
  type t
  val make :
    channel:Channel.t -> ?serialization_options:Serialization_options.t ->
    ?ownership:Result_ownership.t -> unit -> t

  val channel : t -> Channel.t
  val serialization_options : t -> Serialization_options.t option
  val ownership : t -> Result_ownership.t option
  val jsont : t Jsont.t
end

module Channel_value : sig
  type t
  val make : value:Channel_properties.t -> unit -> t
  val value : t -> Channel_properties.t
  val jsont : t Jsont.t
end

module Handle : sig
  type t = string
  val jsont : string Jsont.t
end

module Internal_id : sig
  type t = string
  val jsont : string Jsont.t
end

module Preload_script : sig
  type t = string
  val jsont : t Jsont.t
end

module Realm : sig
  type t = string
  val jsont : t Jsont.t
end

module Realm_info : sig
  module Window : sig
    type t
    val context : t -> Webs_wd__types.Browsing_context.t
    val user_context : t -> Webs_wd__types.User_context.t option
    val sandbox : t -> string option
    val jsont : t Jsont.t
  end
  module Dedicated_worker : sig
    type t
    val owners : t -> Realm.t list
    val jsont : t Jsont.t
  end
  module Shared_worker : sig type t = unit val jsont : t Jsont.t end
  module Service_worker : sig type t = unit val jsont : t Jsont.t end
  module Worker : sig type t = unit val jsont : t Jsont.t end
  module Paint_worklet : sig type t = unit val jsont : t Jsont.t end
  module Audio_worklet : sig type t = unit val jsont : t Jsont.t end
  module Worklet : sig type t = unit val jsont : t Jsont.t end
  type type' =
  [ `Audio_worklet of Audio_worklet.t
  | `Dedicated_worker of Dedicated_worker.t
  | `Paint_worklet of Paint_worklet.t
  | `Service_worker of Service_worker.t
  | `Shared_worker of Shared_worker.t
  | `Window of Window.t
  | `Worker of Worker.t
  | `Worklet of Worklet.t ]
  type t
  val realm : t -> Realm.t
  val origin : t -> string
  val type' : t -> type'
  val jsont : t Jsont.t
end

module Realm_type : sig
  type t =
  [ `Window | `Dedicated_worker | `Shared_worker | `Service_worker
  | `Worker | `Paint_worklet | `Audio_worklet | `Worklet ]

  val jsont : t Jsont.t
end

module Shared_id : sig
  type t = string
  val jsont : string Jsont.t
end

module Shared_reference : sig
  type t
  val make :
    ?exts:Exts.t -> ?handle:string -> shared_id:Shared_id.t -> unit -> t
  val jsont : t Jsont.t
end

module Stack_frame : sig
  type t
  val column_number : t -> int
  val function_name : t -> string
  val line_number : t -> int
  val url : t -> string
  val jsont : t Jsont.t
end

module Stack_trace : sig
  type t
  val call_frames : t -> Stack_frame.t list
  val jsont : t Jsont.t
end

module Source : sig
  type t
  val realm : t -> Realm.t
  val context : t -> Browsing_context.t option
  val user_context : t -> User_context.t option
  val jsont : t Jsont.t
end

module Target : sig
  type t =
  [ `Realm of Realm.t
  | `Context of Browsing_context.t * string option ]

  val jsont : t  Jsont.t
end

module Remote_reference : sig
  type t
  val make : ?shared_id:Shared_id.t -> ?handle:Handle.t -> unit -> t
  val shared_id : t -> Shared_id.t option
  val handle : t -> Handle.t option
  val jsont : t Jsont.t
end

module Undefined_value : sig
  type t = unit
  val jsont : t Jsont.t
end

module Null_value : sig
  type t = unit
  val jsont : t Jsont.t
end

module String_value : sig
  type t = string
  val jsont : t Jsont.t
end

module Number_value : sig
  type t = float
  val jsont : t Jsont.t
end

module Boolean_value : sig
  type t = bool
  val jsont : t Jsont.t
end

module Big_int_value : sig
  type t = string
  val jsont : t Jsont.t
end

module Date_value : sig
  type t = string
  val jsont : t Jsont.t
end

module Regexp_value :  sig
  type t
  val make : pattern:string -> ?flags:string -> unit -> t
  val pattern : t -> string
  val flags : t -> string option
  val jsont : t Jsont.t
end

module Regexp_local_value :  sig
  type t = Regexp_value.t
  val jsont : t Jsont.t
end

module Local_value : sig
  type binding = [ `Key of t | `Key_string of string ] * t
  and t =
  [ `Array_local_value of t list
  | `Big_int_value of Big_int_value.t
  | `Boolean_value of Boolean_value.t
  | `Channel_value of Channel_value.t
  | `Date_local_value of Date_value.t
  | `Map_local_value of binding list
  | `Null_value of Null_value.t
  | `Number_value of Number_value.t
  | `Object_local_value of binding list
  | `Regexp_local_value of Regexp_local_value.t
  | `Remote_reference of Remote_reference.t
  | `Set_local_value of t list
  | `String_value of String_value.t
  | `Undefined_value of Undefined_value.t ]
  val jsont : t Jsont.t
  val binding_jsont : binding Jsont.t
end

module Node_properties : sig
  (**/**)
  type node_remote_value
  (** @canonical Webs_webdriver.Wd_script.Node_remote_value.t *)
  (**/**)
  type t
  val make :
    node_type:int -> child_node_count:int ->
    ?attributes:string Map.Make(String).t -> ?children:node_remote_value list ->
    ?local_name:string -> ?mode:[ `Closed | `Open ] -> ?namespace_uri:string ->
    ?node_value:string -> ?shadow_root:node_remote_value option -> unit ->
    t

  val node_type : t -> int
  val child_node_count : t -> int
  val attributes : t -> string Map.Make(String).t option
  val children : t -> node_remote_value list option
  val local_name : t -> string option
  val mode : t -> [ `Closed | `Open ] option
  val namespace_uri : t -> string option
  val node_value : t -> string option
  val shadow_root : t -> node_remote_value option option
  val attributes_jsont : string Map.Make(String).t Jsont.t
  val mode_jsont : [ `Closed | `Open ] Jsont.t
  val jsont : t Jsont.t
end

module Node_remote_value : sig
  type t = Node_properties.node_remote_value
  val make :
    ?shared_id:Shared_id.t -> ?handle:Handle.t -> ?internal_id:Internal_id.t ->
    ?value:Node_properties.t -> unit -> t

  val shared_id : t -> Shared_id.t option
  val handle : t -> Handle.t option
  val internal_id : t -> Internal_id.t option
  val value : t -> Node_properties.t option
  val jsont_standalone : t Jsont.t
  val jsont : t Jsont.t
end

module Regexp_remote_value : sig
  type t
  val make :
    handle:Handle.t option -> internal_id:Internal_id.t option ->
    value:Regexp_value.t-> unit -> t
  val handle : t -> Handle.t option
  val internal_id : t -> Internal_id.t option
  val value : t -> Regexp_value.t
  val jsont : t Jsont.t
end

module Date_remote_value : sig
  type t
  val make :
    handle:Handle.t option -> internal_id:Internal_id.t option ->
    value:string-> unit -> t
  val handle : t -> Handle.t option
  val internal_id : t -> Internal_id.t option
  val value : t -> string
  val jsont : t Jsont.t
end

(**/**)
module type SIMPLE_REMOTE_VALUE = sig
  type t
  val make :
    handle:Handle.t option ->
    internal_id:Internal_id.t option -> unit -> t
  val handle : t -> Handle.t option
  val internal_id : t -> Internal_id.t option
  val jsont : t Jsont.t
end
(**/**)

module Symbol_remote_value : SIMPLE_REMOTE_VALUE
module Function_remote_value : SIMPLE_REMOTE_VALUE
module Weakmap_remote_value : SIMPLE_REMOTE_VALUE
module Weakset_remote_value : SIMPLE_REMOTE_VALUE
module Generator_remote_value : SIMPLE_REMOTE_VALUE
module Error_remote_value : SIMPLE_REMOTE_VALUE
module Proxy_remote_value : SIMPLE_REMOTE_VALUE
module Promise_remote_value : SIMPLE_REMOTE_VALUE
module Typed_array_remote_value : SIMPLE_REMOTE_VALUE
module Array_buffer_remote_value : SIMPLE_REMOTE_VALUE

module Window_proxy_properties : sig
  type t
  val make : context:Webs_wd__types.Browsing_context.t -> unit -> t
  val context : t -> Webs_wd__types.Browsing_context.t
  val jsont : t Jsont.t
end

module Window_proxy_remote_value : sig
  type t
  val make :
    handle:Handle.t option ->
    internal_id:Internal_id.t option ->
    value:Window_proxy_properties.t -> unit -> t
  val handle : t -> Handle.t option
  val internal_id : t -> Internal_id.t option
  val value : t -> Window_proxy_properties.t
  val jsont : t Jsont.t
end

module Remote_value : sig
  (**/**)
  type array_remote_value
  (** @canonical Webs_webdriver.Wd_script.Array_remote_value.t *)

  type object_remote_value
  (** @canonical Webs_webdriver.Wd_script.Object_remote_value.t *)

  type map_remote_value
  (** @canonical Webs_webdriver.Wd_script.Map_remote_value.t *)

  type set_remote_value
  (** @canonical Webs_webdriver.Wd_script.Set_remote_value.t *)

  type node_list_remote_value
  (** @canonical Webs_webdriver.Wd_script.Node_list_remote_value.t *)

  type html_collection_remote_value
  (** @canonical Webs_webdriver.Wd_script.Html_collection_remote_value.t *)
  (**/**)

  type binding = [ `Key of t | `Key_string of string ] * t
  and t =
  [ `Undefined_value of Undefined_value.t
  | `Null_value of Null_value.t
  | `String_value of String_value.t
  | `Number_value of Number_value.t
  | `Boolean_value of Boolean_value.t
  | `Big_int_value of Big_int_value.t
  | `Symbol_remote_value of Symbol_remote_value.t
  | `Array_remote_value of array_remote_value
  | `Object_remote_value of object_remote_value
  | `Function_remote_value of Function_remote_value.t
  | `Regexp_remote_value of Regexp_remote_value.t
  | `Date_remote_value of Date_remote_value.t
  | `Map_remote_value of map_remote_value
  | `Set_remote_value of set_remote_value
  | `Weakmap_remote_value of Weakmap_remote_value.t
  | `Weakset_remote_value of Weakset_remote_value.t
  | `Generator_remote_value of Generator_remote_value.t
  | `Error_remote_value of Error_remote_value.t
  | `Proxy_remote_value of Proxy_remote_value.t
  | `Promise_remote_value of Promise_remote_value.t
  | `Typed_array_remote_value of Typed_array_remote_value.t
  | `Array_buffer_remote_value of Array_buffer_remote_value.t
  | `Node_list_remote_value of node_list_remote_value
  | `Html_collection_remote_value of html_collection_remote_value
  | `Node_remote_value of Node_remote_value.t
  | `Window_proxy_remote_value of Window_proxy_remote_value.t ]

  val jsont : t Jsont.t
  val binding_jsont : binding Jsont.t
end

(**/**)
module type REMOTE_LIST_REMOTE_VALUE = sig
  type t
  val handle : t -> Handle.t option
  val internal_id : t -> Internal_id.t option
  val value : t -> Remote_value.t list option
  val make :
    handle:Handle.t option ->
    internal_id:Internal_id.t option ->
    value:Remote_value.t list option -> unit -> t
  val jsont : t Jsont.t
end
(**/**)

module Array_remote_value : REMOTE_LIST_REMOTE_VALUE (** @inline *)
  with type t = Remote_value.array_remote_value

module Set_remote_value : REMOTE_LIST_REMOTE_VALUE (** @inline *)
  with type t = Remote_value.set_remote_value

module Node_list_remote_value : REMOTE_LIST_REMOTE_VALUE (** @inline *)
  with type t = Remote_value.node_list_remote_value

module Html_collection_remote_value : REMOTE_LIST_REMOTE_VALUE (** @inline *)
  with type t = Remote_value.html_collection_remote_value

module Object_remote_value : sig
  type t = Remote_value.object_remote_value
  val handle : t -> Handle.t option
  val internal_id : t -> Internal_id.t option
  val value : t -> Remote_value.binding list option
  val make :
    handle:Handle.t option -> internal_id:Internal_id.t option ->
    value:Remote_value.binding list option -> unit -> t
  val jsont : t Jsont.t
end

module Map_remote_value : sig
  type t = Remote_value.map_remote_value
  val handle : t -> Handle.t option
  val internal_id : t -> Internal_id.t option
  val value : t -> Remote_value.binding list option
  val make :
    handle:Handle.t option -> internal_id:Internal_id.t option ->
    value:Remote_value.binding list option -> unit -> t
  val jsont : t Jsont.t
end

module Exception_details : sig
  type t
  val column_number : t -> int
  val exception' : t -> Remote_value.t
  val line_number : t -> int
  val stack_trace : t -> Stack_trace.t
  val text : t -> string
  val jsont : t Jsont.t
end

module Evaluate_result : sig
  module Success : sig
    type t
    val result : t -> Remote_value.t
    val realm : t -> Realm.t
    val jsont : t Jsont.t
  end
  module Exception : sig
    type t
    val exception_details : t -> Exception_details.t
    val realm : t -> Realm.t
    val jsont : t Jsont.t
  end
  type t = [ `Exception of Exception.t | `Success of Success.t ]
  val jsont : t Jsont.t
end

(** {2:command_results Command results} *)

module Add_preload_script_result : sig
  type t
  val preload_script : t -> Preload_script.t
  val jsont : t Jsont.t
end

module Get_realms_result : sig
  type t
  val realms : t -> Realm.t list
  val jsont : t Jsont.t
end

(** {2:event_parameters Event parameters} *)

module Message_parameters : sig
  type t
  val channel : t -> Channel.t
  val data : t -> Remote_value.t
  val source : t -> Source.t
  val jsont : t Jsont.t
end

module Realm_destroyed_parameters : sig
  type t
  val realm : t -> Realm.t
  val jsont : t Jsont.t
end

(** {1:commands Commands} *)

val add_preload_script :
  Connection.t ->
  ?exts:Webs_wd__types.Exts.t ->
  function_declaration:string ->
  ?arguments:Channel_value.t list ->
  ?contexts:Webs_wd__types.Browsing_context.t list ->
  ?user_contexts:Webs_wd__types.User_context.t list ->
  ?sandbox:string -> unit -> Add_preload_script_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-script-addPreloadScript}
    [script.addPreloadScript]} *)

val disown :
  Connection.t -> ?exts:Exts.t -> handles:Handle.t list ->
  target:Target.t -> unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-script-disown}
    [script.disown]} *)

val call_function :
  Connection.t -> ?exts:Webs_wd__types.Exts.t -> function_declaration:string ->
  await_promise:bool -> target:Target.t -> ?arguments:Local_value.t list ->
  ?result_ownership:Result_ownership.t ->
  ?serialization_options:Serialization_options.t -> ?this:Local_value.t ->
  ?user_activation:bool ->
  unit -> Evaluate_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-script-callFunction}
    [script.evaluate]} *)

val evaluate :
  Connection.t -> ?exts:Exts.t -> expression:string ->
  target:Target.t -> await_promise:bool ->
  ?result_ownership:Result_ownership.t ->
  ?serialization_options:Serialization_options.t ->
  ?user_activation:bool -> unit -> Evaluate_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-script-evaluate}
    [script.evaluate]} *)

val get_realms :
  Connection.t -> ?exts:Exts.t -> ?context:Browsing_context.t ->
  ?type':Realm_type.t -> unit -> Get_realms_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-script-getRealms}
    [script.getRealms]} *)

val remove_preload_script :
  Connection.t -> ?exts:Exts.t -> script:Preload_script.t -> unit ->
  Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-script-removePreloadScript}
    [script.removePreloadScript]} *)

(** {1:events Events} *)

val message : Message_parameters.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-script-message}
    [script.message]} *)

val realm_created : Realm_info.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-script-realmCreated}
    [script.realmCreated]} *)

val realm_destroyed : Realm_destroyed_parameters.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-script-realmDestroyed}
    [script.realmDestroyed]} *)
