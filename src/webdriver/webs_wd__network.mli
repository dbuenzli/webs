(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {{:https://developer.mozilla.org/en-US/docs/Web/WebDriver/Reference/BiDi/Modules/network}Network} module.

    @canonical Webs_webdriver.Wd_network *)

open Webs_wd__types
open Webs_wd__protocol

val name : string
(** [name] is ["network"]. *)

(** {1:types Types} *)

module Auth_challenge : sig
  type t
  val make : scheme:string -> realm:string -> t
  val scheme : t -> string
  val realm : t -> string
  val jsont : t Jsont.t
end

module Auth_credentials : sig
  type t
  val make : username:string -> password:string -> t
  val username : t -> string
  val password : t -> string
  val open_jsont : (string -> string -> 'a) -> (t, 'a) Jsont.Object.map
  val jsont : t Jsont.t
  val case_jsont : t Jsont.t
end

module Bytes_value : sig
  type t = [ `Base64 of string | `String of string ]
  val jsont : t Jsont.t
end

module Collector : sig
  type t = string
  val jsont : t Jsont.t
end

module Collector_type : sig
  type t = [ `Blob ] val jsont : t Jsont.t
end

module Same_site : sig
  type t = [ `Default | `Lax | `None | `Strict ]
  val jsont : t Jsont.t
end

module Cookie : sig
  type t
  val make :
    ?exts:Webs_wd__types.Exts.t -> name:string -> value:Bytes_value.t ->
    domain:string -> path:string -> size:int -> http_only:bool ->
    secure:bool -> same_site:Same_site.t -> ?expiry:int -> unit -> t
  val name : t -> string
  val value : t -> Bytes_value.t
  val domain : t -> string
  val path : t -> string
  val size : t -> int
  val http_only : t -> bool
  val secure : t -> bool
  val same_site : t -> Same_site.t
  val expiry : t -> int option
  val exts : t -> Exts.t
  val jsont : t Jsont.t
end

module Cookie_header : sig
  type t
  val make : name:string -> value:Bytes_value.t -> t
  val name : t -> string
  val value : t -> Bytes_value.t
  val jsont : t Jsont.t
end

module Data_type : sig
  type t = [`Request | `Response ]
  val jsont : t Jsont.t
end

module Fetch_timing_info : sig
  type t
  val time_origin : t -> float
  val request_time : t -> float
  val redirect_start : t -> float
  val redirect_end : t -> float
  val fetch_start : t -> float
  val dns_start : t -> float
  val dns_end : t -> float
  val connect_start : t -> float
  val connect_end : t -> float
  val tls_start : t -> float
  val request_start : t -> float
  val response_start : t -> float
  val response_end : t -> float
  val jsont : t Jsont.t
end

module Header : sig
  type t
  val make : name:string -> value:Bytes_value.t -> t
  val name : t -> string
  val value : t -> Bytes_value.t
  val jsont : t Jsont.t
end

module Request : sig
  type t = string
  val jsont : t Jsont.t
end

module Initiator_type : sig
  type t = [ `Other | `Parser | `Preflight | `Script ]
  val jsont : t Jsont.t
end

module Initiator : sig
  type t
  val column_number : t -> int option
  val line_number : t -> int option
  val request : t -> Request.t option
  val stack_trace : t -> Webs_wd__script.Stack_trace.t option
  val type' : t -> Initiator_type.t option
  val jsont : t Jsont.t
end

module Intercept : sig
  type t = string
  val jsont : t Jsont.t
end

module Intercept_phase : sig
  type t = [ `Auth_required | `Before_request_sent | `Response_started ]
  val jsont : t Jsont.t
end

module Request_data : sig
  type t
  val request : t -> Request.t
  val url : t -> string
  val method' : t -> string
  val headers : t -> Header.t list
  val cookies : t -> Cookie.t list
  val header_size : t -> int option
  val body_size : t -> int option
  val destination : t -> string
  val initator_type : t -> string
  val timings : t -> Fetch_timing_info.t
  val jsont : t Jsont.t
end

module Response_content : sig
  type t
  val size : t -> int
  val jsont : t Jsont.t
end

module Response_data : sig
  type t
  val url : t -> string
  val protocol : t -> string
  val status : t -> int
  val status_text : t -> string
  val from_cache : t -> bool
  val headers : t -> Header.t list
  val mime_type : t -> string
  val bytes_received : t -> int
  val header_size : t -> int option
  val body_size : t -> int option
  val content : t -> Response_content.t
  val auth_challenges : t -> Auth_challenge.t list option
  val jsont : t Jsont.t
end

module Set_cookie_header : sig
  type t
  val make :
    name:string -> value:Bytes_value.t -> ?domain:string ->
    ?http_only:bool -> ?expiry:string -> ?max_age:int ->
    ?path:string -> ?same_site:Same_site.t -> ?secure:bool -> unit -> t
  val name : t -> string
  val value : t -> Bytes_value.t
  val domain : t -> string option
  val http_only : t -> bool option
  val expiry : t -> string option
  val max_age : t -> int option
  val path : t -> string option
  val same_site : t -> Same_site.t option
  val secure : t -> bool option
  val jsont : t Jsont.t
end

module Url_pattern_pattern : sig
  type t
  val make :
    ?protocol:string ->
    ?hostname:string ->
    ?port:string -> ?pathname:string -> ?search:string -> unit -> t
  val protocol : t -> string option
  val hostname : t -> string option
  val port : t -> string option
  val pathname : t -> string option
  val search : t -> string option
  val jsont : t Jsont.t
end

module Url_pattern_string : sig
  type t
  val make : pattern:string -> unit -> t
  val pattern : t -> string
  val jsont : t Jsont.t
end

module Url_pattern : sig
  type t =
  [ `Pattern of Url_pattern_pattern.t
  | `String of Url_pattern_string.t ]
  val jsont : t Jsont.t
end

(** {2:command_results Command results} *)

module Add_data_collector_result : sig
  type t
  val collector : t -> Collector.t
  val jsont : t Jsont.t
end

module Add_intercept_result : sig
  type t
  val intercept : t -> Intercept.t
  val jsont : t Jsont.t
end

module Get_data_result : sig
  type t
  val bytes : t -> Bytes_value.t
  val jsont : t Jsont.t
end

(** {2:event_parameters Event parameters} *)

(**/**)
module type EVENT_BASE = sig
  type t
  val context : t -> Browsing_context.t option
  val is_blocked : t -> bool
  val navigation : t -> Webs_wd__browsing_context.Navigation.t
  val redirect_count : t -> int
  val request : t -> Request_data.t
  val timestamp : t -> int
  val user_context : t -> Webs_wd__types.User_context.t option option
  val intercepts : t -> Intercept.t list option
end
(**/**)

module Auth_required_parameters : sig
  include EVENT_BASE
  val response : t -> Response_data.t
  val jsont : t Jsont.t
end

module Before_request_sent_parameters : sig
  include EVENT_BASE
  val initiator : t -> Initiator.t option
  val jsont : t Jsont.t
end

module Fetch_error_parameters : sig
  include EVENT_BASE
  val error_text : t -> string
  val jsont : t Jsont.t
end

module Response_completed_parameters : sig
  include EVENT_BASE
  val response : t -> Response_data.t
  val jsont : t Jsont.t
end

module Response_started_parameters : sig
  include EVENT_BASE
  val response : t -> Response_data.t
  val jsont : t Jsont.t
end

(** {1:commands Commands} *)

val add_data_collector :
  Connection.t -> ?exts:Exts.t -> data_types:Data_type.t list ->
  max_encoded_data_size:int -> ?collector_type:Collector_type.t ->
  ?contexts:Browsing_context.t list -> ?user_contexts:User_context.t list ->
  unit -> Add_data_collector_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-network-addDataCollector}
    [network.addDataCollector]} *)

val add_intercept :
  Connection.t -> ?exts:Exts.t -> phases:Intercept_phase.t list ->
  ?contexts:Browsing_context.t list -> ?url_patterns:Url_pattern.t list ->
  unit -> Add_intercept_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-network-addIntercept}
    [network.addIntercept]} *)

val continue_request :
  Connection.t -> ?exts:Exts.t -> request:Request.t -> ?body:Bytes_value.t ->
  ?cookies:Cookie_header.t list -> ?headers:Header.t list ->
  ?method':string -> ?url:string -> unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-network-continueRequest}
    [network.continueRequest]} *)

val continue_response :
  Connection.t -> ?exts:Exts.t -> request:Request.t ->
  ?cookies:Set_cookie_header.t list -> ?credentials:Auth_credentials.t ->
  ?headers:Header.t list -> ?reason_phrase:string ->
  ?status_code:int -> unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-network-continueResponse}
    [network.continueResponse]} *)

val continue_with_auth :
  Connection.t ->  ?exts:Exts.t -> request:Request.t ->
  action:[ `Cancel | `Default | `Provide_credentials of Auth_credentials.t ] ->
  unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-network-continueWithAuth}
    [network.continueWithAuth]} *)

val disown_data :
  Connection.t -> ?exts:Exts.t -> data_type:Data_type.t ->
  collector:Collector.t -> request:Request.t -> unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-network-disownData}
    [network.disownData]} *)

val fail_request :
  Connection.t -> ?exts:Exts.t -> request:Request.t -> unit ->
  Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-network-failRequest}
    [network.failRequeset]} *)

val get_data :
  Connection.t -> ?exts:Exts.t -> data_type:Data_type.t ->
  ?collector:Collector.t -> ?disown:bool -> request:Request.t -> unit ->
  Get_data_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-network-getData}
    [network.getData]} *)

val provide_response :
  Connection.t -> ?exts:Exts.t -> request:Request.t -> ?body:Bytes_value.t ->
  ?cookies:Set_cookie_header.t list -> ?headers:Header.t list ->
  ?reason_phrase:string -> ?status_code:int -> unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-network-provideResponse}
    [network.provideResponse]} *)

val remove_data_collector :
  Connection.t -> ?exts:Exts.t -> collector:Collector.t -> unit ->
  Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-network-removeDataCollector}[network.removeDataCollector]} *)

val remove_intercept :
  Connection.t -> ?exts:Exts.t -> intercept:Intercept.t -> unit ->
  Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-network-removeIntercept}
    [network.removeIntercept]} *)

val set_cache_behavior :
  Connection.t -> ?exts:Exts.t -> cache_behavior:[`Bypass | `Default ] ->
  ?contexts:Browsing_context.t list -> unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-network-setCacheBehavior}
    [network.setCacheBehavior]} *)

val set_extra_headers :
  Connection.t -> ?exts:Exts.t -> headers:Header.t list ->
  ?contexts:Browsing_context.t list -> ?user_contexts:User_context.t list ->
  unit -> Empty_result.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#command-network-setExtraHeaders}
    [network.setExtraHeaders]} *)

(** {1:events Events} *)

val auth_required : Auth_required_parameters.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-network-authRequired}
    [network.authRequired]} *)

val before_request_sent : Before_request_sent_parameters.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-network-beforeSendRequest}
    [network.beforeRequestSent]} *)

val fetch_error : Fetch_error_parameters.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-network-fetchError}
    [network.fetchError]} *)

val response_completed : Response_completed_parameters.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-network-responseCompleted}
    [network.responseCompleted]} *)

val response_started : Response_started_parameters.t Event.t
(** {{:https://www.w3.org/TR/webdriver-bidi/#event-network-responseStarted}
    [network.responseStarted]} *)
