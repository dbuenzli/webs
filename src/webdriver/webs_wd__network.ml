(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs_wd__types
open Webs_wd__protocol

let name = "network"

(* Types *)

module Auth_challenge = struct
  type t = { scheme : string; realm : string }
  let make ~scheme ~realm = { scheme; realm }
  let scheme a = a.scheme
  let realm a = a.realm
  let jsont =
    let make scheme realm = { scheme; realm } in
    Jsont.Object.map ~kind:"network.AuthChallenge" make
    |> Jsont.Object.mem "scheme" Jsont.string ~enc:scheme
    |> Jsont.Object.mem "realm" Jsont.string ~enc:realm
    |> Jsont.Object.finish
end

module Auth_credentials = struct
  type t = { username : string; password : string }
  let make ~username ~password = { username; password }
  let username a = a.username
  let password a = a.password
  let open_jsont make =
    Jsont.Object.map ~kind:"network.AuthCredentials" make
    |> Jsont.Object.mem "username" Jsont.string ~enc:username
    |> Jsont.Object.mem "passowrd" Jsont.string ~enc:password

  let jsont =
    let make username password type' =
      if type' = "password" then { username; password } else
      Jsont.Error.msgf Jsont.Meta.none
        "Expected member type value \"password\" but found %S" type'
    in
    open_jsont make
    |> Jsont.Object.mem "type" Jsont.string ~enc:(fun _ -> "password")
    |> Jsont.Object.finish

  let case_jsont =
    let make username password = { username; password } in
    open_jsont make
    |> Jsont.Object.finish
end

module Bytes_value = struct
  type t =
  [ `String of string | `Base64 of string ]

  let string_jsont =
    Jsont.Object.map ~kind:"network.StringValue" Fun.id
    |> Jsont.Object.mem "value" Jsont.string
    |> Jsont.Object.finish

  let base64_jsont =
    Jsont.Object.map ~kind:"network.Base64Value" Fun.id
    |> Jsont.Object.mem "value" Jsont.string
    |> Jsont.Object.finish

  let jsont =
    let string = Jsont.Object.Case.map "string" string_jsont in
    let base64 = Jsont.Object.Case.map "base64" base64_jsont in
    let enc_case = function
    | `String s -> Jsont.Object.Case.value string s
    | `Base64 s -> Jsont.Object.Case.value base64 s
    in
    let cases = Jsont.Object.Case.[make string; make base64] in
    Jsont.Object.map ~kind:"network.BytesValue" Fun.id
    |> Jsont.Object.case_mem
      "type" Jsont.string cases ~enc_case ~enc:Fun.id ~tag_to_string:Fun.id
    |> Jsont.Object.finish
end

module Collector = struct
  type t = string
  let jsont = Jsont.with_doc ~kind:"network.Collector" Jsont.string
end

module Collector_type = struct
  type t = [`Blob]
  let jsont = Jsont.enum ~kind:"networkCollectorType" ["blob", `Blob]
end

module Same_site = struct
  type t = [`Strict | `Lax | `None | `Default ]
  let jsont =
    Jsont.enum ~kind:"network.SameSite"
      ["strict", `Strict; "lax", `Lax; "none", `None; "default", `Default]
end

module Cookie = struct
  type t =
    { name : string; value : Bytes_value.t; domain : string; path : string;
      size : int; http_only : bool; secure : bool; same_site : Same_site.t;
      expiry : int option; exts : Exts.t; }
  let make
      ?(exts = Exts.none) ~name ~value ~domain ~path ~size ~http_only
      ~secure ~same_site ?expiry ()
    =
    { name; value; domain; path; size; http_only; secure; same_site;
      expiry; exts }
  let name c = c.name
  let value c = c.value
  let domain c = c.domain
  let path c = c.path
  let size c = c.size
  let http_only c = c.http_only
  let secure c = c.secure
  let same_site c = c.same_site
  let expiry c = c.expiry
  let exts c = c.exts
  let jsont =
    let make
        name value domain path size http_only secure same_site expiry exts
      =
      { name; value; domain; path; size; http_only; secure; same_site;
        expiry; exts }
    in
    Jsont.Object.map ~kind:"network.Cookie" make
    |> Jsont.Object.mem "name" Jsont.string ~enc:name
    |> Jsont.Object.mem "value" Bytes_value.jsont ~enc:value
    |> Jsont.Object.mem "domain" Jsont.string ~enc:domain
    |> Jsont.Object.mem "path" Jsont.string ~enc:path
    |> Jsont.Object.mem "size" Js_uint.jsont ~enc:size
    |> Jsont.Object.mem "httpOnly" Jsont.bool ~enc:http_only
    |> Jsont.Object.mem "secure" Jsont.bool ~enc:secure
    |> Jsont.Object.mem "sameSite" Same_site.jsont ~enc:same_site
    |> Jsont.Object.opt_mem "expiry" Js_uint.jsont ~enc:expiry
    |> Jsont.Object.keep_unknown Exts.jsont ~enc:exts
    |> Jsont.Object.finish
end

module Cookie_header = struct
  type t = { name : string; value : Bytes_value.t }
  let make ~name ~value = { name; value }
  let name h = h.name
  let value h = h.value
  let jsont =
    let make name value = { name; value } in
    Jsont.Object.map ~kind:"network.CookieHeader" make
    |> Jsont.Object.mem "name" Jsont.string ~enc:name
    |> Jsont.Object.mem "value" Bytes_value.jsont ~enc:value
    |> Jsont.Object.finish
end

module Data_type = struct
  type t = [`Request | `Response ]
  let jsont =
    Jsont.enum ~kind:"network.DataType"
      ["request", `Request; "response", `Response]
end

module Fetch_timing_info = struct
  type t =
    { time_origin : float; request_time : float; redirect_start : float;
      redirect_end : float; fetch_start : float; dns_start : float;
      dns_end : float; connect_start : float; connect_end : float;
      tls_start : float; request_start : float; response_start : float;
      response_end : float }
  let time_origin i = i.time_origin
  let request_time i = i.request_time
  let redirect_start i = i.redirect_start
  let redirect_end i = i.redirect_end
  let fetch_start i = i.fetch_start
  let dns_start i = i.dns_start
  let dns_end i = i.dns_end
  let connect_start i = i.connect_start
  let connect_end i = i.connect_end
  let tls_start i = i.tls_start
  let request_start i = i.request_start
  let response_start i = i.response_start
  let response_end i = i.response_end
  let jsont =
    let make
        time_origin request_time redirect_start redirect_end
        fetch_start dns_start dns_end connect_start connect_end
        tls_start request_start response_start response_end
      =
      { time_origin; request_time; redirect_start; redirect_end;
        fetch_start; dns_start; dns_end; connect_start; connect_end;
        tls_start; request_start; response_start; response_end}
    in
    Jsont.Object.map ~kind:"network.FetchTimingInfo" make
    |> Jsont.Object.mem "timeOrigin" Jsont.number ~enc:time_origin
    |> Jsont.Object.mem "requestTime" Jsont.number ~enc:request_time
    |> Jsont.Object.mem "redirectStart" Jsont.number ~enc:redirect_start
    |> Jsont.Object.mem "redirectEnd" Jsont.number ~enc:redirect_end
    |> Jsont.Object.mem "fetchStart" Jsont.number ~enc:fetch_start
    |> Jsont.Object.mem "dnsStart" Jsont.number ~enc:dns_start
    |> Jsont.Object.mem "dnsEnd" Jsont.number ~enc:dns_end
    |> Jsont.Object.mem "connectStart" Jsont.number ~enc:connect_start
    |> Jsont.Object.mem "connectEnd" Jsont.number ~enc:connect_end
    |> Jsont.Object.mem "tlsstart" Jsont.number ~enc:tls_start
    |> Jsont.Object.mem "requestStart" Jsont.number ~enc:request_start
    |> Jsont.Object.mem "responseStart" Jsont.number ~enc:response_start
    |> Jsont.Object.mem "responseEnd" Jsont.number ~enc:response_end
    |> Jsont.Object.finish
end

module Header = struct
  type t = { name : string; value : Bytes_value.t }
  let make ~name ~value = { name; value }
  let name h = h.name
  let value h = h.value
  let jsont =
    let make name value = { name; value } in
    Jsont.Object.map ~kind:"network.Header" make
    |> Jsont.Object.mem "name" Jsont.string ~enc:name
    |> Jsont.Object.mem "value" Bytes_value.jsont ~enc:value
    |> Jsont.Object.finish
end

module Request = struct
  type t = string
  let jsont = Jsont.with_doc ~kind:"network.Request" Jsont.string
end

module Initiator_type = struct
  type t = [`Parser | `Script | `Preflight | `Other ]
  let jsont =
    Jsont.enum ~kind:"network.InitatorType" (* not in the spec *)
      ["parser", `Parser; "script", `Script; "preflight", `Preflight;
       "other", `Other ]
end

module Initiator = struct
  type t =
    { column_number : int option;
      line_number : int option;
      request : Request.t option;
      stack_trace : Webs_wd__script.Stack_trace.t option;
      type' : Initiator_type.t option }

  let column_number i = i.column_number
  let line_number i = i.line_number
  let request i = i.request
  let stack_trace i = i.stack_trace
  let type' i = i.type'
  let jsont =
    let make column_number line_number request stack_trace type' =
      { column_number; line_number; request; stack_trace; type' }
    in
    Jsont.Object.map ~kind:"network.Initiator" make
    |> Jsont.Object.opt_mem "columnNumber" Js_uint.jsont ~enc:column_number
    |> Jsont.Object.opt_mem "lineNumber" Js_uint.jsont ~enc:line_number
    |> Jsont.Object.opt_mem "request" Request.jsont ~enc:request
    |> Jsont.Object.opt_mem
      "stackTrace" Webs_wd__script.Stack_trace.jsont ~enc:stack_trace
    |> Jsont.Object.opt_mem "type" Initiator_type.jsont ~enc:type'
    |> Jsont.Object.finish
end

module Intercept = struct
  type t = string
  let jsont = Jsont.with_doc ~kind:"network.Intercept" Jsont.string
end

module Intercept_phase = struct
  type t = [`Before_request_sent | `Response_started | `Auth_required ]
  let jsont =
    Jsont.enum ~kind:"network.InterceptPhase"
      [ "beforeRequestSent", `Before_request_sent;
        "responseStarted", `Response_started;
        "authRequired", `Auth_required ]
end

module Request_data = struct
  type t =
    { request : Request.t; url : string; method' : string;
      headers : Header.t list; cookies : Cookie.t list;
      header_size : int option; body_size : int option; destination : string;
      initator_type : string; timings : Fetch_timing_info.t; }
  let request d = d.request
  let url d = d.url
  let method' d = d.method'
  let headers d = d.headers
  let cookies d = d.cookies
  let header_size d = d.header_size
  let body_size d = d.body_size
  let destination d = d.destination
  let initator_type d = d.initator_type
  let timings d = d.timings
  let jsont =
    let make
        request url method' headers cookies header_size body_size
        destination initator_type timings =
      { request; url; method'; headers; cookies; header_size; body_size;
        destination; initator_type; timings}
    in
    Jsont.Object.map ~kind:"network.RequestData" make
    |> Jsont.Object.mem "request" Request.jsont ~enc:request
    |> Jsont.Object.mem "url" Jsont.string ~enc:url
    |> Jsont.Object.mem "method" Jsont.string ~enc:method'
    |> Jsont.Object.mem "headers" (Jsont.list Header.jsont)  ~enc:headers
    |> Jsont.Object.mem "cookies" (Jsont.list Cookie.jsont) ~enc:cookies
    |> Jsont.Object.mem
      "headerSize" (Jsont.option Js_uint.jsont) ~enc:header_size
    |> Jsont.Object.mem
      "bodySize" (Jsont.option Js_uint.jsont) ~enc:body_size
    |> Jsont.Object.mem "destination" Jsont.string ~enc:destination
    |> Jsont.Object.mem "initiatorType" Jsont.string ~enc:initator_type
    |> Jsont.Object.mem "timings" Fetch_timing_info.jsont ~enc:timings
    |> Jsont.Object.finish
end

module Response_content = struct
  type t = { size : int }
  let size c = c.size
  let jsont =
    let make size = { size } in
    Jsont.Object.map ~kind:"network.ResponseContent" make
    |> Jsont.Object.mem "size" Js_uint.jsont ~enc:size
    |> Jsont.Object.finish
end

module Response_data = struct
  type t =
    { url : string; protocol : string; status : int; status_text : string;
      from_cache : bool; headers : Header.t list; mime_type : string;
      bytes_received : int; header_size : int option; body_size : int option;
      content : Response_content.t;
      auth_challenges : Auth_challenge.t list option }
  let url d = d.url
  let protocol d = d.protocol
  let status d = d.status
  let status_text d = d.status_text
  let from_cache d = d.from_cache
  let headers d = d.headers
  let mime_type d = d.mime_type
  let bytes_received d = d.bytes_received
  let header_size d = d.header_size
  let body_size d = d.body_size
  let content d = d.content
  let auth_challenges d = d.auth_challenges
  let jsont =
    let make
        url protocol status status_text from_cache headers mime_type
        bytes_received header_size body_size content auth_challenges
      =
      { url; protocol; status; status_text; from_cache; headers; mime_type;
        bytes_received; header_size; body_size; content; auth_challenges }
    in
    Jsont.Object.map ~kind:"network.ResponseData" make
    |> Jsont.Object.mem "url" Jsont.string ~enc:url
    |> Jsont.Object.mem "protocol" Jsont.string ~enc:protocol
    |> Jsont.Object.mem "status" Js_uint.jsont ~enc:status
    |> Jsont.Object.mem "statusText" Jsont.string ~enc:status_text
    |> Jsont.Object.mem "fromCache" Jsont.bool ~enc:from_cache
    |> Jsont.Object.mem "headers" Jsont.(list Header.jsont) ~enc:headers
    |> Jsont.Object.mem "mimeType" Jsont.string ~enc:mime_type
    |> Jsont.Object.mem "bytesReceived" Js_uint.jsont ~enc:bytes_received
    |> Jsont.Object.mem
      "headerSize" (Jsont.option Js_uint.jsont) ~enc:header_size
    |> Jsont.Object.mem "bodySize" (Jsont.option Js_uint.jsont) ~enc:body_size
    |> Jsont.Object.mem "content" Response_content.jsont ~enc:content
    |> Jsont.Object.opt_mem
      "authChallenges" (Jsont.list Auth_challenge.jsont) ~enc:auth_challenges
    |> Jsont.Object.finish
end

module Set_cookie_header = struct
  type t =
    { name : string; value : Bytes_value.t; domain : string option;
      http_only : bool option; expiry : string option; max_age : int option;
      path : string option; same_site : Same_site.t option;
      secure : bool option  }
  let make
      ~name ~value ?domain ?http_only ?expiry ?max_age ?path ?same_site ?secure
      ()
    =
    { name; value; domain; http_only; expiry; max_age; path; same_site; secure }
  let name c = c.name
  let value c = c.value
  let domain c = c.domain
  let http_only c = c.http_only
  let expiry c = c.expiry
  let max_age c = c.max_age
  let path c = c.path
  let same_site c = c.same_site
  let secure c = c.secure
  let jsont =
    let make name value domain http_only expiry max_age path same_site secure =
      { name; value; domain; http_only; expiry; max_age; path; same_site;
        secure }
    in
    Jsont.Object.map ~kind:"network.SetCookieHeader" make
    |> Jsont.Object.mem "name" Jsont.string ~enc:name
    |> Jsont.Object.mem "value" Bytes_value.jsont ~enc:value
    |> Jsont.Object.opt_mem "domain" Jsont.string ~enc:domain
    |> Jsont.Object.opt_mem "httpOnly" Jsont.bool ~enc:http_only
    |> Jsont.Object.opt_mem "expiry" Jsont.string ~enc:expiry
    |> Jsont.Object.opt_mem "maxAge" Js_int.jsont ~enc:max_age
    |> Jsont.Object.opt_mem "path" Jsont.string ~enc:path
    |> Jsont.Object.opt_mem "sameSite" Same_site.jsont ~enc:same_site
    |> Jsont.Object.opt_mem "secure" Jsont.bool ~enc:secure
    |> Jsont.Object.finish
end

module Url_pattern_pattern = struct
  type t =
    { protocol : string option;
      hostname : string option;
      port : string option;
      pathname : string option;
      search : string option; }
  let make ?protocol ?hostname ?port ?pathname ?search () =
    { protocol; hostname; port; pathname; search }
  let protocol p = p.protocol
  let hostname p = p.hostname
  let port p = p.port
  let pathname p = p.pathname
  let search p = p.search
  let jsont =
    let make protocol hostname port pathname search =
      { protocol; hostname; port; pathname; search }
    in
    Jsont.Object.map ~kind:"network.UrlPatternPattern" make
    |> Jsont.Object.opt_mem "protocol" Jsont.string ~enc:protocol
    |> Jsont.Object.opt_mem "hostname" Jsont.string ~enc:hostname
    |> Jsont.Object.opt_mem "port" Jsont.string ~enc:port
    |> Jsont.Object.opt_mem "pathname" Jsont.string ~enc:pathname
    |> Jsont.Object.opt_mem "search" Jsont.string ~enc:search
    |> Jsont.Object.finish
end

module Url_pattern_string = struct
  type t = { pattern : string }
  let make ~pattern () = { pattern }
  let pattern p = p.pattern
  let jsont =
    let make pattern = { pattern } in
    Jsont.Object.map ~kind:"newtork.UrlPatternString" make
    |> Jsont.Object.mem "pattern" Jsont.string ~enc:pattern
    |> Jsont.Object.finish
end

module Url_pattern = struct
  type t =
  [ `Pattern of Url_pattern_pattern.t
  | `String of Url_pattern_string.t ]

  let jsont =
    let pattern =
      Jsont.Object.Case.map "pattern" Url_pattern_pattern.jsont
        ~dec:(fun p -> `Pattern p)
    in
    let string =
      Jsont.Object.Case.map "string" Url_pattern_string.jsont
        ~dec:(fun p -> `String p)
    in
    let enc_case = function
    | `Pattern p -> Jsont.Object.Case.value pattern p
    | `String s -> Jsont.Object.Case.value string s
    in
    let cases = Jsont.Object.Case.[make pattern; make string] in
    Jsont.Object.map ~kind:"network.UrlPatternString" Fun.id
    |> Jsont.Object.case_mem
      "type" Jsont.string cases ~enc_case ~enc:Fun.id ~tag_to_string:Fun.id
    |> Jsont.Object.finish
end

(* Command results *)

module Add_data_collector_result = struct
  type t = { collector : Collector.t }
  let collector r = r.collector
  let jsont =
    let make collector = { collector } in
    Jsont.Object.map ~kind:"network.AddDataCollectorResult" make
    |> Jsont.Object.mem "collector" Collector.jsont ~enc:collector
    |> Jsont.Object.finish
end

module Add_intercept_result = struct
  type t = { intercept : Intercept.t }
  let intercept r = r.intercept
  let jsont =
    let make intercept = { intercept } in
    Jsont.Object.map ~kind:"network.AddInterceptResult" make
    |> Jsont.Object.mem "intercept" Intercept.jsont ~enc:intercept
    |> Jsont.Object.finish
end

module Get_data_result = struct
  type t = { bytes : Bytes_value.t }
  let bytes b = b.bytes
  let jsont =
    let make bytes = { bytes } in
    Jsont.Object.map ~kind:"network.GetDataResult" make
    |> Jsont.Object.mem "bytes" Bytes_value.jsont ~enc:bytes
    |> Jsont.Object.finish
end

(* Event parameters *)

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

module Event_base_parameters = struct
  type 'a t =
    { context : Browsing_context.t option;
      is_blocked : bool;
      navigation : Webs_wd__browsing_context.Navigation.t;
      redirect_count : int;
      request : Request_data.t;
      timestamp : int;
      user_context : User_context.t option option;
      intercepts : Intercept.t list option;
      other : 'a }
  let context p = p.context
  let is_blocked p = p.is_blocked
  let navigation p = p.navigation
  let redirect_count p = p.redirect_count
  let request p = p.request
  let timestamp p = p.timestamp
  let user_context p = p.user_context
  let intercepts p = p.intercepts
  let other p = p.other
  let make
      context is_blocked navigation redirect_count request timestamp
      user_context intercepts other
    =
    { context; is_blocked; navigation; redirect_count; request; timestamp;
      user_context; intercepts; other }
  let jsont_open ~kind make =
    Jsont.Object.map ~kind make
    |> Jsont.Object.mem
      "context" (Jsont.option Browsing_context.jsont) ~enc:context
    |> Jsont.Object.mem "isBlocked" Jsont.bool ~enc:is_blocked
    |> Jsont.Object.mem
      "navigation" Webs_wd__browsing_context.Navigation.jsont ~enc:navigation
    |> Jsont.Object.mem "redirectCount" Js_uint.jsont ~enc:redirect_count
    |> Jsont.Object.mem "request" Request_data.jsont ~enc:request
    |> Jsont.Object.mem "timestamp" Js_uint.jsont ~enc:timestamp
    |> Jsont.Object.opt_mem
      "userContext" (Jsont.option User_context.jsont) ~enc:user_context
    |> Jsont.Object.opt_mem
      "intercepts"  (Jsont.list Intercept.jsont) ~enc:intercepts
end

module Auth_required_parameters = struct
  include Event_base_parameters
  type nonrec t = Response_data.t t
  let response p = p.other
  let jsont =
    jsont_open ~kind:"network.AuthRequiredParameters" make
    |> Jsont.Object.mem "response" Response_data.jsont ~enc:response
    |> Jsont.Object.finish
end

module Before_request_sent_parameters = struct
  include Event_base_parameters
  type nonrec t = Initiator.t option t
  let initiator p = p.other
  let jsont =
    jsont_open ~kind:"network.BeforeRequestSentParameters" make
    |> Jsont.Object.opt_mem "initator" Initiator.jsont ~enc:initiator
    |> Jsont.Object.finish
end

module Fetch_error_parameters = struct
  include Event_base_parameters
  type nonrec t = string t
  let error_text p = p.other
  let jsont =
    jsont_open ~kind:"network.FetchErrorParameters" make
    |> Jsont.Object.mem "errorText" Jsont.string ~enc:error_text
    |> Jsont.Object.finish
end

module Response_completed_parameters = struct
  include Event_base_parameters
  type nonrec t = Response_data.t t
  let response p = p.other
  let jsont =
    jsont_open ~kind:"network.ResponseCompletedParameters" make
    |> Jsont.Object.mem "response" Response_data.jsont ~enc:response
    |> Jsont.Object.finish
end

module Response_started_parameters = struct
  include Event_base_parameters
  type nonrec t = Response_data.t t
  let response p = p.other
  let jsont =
    jsont_open ~kind:"network.ResponseStartedParameters" make
    |> Jsont.Object.mem "response" Response_data.jsont ~enc:response
    |> Jsont.Object.finish
end

(* [network.AddDataCollector] *)

type add_data_collector_params =
  { data_types : Data_type.t list;
    max_encoded_data_size : int;
    collector_type : Collector_type.t option;
    contexts : Browsing_context.t list option;
    user_contexts : User_context.t list option; }

let add_data_collector_params_jsont =
  let make
      data_types max_encoded_data_size collector_type contexts
      user_contexts
    =
    { data_types; max_encoded_data_size; collector_type; contexts;
      user_contexts }
  in
  let data_types p = p.data_types
  and max_encoded_data_size p = p.max_encoded_data_size
  and collector_type p = p.collector_type
  and contexts p = p.contexts and user_contexts p = p.user_contexts in
  Jsont.Object.map ~kind:"network.AddDataCollectorParameters" make
  |> Jsont.Object.mem "dataTypes" (Jsont.list Data_type.jsont) ~enc:data_types
  |> Jsont.Object.mem
    "maxEncodedDataSize" Js_uint.jsont ~enc:max_encoded_data_size
  |> Jsont.Object.opt_mem
    "collectorType" Collector_type.jsont ~enc:collector_type
  |> Jsont.Object.opt_mem
    "contexts" (Jsont.list Browsing_context.jsont) ~enc:contexts
  |> Jsont.Object.opt_mem
    "userContexts" (Jsont.list User_context.jsont) ~enc:user_contexts
  |> Jsont.Object.finish

let add_data_collector_command =
  let params_jsont = add_data_collector_params_jsont in
  let result_jsont = Add_data_collector_result.jsont in
  Command.define "network.addDataCollector" ~params_jsont ~result_jsont

let add_data_collector
    c ?exts ~data_types ~max_encoded_data_size ?collector_type ?contexts
    ?user_contexts ()
  =
  let params =
    { data_types; max_encoded_data_size; collector_type; contexts;
      user_contexts}
  in
  call c ?exts add_data_collector_command params

(* [network.addIntercept] *)

type add_intercept_params =
  { phases : Intercept_phase.t list ;
    contexts : Browsing_context.t list option;
    url_patterns : Url_pattern.t list option }

let add_intercept_params_jsont =
  let make phases contexts url_patterns = { phases; contexts; url_patterns } in
  let phases p = p.phases and contexts p = p.contexts
  and url_patterns p = p.url_patterns in
  Jsont.Object.map ~kind:"network.AddInterceptParameters" make
  |> Jsont.Object.mem "phases" (Jsont.list Intercept_phase.jsont) ~enc:phases
  |> Jsont.Object.opt_mem
    "contexts" (Jsont.list Browsing_context.jsont) ~enc:contexts
  |> Jsont.Object.opt_mem
    "urlPatterns" (Jsont.list Url_pattern.jsont) ~enc:url_patterns
  |> Jsont.Object.finish

let add_intercept_command =
  let params_jsont = add_intercept_params_jsont in
  let result_jsont = Add_intercept_result.jsont in
  Command.define "network.addIntercept" ~params_jsont ~result_jsont

let add_intercept c ?exts ~phases ?contexts ?url_patterns () =
  let params = { phases; contexts; url_patterns } in
  call c ?exts add_intercept_command params

(* [network.continueRequest] *)

type continue_request_params =
  { request : Request.t;
    body : Bytes_value.t option;
    cookies : Cookie_header.t list option;
    headers : Header.t list option;
    method' : string option;
    url : string option; }

let continue_request_params_jsont =
  let make request body cookies headers method' url =
    { request; body; cookies; headers; method'; url }
  in
  let request p = p.request and body p = p.body and cookies p = p.cookies
  and headers p = p.headers and method' p = p.method' and url p = p.url in
  Jsont.Object.map ~kind:"network.ContinueRequestParameters" make
  |> Jsont.Object.mem "request" Request.jsont ~enc:request
  |> Jsont.Object.opt_mem "body" Bytes_value.jsont ~enc:body
  |> Jsont.Object.opt_mem
    "cookies" (Jsont.list Cookie_header.jsont) ~enc:cookies
  |> Jsont.Object.opt_mem "headers" (Jsont.list Header.jsont) ~enc:headers
  |> Jsont.Object.opt_mem "method" Jsont.string ~enc:method'
  |> Jsont.Object.opt_mem "url" Jsont.string ~enc:url
  |> Jsont.Object.finish

let continue_request_command =
  let params_jsont = continue_request_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "network.continueRequest" ~params_jsont ~result_jsont

let continue_request c ?exts ~request ?body ?cookies ?headers ?method' ?url ()=
  let params = { request; body; cookies; headers; method'; url } in
  call c ?exts continue_request_command params

(* [network.continueResponse] *)

type continue_response_params =
  { request : Request.t;
    cookies : Set_cookie_header.t list option;
    credentials : Auth_credentials.t option;
    headers : Header.t list option;
    reason_phrase : string option;
    status_code : int option }

let continue_request_params_jsont =
  let make request cookies credentials headers reason_phrase status_code =
    { request; cookies; credentials; headers; reason_phrase; status_code }
  in
  let request p = p.request and cookies p = p.cookies
  and credentials p = p.credentials and headers p = p.headers
  and reason_phrase p = p.reason_phrase and status_code p = p.status_code in
  Jsont.Object.map ~kind:"network.ContinueResponseParameters" make
  |> Jsont.Object.mem "request" Request.jsont ~enc:request
  |> Jsont.Object.opt_mem
    "cookies" (Jsont.list Set_cookie_header.jsont) ~enc:cookies
  |> Jsont.Object.opt_mem "credentials" Auth_credentials.jsont ~enc:credentials
  |> Jsont.Object.opt_mem "headers" (Jsont.list Header.jsont) ~enc:headers
  |> Jsont.Object.opt_mem "reasonPhrase" Jsont.string ~enc:reason_phrase
  |> Jsont.Object.opt_mem "statusCode" Js_uint.jsont ~enc:status_code
  |> Jsont.Object.finish

let continue_request_command =
  let params_jsont = continue_request_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "network.continueResponse" ~params_jsont ~result_jsont

let continue_response
    c ?exts ~request ?cookies ?credentials ?headers ?reason_phrase ?status_code
    ()
  =
  let params =
    { request; cookies; credentials; headers; reason_phrase; status_code}
  in
  call c ?exts continue_request_command params

(* [network.continueWithAuthResult] *)

type continue_with_auth_params =
  { request : Request.t;
    action : string;
    credentials : Auth_credentials.t option }

let continue_with_auth_params_jsont =
  let make request action credentials = { request; action; credentials } in
  let request p = p.request and action p = p.action
  and credentials p = p.credentials in
  Jsont.Object.map ~kind:"network.ContinueWithAuthParameters" make
  |> Jsont.Object.mem "request" Request.jsont ~enc:request
  |> Jsont.Object.mem "action" Jsont.string ~enc:action
  |> Jsont.Object.opt_mem "credentials" Auth_credentials.jsont ~enc:credentials
  |> Jsont.Object.finish

let continue_with_auth_command =
  let params_jsont = continue_with_auth_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "network.continueWithAuth" ~params_jsont ~result_jsont

let continue_with_auth c ?exts ~request ~action () =
  let action, credentials = match action with
  | `Provide_credentials creds -> "provideCredentials", Some creds
  | `Default -> "default", None | `Cancel -> "cancel", None
  in
  let params = { request; action; credentials } in
  call c ?exts continue_with_auth_command params

(* [network.disownData] *)

type disown_data_params =
  { data_type : Data_type.t;
    collector : Collector.t;
    request : Request.t }

let disown_data_params_jsont =
  let make data_type collector request = { data_type; collector; request } in
  let data_type p = p.data_type and collector p = p.collector
  and request p = p.request in
  Jsont.Object.map ~kind:"network.DisownDataParameters" make
  |> Jsont.Object.mem "dataType" Data_type.jsont ~enc:data_type
  |> Jsont.Object.mem "collector" Collector.jsont ~enc:collector
  |> Jsont.Object.mem "request" Request.jsont ~enc:request
  |> Jsont.Object.finish

let disown_data_command =
  let params_jsont = disown_data_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "network.disownData" ~params_jsont ~result_jsont

let disown_data c ?exts ~data_type ~collector ~request () =
  let params = { data_type; collector; request } in
  call c ?exts disown_data_command params

(* [network.failRequest] *)

type fail_request_params = { request : Request.t }
let fail_request_param_jsont =
  let make request = { request } and request p = p.request in
  Jsont.Object.map ~kind:"network.FailRequestParameters" make
  |> Jsont.Object.mem "request" Request.jsont ~enc:request
  |> Jsont.Object.finish

let fail_request_command =
  let params_jsont = fail_request_param_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "network.failRequest" ~params_jsont ~result_jsont

let fail_request c ?exts ~request () =
  call c ?exts fail_request_command { request }

(* [network.getData] *)

type get_data_params =
  { data_type : Data_type.t;
    collector : Collector.t option;
    disown : bool option;
    request : Request.t }

let get_data_params_jsont =
  let make data_type collector disown request =
    { data_type; collector; disown; request }
  in
  let data_type p = p.data_type and collector p = p.collector
  and disown p = p.disown and request p = p.request in
  Jsont.Object.map ~kind:"networkGetDataParameters" make
  |> Jsont.Object.mem "dataType" Data_type.jsont ~enc:data_type
  |> Jsont.Object.opt_mem "collector" Collector.jsont ~enc:collector
  |> Jsont.Object.opt_mem "disown" Jsont.bool ~enc:disown
  |> Jsont.Object.mem "request" Request.jsont ~enc:request
  |> Jsont.Object.finish

let get_data_command =
  let params_jsont = get_data_params_jsont in
  let result_jsont = Get_data_result.jsont in
  Command.define "network.getData" ~params_jsont ~result_jsont

let get_data c ?exts ~data_type ?collector ?disown ~request () =
  let params = { data_type; collector; disown; request } in
  call c ?exts get_data_command params

(* [network.provideResponse] *)

type provide_response_params =
  { request : Request.t;
    body : Bytes_value.t option;
    cookies : Set_cookie_header.t list option;
    headers : Header.t list option;
    reason_phrase : string option;
    status_code : int option }

let provide_response_params_jsont =
  let make request body cookies headers reason_phrase status_code =
    { request; body; cookies; headers; reason_phrase; status_code }
  in
  let request p = p.request and body p = p.body and cookies p = p.cookies
  and headers p = p.headers and reason_phrase p = p.reason_phrase
  and status_code p = p.status_code in
  Jsont.Object.map ~kind:"network.ProvideResponseParameters" make
  |> Jsont.Object.mem "request" Request.jsont ~enc:request
  |> Jsont.Object.opt_mem "body" Bytes_value.jsont ~enc:body
  |> Jsont.Object.opt_mem
    "cookies" (Jsont.list Set_cookie_header.jsont) ~enc:cookies
  |> Jsont.Object.opt_mem "headers" (Jsont.list Header.jsont) ~enc:headers
  |> Jsont.Object.opt_mem "reasonPhrase" Jsont.string ~enc:reason_phrase
  |> Jsont.Object.opt_mem "statusCode" Js_uint.jsont ~enc:status_code
  |> Jsont.Object.finish

let provide_response_command =
  let params_jsont = provide_response_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "network.provideResponse" ~params_jsont ~result_jsont

let provide_response
    c ?exts ~request ?body ?cookies ?headers ?reason_phrase ?status_code ()
  =
  let params = { request; body; cookies; headers; reason_phrase; status_code} in
  call c ?exts provide_response_command params

(* [network.removeDataCollector] *)

type remove_data_collector_params = { collector : Collector.t }
let remove_data_collector_param_jsont =
  let make collector = { collector } and collector p = p.collector in
  Jsont.Object.map ~kind:"network.RemoveDataCollectorParameters" make
  |> Jsont.Object.mem "collector" Request.jsont ~enc:collector
  |> Jsont.Object.finish

let remove_data_collector_command =
  let params_jsont = remove_data_collector_param_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "network.removeDataCollector" ~params_jsont ~result_jsont

let remove_data_collector c ?exts ~collector () =
  call c ?exts remove_data_collector_command { collector }

(* [network.removeIntercept] *)

type remove_intercept_params = { intercept : Intercept.t }
let remove_intercept_param_jsont =
  let make intercept = { intercept } and intercept p = p.intercept in
  Jsont.Object.map ~kind:"network.RemoveInterceptParameters" make
  |> Jsont.Object.mem "intercept" Request.jsont ~enc:intercept
  |> Jsont.Object.finish

let remove_intercept_command =
  let params_jsont = remove_intercept_param_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "network.removeIntercept" ~params_jsont ~result_jsont

let remove_intercept c ?exts ~intercept () =
  call c ?exts remove_intercept_command { intercept }

(* [network.setCacheBehavior] *)

type set_cache_behavior_params =
  { cache_behavior : string;
    contexts : Browsing_context.t list option }

let set_cache_behavior_params_jsont =
  let make cache_behavior contexts = { cache_behavior; contexts } in
  let cache_behavior p = p.cache_behavior and contexts p = p.contexts in
  Jsont.Object.map ~kind:"network.SetCacheBehaviorParameters" make
  |> Jsont.Object.mem "cacheBehavior" Jsont.string ~enc:cache_behavior
  |> Jsont.Object.opt_mem
    "context" (Jsont.list Browsing_context.jsont) ~enc:contexts
  |> Jsont.Object.finish

let set_cache_behavior_command =
  let params_jsont = set_cache_behavior_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "network.setCacheBehavior" ~params_jsont ~result_jsont

let set_cache_behavior c ?exts ~cache_behavior ?contexts () =
  let cache_behavior = match cache_behavior with
  | `Default -> "default" | `Bypass -> "bypass"
  in
  let params = { cache_behavior; contexts } in
  call c ?exts set_cache_behavior_command params

(* [network.setExtraHeaders] *)

type set_extra_headers_params =
  { headers : Header.t list;
    contexts : Browsing_context.t list option;
    user_contexts : User_context.t list option; }

let set_extra_headers_params_jsont =
  let make headers contexts user_contexts =
    { headers; contexts; user_contexts }
  in
  let headers p = p.headers
  and contexts p = p.contexts and user_contexts p = p.user_contexts in
  Jsont.Object.map ~kind:"network.setExtraHeadersParameters" make
  |> Jsont.Object.mem "headers" (Jsont.list Header.jsont) ~enc:headers
  |> Jsont.Object.opt_mem
    "contexts" (Jsont.list Browsing_context.jsont) ~enc:contexts
  |> Jsont.Object.opt_mem
    "userContexts" (Jsont.list User_context.jsont) ~enc:user_contexts
  |> Jsont.Object.finish

let set_extra_headers_command =
  let params_jsont = set_extra_headers_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "network.addDataCollector" ~params_jsont ~result_jsont

let set_extra_headers c ?exts ~headers ?contexts ?user_contexts () =
  let params = { headers; contexts; user_contexts} in
  call c ?exts set_extra_headers_command params

(* Events *)

let auth_required =
  let params_jsont = Auth_required_parameters.jsont in
  Event.define "network.authRequired" ~params_jsont

let before_request_sent =
  let params_jsont = Before_request_sent_parameters.jsont in
  Event.define "network.beforeRequestSent" ~params_jsont

let fetch_error =
  let params_jsont = Fetch_error_parameters.jsont in
  Event.define "network.fetchError" ~params_jsont

let response_completed =
  let params_jsont = Response_completed_parameters.jsont in
  Event.define "network.responseCompleted" ~params_jsont

let response_started =
  let params_jsont = Response_started_parameters.jsont in
  Event.define "network.responseStarted" ~params_jsont
