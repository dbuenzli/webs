(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs_wd__types
open Webs_wd__protocol

let name = "script"

(* Types *)

module Serialization_options = struct
  type t =
    { max_dom_depth : int option option;
      max_object_depth : int option option;
      include_shadow_tree : string option }

  let make ?max_dom_depth ?max_object_depth ?include_shadow_tree () =
    let include_shadow_tree = match include_shadow_tree with
    | None -> None | Some `None -> Some "none" | Some `Open -> Some "open"
    | Some `All -> Some "all"
    in
    { max_dom_depth; max_object_depth; include_shadow_tree; }

  let jsont =
    let make max_dom_depth max_object_depth include_shadow_tree =
      { max_dom_depth; max_object_depth; include_shadow_tree }
    in
    let max_dom_depth o = o.max_dom_depth
    and max_object_depth o = o.max_object_depth
    and include_shadow_tree o = o.include_shadow_tree in
    Jsont.Object.map ~kind:"script.ScriptSerizationObjects" make
    |> Jsont.Object.opt_mem
      "maxDomDepth" (Jsont.option Js_uint.jsont) ~enc:max_dom_depth
    |> Jsont.Object.opt_mem
      "maxObjectDepth" (Jsont.option Js_uint.jsont) ~enc:max_object_depth
    |> Jsont.Object.opt_mem
      "includeShadowTree" Jsont.string ~enc:include_shadow_tree
    |> Jsont.Object.finish
end

module Result_ownership = struct
  type t = [ `Root | `None ]
  let jsont =
    Jsont.enum ~kind:"script.ResultOwnership"
      ["root", `Root; "none", `None]
end

module Channel = struct
  type t = string
  let jsont = Jsont.with_doc ~kind:"script.Channel" Jsont.string
end

module Channel_properties = struct
  type t =
    { channel : Channel.t;
      serialization_options : Serialization_options.t option;
      ownership : Result_ownership.t option }
  let make ~channel ?serialization_options ?ownership () =
    { channel; serialization_options; ownership }
  let channel p = p.channel
  let serialization_options p = p.serialization_options
  let ownership p = p.ownership
  let jsont =
    let make channel serialization_options ownership =
      { channel; serialization_options; ownership }
    in
    Jsont.Object.map ~kind:"ChannelProperties" make
    |> Jsont.Object.mem "channel" Channel.jsont ~enc:channel
    |> Jsont.Object.opt_mem
      "serializationOptions" Serialization_options.jsont
      ~enc:serialization_options
    |> Jsont.Object.opt_mem "ownership" Result_ownership.jsont ~enc:ownership
    |> Jsont.Object.finish
end

module Channel_value = struct
  type t = { value : Channel_properties.t }
  let make ~value () = { value }
  let value v = v.value
  let jsont =
    let make value = { value } in
    Jsont.Object.map ~kind:"script.ChannelValue" make
    |> Jsont.Object.mem "value" Channel_properties.jsont
    |> Jsont.Object.finish
end

module Internal_id = struct
  type t = string
  let jsont = Jsont.with_doc ~kind:"script.InternalId" Jsont.string
end

module Handle = struct
  type t = string
  let jsont = Jsont.with_doc ~kind:"script.Handle" Jsont.string
end

module Preload_script = struct
  type t = string
  let jsont = Jsont.with_doc ~kind:"script.PreloadScript" Jsont.string
end

module Realm = struct
  type t = string
  let jsont = Jsont.with_doc ~kind:"script.Realm" Jsont.string
end

module Realm_info = struct
  module Window = struct
    type t =
      { context : Browsing_context.t;
        user_context : User_context.t option;
        sandbox : string option }
    let context i = i.context
    let user_context i = i.user_context
    let sandbox i = i.sandbox
    let jsont =
      let make context user_context sandbox =
        { context; user_context; sandbox}
      in
      Jsont.Object.map ~kind:"script.WindowRealmInfo" make
      |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:context
      |> Jsont.Object.opt_mem "userContext" User_context.jsont ~enc:user_context
      |> Jsont.Object.opt_mem "sandbox" User_context.jsont ~enc:user_context
      |> Jsont.Object.finish
  end
  module Dedicated_worker = struct
    type t = { owners : Realm.t list }
    let owners i = i.owners
    let jsont =
      let make owners = { owners } in
      Jsont.Object.map ~kind:"script.DedicatedWorkerRealmInfo" make
      |> Jsont.Object.mem "owners" (Jsont.list Realm.jsont) ~enc:owners
      |> Jsont.Object.finish
  end
  module Shared_worker = struct
    type t = unit
    let jsont =
      Jsont.Object.map ~kind:"script.SharedWorkerRealmInfo" ()
      |> Jsont.Object.finish
  end
  module Service_worker = struct
    type t = unit
    let jsont =
      Jsont.Object.map ~kind:"script.ServiceWorkerRealmInfo" ()
      |> Jsont.Object.finish
  end
  module Worker = struct
    type t = unit
    let jsont =
      Jsont.Object.map ~kind:"script.WorkerRealmInfo" ()
      |> Jsont.Object.finish
  end
  module Paint_worklet = struct
    type t = unit
    let jsont =
      Jsont.Object.map ~kind:"script.PaintWorkletRealmInfo" ()
      |> Jsont.Object.finish
  end
  module Audio_worklet = struct
    type t = unit
    let jsont =
      Jsont.Object.map ~kind:"script.AudioWorkletRealmInfo" ()
      |> Jsont.Object.finish
  end
  module Worklet = struct
    type t = unit
    let jsont =
      Jsont.Object.map ~kind:"script.WorkletRealmInfo" ()
      |> Jsont.Object.finish
  end
  type type' =
  [ `Window of Window.t
  | `Dedicated_worker of Dedicated_worker.t
  | `Shared_worker of Shared_worker.t
  | `Service_worker of Service_worker.t
  | `Worker of Worker.t
  | `Paint_worklet of Paint_worklet.t
  | `Audio_worklet of Audio_worklet.t
  | `Worklet of Worklet.t ]
  type t =
    { realm : Realm.t;
      origin : string;
      type' : type'; }

  let realm i = i.realm
  let origin i = i.origin
  let type' i = i.type'
  let jsont =
    let window =
      Jsont.Object.Case.map "window" Window.jsont ~dec:(fun v -> `Window v)
    in
    let dedicated_worker =
      Jsont.Object.Case.map "dedicated-worker" Dedicated_worker.jsont
        ~dec:(fun v -> `Dedicated_worker v)
    in
    let shared_worker =
      Jsont.Object.Case.map "shared-worker" Shared_worker.jsont
        ~dec:(fun v -> `Shared_worker v)
    in
    let service_worker =
      Jsont.Object.Case.map "service-worker" Service_worker.jsont
        ~dec:(fun v -> `Service_worker v)
    in
    let worker =
      Jsont.Object.Case.map "worker" Worker.jsont
        ~dec:(fun v -> `Worker v)
    in
    let paint_worklet =
      Jsont.Object.Case.map "paint-worklet" Paint_worklet.jsont
        ~dec:(fun v -> `Paint_worklet v)
    in
    let audio_worklet =
      Jsont.Object.Case.map "audio-worklet" Audio_worklet.jsont
        ~dec:(fun v -> `Audio_worklet v)
    in
    let worklet =
      Jsont.Object.Case.map "audio-worklet" Worklet.jsont
        ~dec:(fun v -> `Worklet v)
    in
    let enc_case = function
    | `Window w -> Jsont.Object.Case.value window w
    | `Dedicated_worker w -> Jsont.Object.Case.value dedicated_worker w
    | `Shared_worker w -> Jsont.Object.Case.value shared_worker w
    | `Service_worker w -> Jsont.Object.Case.value service_worker w
    | `Worker w -> Jsont.Object.Case.value worker w
    | `Paint_worklet w -> Jsont.Object.Case.value paint_worklet w
    | `Audio_worklet w -> Jsont.Object.Case.value audio_worklet w
    | `Worklet w -> Jsont.Object.Case.value worklet w
    in
    let cases = Jsont.Object.Case.[
        make window; make dedicated_worker; make shared_worker;
        make service_worker; make worker; make paint_worklet;
        make audio_worklet; make worklet ]
    in
    let make realm origin type' = { realm; origin; type' } in
    Jsont.Object.map ~kind:"script.RealmInfo" make
    |> Jsont.Object.mem "realm" Realm.jsont ~enc:realm
    |> Jsont.Object.mem "origin" Jsont.string ~enc:origin
    |> Jsont.Object.case_mem "type"
      Jsont.string cases ~enc:type' ~enc_case ~tag_to_string:Fun.id
    |> Jsont.Object.finish
end

module Realm_type = struct
  type t =
  [ `Window | `Dedicated_worker | `Shared_worker | `Service_worker
  | `Worker | `Paint_worklet | `Audio_worklet | `Worklet ]

  let jsont = Jsont.enum ~kind:"script.RealmType"
      [ "window", `Window;
        "dedicated-worker", `Dedicated_worker;
        "shared-worker", `Shared_worker;
        "service-worker", `Service_worker;
        "worker", `Worker;
        "paint-worklet", `Paint_worklet;
        "worklet", `Worklet; ]
end

module Shared_id = struct
  type t = string
  let jsont = Jsont.with_doc ~kind:"script.SharedId" Jsont.string
end

module Shared_reference = struct
  type t =
    { shared_id : Shared_id.t;
      handle : string option;
      exts : Exts.t }

  let make ?(exts = Exts.none) ?handle ~shared_id () =
    { shared_id; handle; exts }

  let jsont =
    let make shared_id handle exts = { shared_id; handle; exts } in
    let shared_id r = r.shared_id and handle r = r.handle and exts r = r.exts in
    Jsont.Object.map ~kind:"script.SharedReference" make
    |> Jsont.Object.mem "sharedId" Shared_id.jsont ~enc:shared_id
    |> Jsont.Object.opt_mem "handle" Handle.jsont ~enc:handle
    |> Jsont.Object.keep_unknown Exts.jsont ~enc:exts
    |> Jsont.Object.finish
end

module Stack_frame = struct
  type t =
    { column_number : int;
      function_name : string;
      line_number : int;
      url : string; }
  let column_number f = f.column_number
  let function_name f = f.function_name
  let line_number f = f.line_number
  let url f = f.url
  let jsont =
    let make column_number function_name line_number url =
      { column_number; function_name; line_number; url; }
    in
    Jsont.Object.map ~kind:"script.StackFrame" make
    |> Jsont.Object.mem "columnNumber" Js_uint.jsont ~enc:column_number
    |> Jsont.Object.mem "functionName" Jsont.string ~enc:function_name
    |> Jsont.Object.mem "lineNumber" Js_uint.jsont ~enc:line_number
    |> Jsont.Object.mem "url" Jsont.string ~enc:url
    |> Jsont.Object.finish
end

module Stack_trace = struct
  type t = { call_frames : Stack_frame.t list; }
  let call_frames t = t.call_frames
  let jsont =
    let make call_frames = { call_frames } in
    Jsont.Object.map ~kind:"script.StackTrace" make
    |> Jsont.Object.mem
      "callFrames" (Jsont.list Stack_frame.jsont) ~enc:call_frames
    |> Jsont.Object.finish
end

module Source = struct
  type t =
    { realm : Realm.t;
      context : Browsing_context.t option;
      user_context : User_context.t option }
  let realm s = s.realm
  let context s = s.context
  let user_context s = s.user_context
  let jsont =
    let make realm context user_context = { realm; context; user_context } in
    Jsont.Object.map ~kind:"script.Source" make
    |> Jsont.Object.mem "realm" Realm.jsont ~enc:realm
    |> Jsont.Object.opt_mem "context" Browsing_context.jsont ~enc:context
    |> Jsont.Object.opt_mem "userContext" User_context.jsont ~enc:user_context
    |> Jsont.Object.finish
end

module Target = struct
  type t =
  [ `Realm of Realm.t
  | `Context of Browsing_context.t * string option ]

  let jsont =
    let make realm context sandbox =
      match realm, context with
      | Some _, Some _ ->
          Jsont.Error.msgf Jsont.Meta.none
            "realm and context member cannot be present at the same time"
      | None, None ->
          Jsont.Error.msgf Jsont.Meta.none "missing a realm or context member"
      | Some realm, None -> `Realm realm
      | None, Some context -> `Context (context, sandbox)
    in
    let enc_realm = function `Realm r -> Some r | `Context _ -> None in
    let enc_context = function `Realm _ -> None | `Context (c, _) -> Some c in
    let enc_sandbox = function `Realm _ -> None | `Context (_, s) -> s in
    Jsont.Object.map ~kind:"scriptTarget" make
    |> Jsont.Object.opt_mem "realm" Realm.jsont ~enc:enc_realm
    |> Jsont.Object.opt_mem "context" Browsing_context.jsont ~enc:enc_context
    |> Jsont.Object.opt_mem "sandbox" Jsont.string ~enc:enc_sandbox
    |> Jsont.Object.finish
end

module Remote_reference = struct
  type t =
    { shared_id : Shared_id.t option;
      handle : Handle.t option }
  let make ?shared_id ?handle () = { shared_id; handle }
  let shared_id r = r.shared_id
  let handle r = r.handle
  let jsont =
    let make shared_id handle = { shared_id; handle } in
    Jsont.Object.map ~kind:"script.RemoteReference" make
    |> Jsont.Object.opt_mem "sharedId" Shared_id.jsont ~enc:shared_id
    |> Jsont.Object.opt_mem "handle" Handle.jsont ~enc:handle
    |> Jsont.Object.finish
end

module Undefined_value = struct
  type t = unit
  let jsont =
    Jsont.Object.map ~kind:"script.UndefinedValue" ()
    |> Jsont.Object.finish
end

module Null_value = struct
  type t = unit
  let jsont =
    Jsont.Object.map ~kind:"script.NullValue" ()
    |> Jsont.Object.finish
end

module String_value = struct
  type t = string
  let jsont =
    Jsont.Object.map ~kind:"script.StringValue" Fun.id
    |> Jsont.Object.mem "value" Jsont.string ~enc:Fun.id
    |> Jsont.Object.finish
end

module Number_value = struct
  type t = float
  let jsont =
    Jsont.Object.map ~kind:"script.NumberValue" Fun.id
    |> Jsont.Object.mem "value" Jsont.any_float ~enc:Fun.id
    |> Jsont.Object.finish
end

module Boolean_value = struct
  type t = bool
  let jsont =
    Jsont.Object.map ~kind:"script.BooleanValue" Fun.id
    |> Jsont.Object.mem "value" Jsont.bool ~enc:Fun.id
    |> Jsont.Object.finish
end

module Big_int_value = struct
  type t = string
  let jsont =
    Jsont.Object.map ~kind:"script.BigIntValue" Fun.id
    |> Jsont.Object.mem "value" Jsont.string ~enc:Fun.id
    |> Jsont.Object.finish
end

module Date_value = struct
  type t = string
  let jsont =
    Jsont.Object.map ~kind:"script.DateLocalValue" Fun.id
    |> Jsont.Object.mem "value" Jsont.string ~enc:Fun.id
    |> Jsont.Object.finish
end

module Regexp_value = struct
  type t = { pattern : string; flags : string option }
  let make ~pattern ?flags () = { pattern; flags }
  let pattern r = r.pattern
  let flags r = r.flags
  let jsont =
    let make pattern flags = { pattern; flags } in
    Jsont.Object.map ~kind:"script.RegExpValue" make
    |> Jsont.Object.mem "pattern" Jsont.string ~enc:pattern
    |> Jsont.Object.opt_mem "flags" Jsont.string ~enc:flags
    |> Jsont.Object.finish
end

module Regexp_local_value = struct
  type t = Regexp_value.t
  let jsont =
    Jsont.Object.map ~kind:"script.RegExpLocalValue" Fun.id
    |> Jsont.Object.mem "value" Regexp_value.jsont ~enc:Fun.id
    |> Jsont.Object.finish
end

let gen_binding_jsont t =
  let local_value =
    let enc = function `Key k -> k | `Key_string _ -> assert false in
    let dec k = `Key k in
    Jsont.map ~enc ~dec t
  in
  let string_key =
    let enc = function `Key_string k -> k | `Key _ -> assert false in
    let dec k = `Key_string k in
    Jsont.map ~enc ~dec Jsont.string
  in
  let el =
    let enc = function
    | `Key _ -> local_value
    | `Key_string _ -> string_key
    in
    Jsont.any ~dec_string:string_key ~dec_object:local_value ~enc ()
  in
  let t2 =
    Jsont.t2
      ~dec:(fun k v -> (k, v))
      ~enc:(fun b n -> if n = 0 then fst b else snd b)
      el
  in
  (* https://github.com/dbuenzli/jsont/issues/11 would be useful. *)
  let enc (k, v) = (k, `Key v) in
  let dec (k, v) = match v with
  | `Key v -> k, v
  | `Key_string _ ->
      Jsont.Error.msgf Jsont.Meta.none
        "Expected a script.LocalValue in second component but \
         found a string"
  in
  Jsont.map ~enc ~dec t2


module Local_value = struct
  type binding = [ `Key of t | `Key_string of string] * t
  and t =
  [ `Remote_reference of Remote_reference.t
  | `Undefined_value of Undefined_value.t
  | `Null_value of Null_value.t
  | `String_value of String_value.t
  | `Number_value of Number_value.t
  | `Boolean_value of Boolean_value.t
  | `Big_int_value of Big_int_value.t
  | `Channel_value of Channel_value.t
  | `Date_local_value of Date_value.t
  | `Regexp_local_value of Regexp_local_value.t
  | `Array_local_value of t list
  | `Map_local_value of binding list
  | `Object_local_value of binding list
  | `Set_local_value of t list ]

  let rec binding_jsont = lazy (gen_binding_jsont (Jsont.rec' lazy_jsont))
  and lazy_jsont = lazy begin
    let binding_jsont = Lazy.force binding_jsont in
    let array_local_jsont =
      Jsont.Object.map ~kind:"script.ArrayLocalValue" Fun.id
      |> Jsont.Object.mem "value" Jsont.(list (rec' lazy_jsont)) ~enc:Fun.id
      |> Jsont.Object.finish
    in
    let map_local_jsont =
      Jsont.Object.map ~kind:"script.MapLocalValue" Fun.id
      |> Jsont.Object.mem "value" (Jsont.list binding_jsont) ~enc:Fun.id
      |> Jsont.Object.finish
    in
    let object_local_jsont =
      Jsont.Object.map ~kind:"script.ObjectLocalValue" Fun.id
      |> Jsont.Object.mem "value" (Jsont.list binding_jsont) ~enc:Fun.id
      |> Jsont.Object.finish
    in
    let set_local_jsont =
      Jsont.Object.map ~kind:"script.SetLocalValue" Fun.id
      |> Jsont.Object.mem  "value" Jsont.(list (rec' lazy_jsont)) ~enc:Fun.id
      |> Jsont.Object.finish
    in
    (* Note formally the case "remotereference" does not exist
       we use it to decode Remote_reference which has no type case member. *)
    let enc_omit = function "remotereference" -> true | _ -> false in
    let remotereference =
      Jsont.Object.Case.map "remotereference" Remote_reference.jsont
        ~dec:(fun r -> `Remote_reference r)
    in
    let undefinedvalue =
      Jsont.Object.Case.map "undefined" Undefined_value.jsont
        ~dec:(fun v -> `Undefined_value v)
    in
    let nullvalue =
      Jsont.Object.Case.map "null" Null_value.jsont
        ~dec:(fun v -> `Null_value v)
    in
    let stringvalue =
      Jsont.Object.Case.map "string" String_value.jsont
        ~dec:(fun v -> `String_value v)
    in
    let numbervalue =
      Jsont.Object.Case.map "number" Number_value.jsont
        ~dec:(fun v -> `Number_value v)
    in
    let booleanvalue =
      Jsont.Object.Case.map "boolean" Boolean_value.jsont
        ~dec:(fun v -> `Boolean_value v)
    in
    let bigintvalue =
      Jsont.Object.Case.map "bigint" Big_int_value.jsont
        ~dec:(fun v -> `Big_int_value v)
    in
    let channelvalue =
      Jsont.Object.Case.map "channel" Channel_value.jsont
        ~dec:(fun v -> `Channel_value v)
    in
    let datelocalvalue =
      Jsont.Object.Case.map "date" Date_value.jsont
        ~dec:(fun v -> `Date_local_value v)
    in
    let regexplocalvalue =
      Jsont.Object.Case.map "regexp" Regexp_local_value.jsont
        ~dec:(fun v -> `Regexp_local_value v)
    in
    let arraylocalvalue =
      Jsont.Object.Case.map "array" array_local_jsont
        ~dec:(fun v -> `Array_local_value v)
    in
    let maplocalvalue =
      Jsont.Object.Case.map "map" map_local_jsont
        ~dec:(fun v -> `Map_local_value v)
    in
    let objectlocalvalue =
      Jsont.Object.Case.map "object" object_local_jsont
        ~dec:(fun v -> `Object_local_value v)
    in
    let setlocalvalue =
      Jsont.Object.Case.map "set" set_local_jsont
        ~dec:(fun v -> `Set_local_value v)
    in
    let enc_case = function
    | `Remote_reference r -> Jsont.Object.Case.value remotereference r
    | `Undefined_value u -> Jsont.Object.Case.value undefinedvalue u
    | `Null_value n -> Jsont.Object.Case.value nullvalue n
    | `String_value s -> Jsont.Object.Case.value stringvalue s
    | `Number_value n -> Jsont.Object.Case.value numbervalue n
    | `Boolean_value b -> Jsont.Object.Case.value booleanvalue b
    | `Big_int_value b -> Jsont.Object.Case.value bigintvalue b
    | `Channel_value c -> Jsont.Object.Case.value channelvalue c
    | `Date_local_value d -> Jsont.Object.Case.value datelocalvalue d
    | `Regexp_local_value r -> Jsont.Object.Case.value regexplocalvalue r
    | `Array_local_value a -> Jsont.Object.Case.value arraylocalvalue a
    | `Map_local_value m -> Jsont.Object.Case.value maplocalvalue m
    | `Object_local_value o -> Jsont.Object.Case.value objectlocalvalue o
    | `Set_local_value s -> Jsont.Object.Case.value setlocalvalue s
    in
    let cases = Jsont.Object.Case.[
        make remotereference; make undefinedvalue; make nullvalue;
        make stringvalue; make numbervalue; make booleanvalue;
        make bigintvalue; make channelvalue; make datelocalvalue;
        make arraylocalvalue; make maplocalvalue; make objectlocalvalue;
        make setlocalvalue;
      ]
    in
    Jsont.Object.map ~kind:"script.LocalValue" Fun.id
    |> Jsont.Object.case_mem
      "type" Jsont.string cases ~enc_case ~enc:Fun.id ~enc_omit
      ~dec_absent:"remotereference"
    |> Jsont.Object.finish
  end

  let jsont = Lazy.force lazy_jsont
  let binding_jsont = Lazy.force binding_jsont
end

module Base_remote_value = struct
  type 'a t =
    { handle : Handle.t option;
      internal_id : Internal_id.t option;
      other : 'a}

  let make handle internal_id other = { handle; internal_id; other }
  let handle r = r.handle
  let internal_id r = r.internal_id
  let other r = r.other
  let jsont_open ~kind make =
    Jsont.Object.map ~kind make
    |> Jsont.Object.opt_mem "handle" Handle.jsont ~enc:handle
    |> Jsont.Object.opt_mem "internalId" Internal_id.jsont ~enc:internal_id
end

module type KIND = sig val kind : string end
module type SIMPLE_REMOTE_VALUE = sig
  type t
  val make :
    handle:Handle.t option ->
    internal_id:Internal_id.t option -> unit -> t
  val handle : t -> Handle.t option
  val internal_id : t -> Internal_id.t option
  val jsont : t Jsont.t
end

module Simple_remote_value (K : KIND) = struct
  include Base_remote_value
  type nonrec t = unit t
  let make ~handle ~internal_id () = { handle; internal_id; other = () }
  let jsont =
    let make handle internal_id = { handle; internal_id; other = () } in
    jsont_open ~kind:K.kind make
    |> Jsont.Object.finish
end

module Symbol_remote_value =
  Simple_remote_value (struct let kind = "script.SymbolRemoteValue" end)

module Function_remote_value =
  Simple_remote_value (struct let kind = "script.FunctionRemoteValue" end)

module Weakmap_remote_value =
  Simple_remote_value (struct let kind = "script.WeakMapRemoteValue" end)

module Weakset_remote_value =
  Simple_remote_value (struct let kind = "script.WeakSetRemoteValue" end)

module Generator_remote_value =
  Simple_remote_value (struct let kind = "script.GeneratorRemoteValue" end)

module Error_remote_value =
  Simple_remote_value (struct let kind = "script.ErrorRemoteValue" end)

module Proxy_remote_value =
  Simple_remote_value (struct let kind = "script.ProxyRemoteValue" end)

module Promise_remote_value =
  Simple_remote_value (struct let kind = "script.PromiseRemoteValue" end)

module Typed_array_remote_value =
  Simple_remote_value (struct let kind = "script.TypedArrayRemoteValue" end)

module Array_buffer_remote_value =
  Simple_remote_value (struct let kind = "script.ArrayBufferRemoteValue" end)

module Regexp_remote_value = struct
  include Base_remote_value
  type nonrec t = Regexp_value.t t
  let make ~handle ~internal_id ~value () =
    { handle; internal_id; other = value }
  let value r = r.other
  let jsont =
    jsont_open ~kind:"script.RegExpRemoteValue" Base_remote_value.make
    |> Jsont.Object.mem "value" Regexp_value.jsont ~enc:value
    |> Jsont.Object.finish
end

module Date_remote_value = struct
  include Base_remote_value
  type nonrec t = string t
  let make ~handle ~internal_id ~value () =
    { handle; internal_id; other = value }
  let value d = d.other
  let jsont =
    jsont_open ~kind:"script.DateRemoteValue" Base_remote_value.make
    |> Jsont.Object.mem "value" Jsont.string ~enc:value
    |> Jsont.Object.finish
end

module Node_properties = struct
  type node_remote_value =
  { shared_id : Shared_id.t option; handle : Handle.t option;
    internal_id : Internal_id.t option ;
    value : t option }

  and t =
  { node_type : int;
    child_node_count : int;
    attributes : string Map.Make(String).t option;
    children : node_remote_value list option;
    local_name : string option;
    mode : [`Open | `Closed ] option;
    namespace_uri : string option;
    node_value : string option;
    shadow_root : node_remote_value option option }

  let make
      ~node_type ~child_node_count ?attributes ?children ?local_name
      ?mode ?namespace_uri ?node_value ?shadow_root ()
    =
    { node_type; child_node_count; attributes; children; local_name;
      mode; namespace_uri; node_value; shadow_root }

  let node_type p = p.node_type
  let child_node_count p = p.child_node_count
  let attributes p = p.attributes
  let children p = p.children
  let local_name p = p.local_name
  let mode p = p.mode
  let namespace_uri p = p.namespace_uri
  let node_value p = p.node_value
  let shadow_root p = p.shadow_root

  let attributes_jsont = Jsont.Object.as_string_map Jsont.string
  let mode_jsont =
    Jsont.enum ~kind:"scriptNodePropertyMode" (* Not in the spec *)
      ["open", `Open; "closed", `Closed]

  let node_remote_value_open_jsont make props_jsont =
    let shared_id n = n.shared_id and handle n = n.handle
    and internal_id n = n.internal_id and value n = n.value in
    Jsont.Object.map ~kind:"script.NodeRemoteValue" make
    |> Jsont.Object.opt_mem "sharedId" Shared_id.jsont ~enc:shared_id
    |> Jsont.Object.opt_mem "handle" Handle.jsont ~enc:handle
    |> Jsont.Object.opt_mem "internalId" Internal_id.jsont ~enc:internal_id
    |> Jsont.Object.opt_mem "value" props_jsont ~enc:value

  let rec props_jsont = lazy begin
    let node_remote_value_jsont =
      Lazy.force node_remote_value_standalone_jsont
    in
    let make
        node_type child_node_count attributes children local_name
        mode namespace_uri node_value shadow_root
      =
      { node_type; child_node_count; attributes; children; local_name;
        mode; namespace_uri; node_value; shadow_root}
    in
    Jsont.Object.map ~kind:"script.NodeProperties" make
    |> Jsont.Object.mem "nodeType" Js_uint.jsont ~enc:node_type
    |> Jsont.Object.mem "childNodeCount" Js_uint.jsont ~enc:child_node_count
    |> Jsont.Object.opt_mem "attributes" attributes_jsont ~enc:attributes
    |> Jsont.Object.opt_mem
      "children" (Jsont.list node_remote_value_jsont) ~enc:children
    |> Jsont.Object.opt_mem "localName" Jsont.string ~enc:local_name
    |> Jsont.Object.opt_mem "mode" mode_jsont ~enc:mode
    |> Jsont.Object.opt_mem "namespaceURI" Jsont.string ~enc:namespace_uri
    |> Jsont.Object.opt_mem "nodeValue" Jsont.string ~enc:node_value
    |> Jsont.Object.opt_mem
      "shadowRoot" (Jsont.option node_remote_value_jsont)
      ~enc:shadow_root
    |> Jsont.Object.finish
  end
  (* The annoyance here is due to NodeRemoteValue being used both as a case
     class and a standalone type which Jsont doesn't support well. *)
  and node_remote_value_standalone_jsont = lazy begin
    let make shared_id handle internal_id value type' =
      if type' = "node" then { shared_id; handle; internal_id; value } else
      Jsont.Error.msgf Jsont.Meta.none
        "Expected type member value \"node\" but found %S" type'
    in
    node_remote_value_open_jsont make (Jsont.rec' props_jsont)
    |> Jsont.Object.mem "type" Jsont.string ~enc:(fun _ -> "node")
    |> Jsont.Object.finish
  end
  and node_remote_value_jsont = lazy begin
    let make shared_id handle internal_id value =
      { shared_id; handle; internal_id; value }
    in
    node_remote_value_open_jsont make (Jsont.rec' props_jsont)
    |> Jsont.Object.finish
  end

  let jsont = Lazy.force props_jsont
end

module Node_remote_value = struct
  type t = Node_properties.node_remote_value
  let make ?shared_id ?handle ?internal_id ?value () =
    Node_properties.{ shared_id; handle; internal_id; value }
  let shared_id (n : t) = n.shared_id
  let handle (n : t) = n.handle
  let internal_id (n : t) = n.internal_id
  let value (n : t) = n.value
  let jsont_standalone = Lazy.force Node_properties.node_remote_value_jsont
  let jsont = Lazy.force Node_properties.node_remote_value_jsont
end

module Window_proxy_properties = struct
  type t = { context : Browsing_context.t }
  let make ~context () = { context }
  let context p = p.context
  let jsont =
    let make context = { context } in
    Jsont.Object.map ~kind:"WindowProxyProperties" make
    |> Jsont.Object.mem "context" Browsing_context.jsont ~enc:context
    |> Jsont.Object.finish
end

module Window_proxy_remote_value = struct
  include Base_remote_value
  type nonrec t = Window_proxy_properties.t t
  let make ~handle ~internal_id ~value () =
    { handle; internal_id; other = value }
  let value d = d.other
  let jsont =
    jsont_open ~kind:"script.DateRemoteValue" Base_remote_value.make
    |> Jsont.Object.mem "value" Window_proxy_properties.jsont ~enc:value
    |> Jsont.Object.finish
end

module Remote_value = struct
  type binding = [ `Key of t | `Key_string of string] * t
  and array_remote_value = t list option Base_remote_value.t
  and object_remote_value = binding list option Base_remote_value.t
  and map_remote_value = binding list option Base_remote_value.t
  and set_remote_value = t list option Base_remote_value.t
  and node_list_remote_value = t list option Base_remote_value.t
  and html_collection_remote_value = t list option Base_remote_value.t
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
  | `Window_proxy_remote_value of Window_proxy_remote_value.t
  ]

  let list_remote_value_jsont ~kind t =
    Base_remote_value.(jsont_open ~kind make)
    |> Jsont.Object.opt_mem "value" Jsont.(list t)
      ~enc:Base_remote_value.other
    |> Jsont.Object.finish

  let binding_list_remote_value_jsont ~kind t =
    Base_remote_value.(jsont_open ~kind make)
    |> Jsont.Object.opt_mem "value" Jsont.(list t)
      ~enc:Base_remote_value.other
    |> Jsont.Object.finish

  let rec array_remote_value_jsont =
    lazy (list_remote_value_jsont ~kind:"script.ArrayRemoteValue"
            (Jsont.rec' lazy_jsont))
  and object_remote_value_jsont =
    lazy (binding_list_remote_value_jsont ~kind:"script.ObjectRemoteValue"
            (Jsont.rec' binding_jsont))
  and map_remote_value_jsont =
    lazy (binding_list_remote_value_jsont ~kind:"script.MapRemoteValue"
            (Jsont.rec' binding_jsont))
  and set_remote_value_jsont =
    lazy (list_remote_value_jsont ~kind:"script.SetRemoteValue"
            (Jsont.rec' lazy_jsont))
  and node_list_remote_value_jsont =
    lazy (list_remote_value_jsont ~kind:"script.NodeListRemoteValue"
            (Jsont.rec' lazy_jsont))
  and html_collection_remote_value_jsont =
    lazy (list_remote_value_jsont ~kind:"script.HTMLCollectionRemoteValue"
            (Jsont.rec' lazy_jsont))
  and binding_jsont = lazy (gen_binding_jsont (Lazy.force lazy_jsont))
  and lazy_jsont : t Jsont.t Lazy.t = lazy begin
    let undefinedvalue =
      Jsont.Object.Case.map "undefined" Undefined_value.jsont
        ~dec:(fun v -> `Undefined_value v)
    in
    let nullvalue =
      Jsont.Object.Case.map "null" Null_value.jsont
        ~dec:(fun v -> `Null_value v)
    in
    let stringvalue =
      Jsont.Object.Case.map "string" String_value.jsont
        ~dec:(fun v -> `String_value v)
    in
    let numbervalue =
      Jsont.Object.Case.map "number" Number_value.jsont
        ~dec:(fun v -> `Number_value v)
    in
    let booleanvalue =
      Jsont.Object.Case.map "boolean" Boolean_value.jsont
        ~dec:(fun v -> `Boolean_value v)
    in
    let bigintvalue =
      Jsont.Object.Case.map "bigint" Big_int_value.jsont
        ~dec:(fun v -> `Big_int_value v)
    in
    let symbolremotevalue =
      Jsont.Object.Case.map "symbol" Symbol_remote_value.jsont
        ~dec:(fun v -> `Symbol_remote_value v)
    in
    let arrayremotevalue =
      Jsont.Object.Case.map "array" (Lazy.force array_remote_value_jsont)
        ~dec:(fun v -> `Array_remote_value v)
    in
    let objectremotevalue =
      Jsont.Object.Case.map "object" (Lazy.force object_remote_value_jsont)
        ~dec:(fun v -> `Object_remote_value v)
    in
    let functionremotevalue =
      Jsont.Object.Case.map "function" Function_remote_value.jsont
        ~dec:(fun v -> `Function_remote_value v)
    in
    let regexpremotevalue =
      Jsont.Object.Case.map "regexp" Regexp_remote_value.jsont
        ~dec:(fun v -> `Regexp_remote_value v)
    in
    let dateremotevalue =
      Jsont.Object.Case.map "date" Date_remote_value.jsont
        ~dec:(fun v -> `Date_remote_value v)
    in
    let mapremotevalue =
      Jsont.Object.Case.map "map" (Lazy.force map_remote_value_jsont)
        ~dec:(fun v -> `Map_remote_value v)
    in
    let setremotevalue =
      Jsont.Object.Case.map "set" (Lazy.force set_remote_value_jsont)
        ~dec:(fun v -> `Set_remote_value v)
    in
    let weakmapremotevalue =
      Jsont.Object.Case.map "weakmap" Weakmap_remote_value.jsont
        ~dec:(fun v -> `Weakmap_remote_value v)
    in
    let weaksetremotevalue =
      Jsont.Object.Case.map "weakset" Weakset_remote_value.jsont
        ~dec:(fun v -> `Weakset_remote_value v)
    in
    let generatorremotevalue =
      Jsont.Object.Case.map "generator" Generator_remote_value.jsont
        ~dec:(fun v -> `Generator_remote_value v)
    in
    let errorremotevalue =
      Jsont.Object.Case.map "error" Error_remote_value.jsont
        ~dec:(fun v -> `Error_remote_value v)
    in
    let proxyremotevalue =
      Jsont.Object.Case.map "proxy" Proxy_remote_value.jsont
        ~dec:(fun v -> `Proxy_remote_value v)
    in
    let promiseremotevalue =
      Jsont.Object.Case.map "promise" Promise_remote_value.jsont
        ~dec:(fun v -> `Promise_remote_value v)
    in
    let typedarrayremotevalue =
      Jsont.Object.Case.map "typedarray" Typed_array_remote_value.jsont
        ~dec:(fun v -> `Typed_array_remote_value v)
    in
    let arraybufferremotevalue =
      Jsont.Object.Case.map "arraybuffer" Array_buffer_remote_value.jsont
        ~dec:(fun v -> `Array_buffer_remote_value v)
    in
    let nodelistremotevalue =
      Jsont.Object.Case.map "nodelist" (Lazy.force node_list_remote_value_jsont)
        ~dec:(fun v -> `Node_list_remote_value v)
    in
    let htmlcollectionremotevalue =
      Jsont.Object.Case.map "htmlcollection"
        (Lazy.force html_collection_remote_value_jsont)
        ~dec:(fun v -> `Html_collection_remote_value v)
    in
    let noderemotevalue =
      Jsont.Object.Case.map "node" Node_remote_value.jsont
        ~dec:(fun v -> `Node_remote_value v)
    in
    let windowproxyremotevalue =
      Jsont.Object.Case.map "window" Window_proxy_remote_value.jsont
        ~dec:(fun v -> `Window_proxy_remote_value v)
    in
    let enc_case = function
    | `Undefined_value u -> Jsont.Object.Case.value undefinedvalue u
    | `Null_value n -> Jsont.Object.Case.value nullvalue n
    | `String_value s -> Jsont.Object.Case.value stringvalue s
    | `Number_value n -> Jsont.Object.Case.value numbervalue n
    | `Boolean_value b -> Jsont.Object.Case.value booleanvalue b
    | `Big_int_value b -> Jsont.Object.Case.value bigintvalue b
    | `Symbol_remote_value s -> Jsont.Object.Case.value symbolremotevalue s
    | `Array_remote_value a -> Jsont.Object.Case.value arrayremotevalue a
    | `Object_remote_value o -> Jsont.Object.Case.value objectremotevalue o
    | `Function_remote_value f -> Jsont.Object.Case.value functionremotevalue f
    | `Regexp_remote_value r -> Jsont.Object.Case.value regexpremotevalue r
    | `Date_remote_value r -> Jsont.Object.Case.value dateremotevalue r
    | `Map_remote_value m -> Jsont.Object.Case.value mapremotevalue m
    | `Set_remote_value s -> Jsont.Object.Case.value setremotevalue s
    | `Weakmap_remote_value w -> Jsont.Object.Case.value weakmapremotevalue w
    | `Weakset_remote_value w -> Jsont.Object.Case.value weaksetremotevalue w
    | `Generator_remote_value g ->
        Jsont.Object.Case.value generatorremotevalue g
    | `Error_remote_value e -> Jsont.Object.Case.value errorremotevalue e
    | `Proxy_remote_value p -> Jsont.Object.Case.value proxyremotevalue p
    | `Promise_remote_value p -> Jsont.Object.Case.value promiseremotevalue p
    | `Typed_array_remote_value a ->
        Jsont.Object.Case.value typedarrayremotevalue a
    | `Array_buffer_remote_value a ->
        Jsont.Object.Case.value arraybufferremotevalue a
    | `Node_list_remote_value l ->
        Jsont.Object.Case.value nodelistremotevalue l
    | `Html_collection_remote_value l ->
        Jsont.Object.Case.value htmlcollectionremotevalue l
    | `Node_remote_value n ->
        Jsont.Object.Case.value noderemotevalue n
    | `Window_proxy_remote_value n ->
        Jsont.Object.Case.value windowproxyremotevalue n
    in
    let cases = Jsont.Object.Case.[
        make undefinedvalue; make nullvalue;
        make stringvalue; make numbervalue; make booleanvalue;
        make bigintvalue; make symbolremotevalue; make arrayremotevalue;
        make objectremotevalue; make functionremotevalue;
        make regexpremotevalue; make dateremotevalue; make mapremotevalue;
        make setremotevalue; make weakmapremotevalue; make weaksetremotevalue;
        make generatorremotevalue; make errorremotevalue;
        make proxyremotevalue; make promiseremotevalue;
        make typedarrayremotevalue; make arraybufferremotevalue;
        make nodelistremotevalue; make htmlcollectionremotevalue;
        make noderemotevalue; make windowproxyremotevalue
      ]
    in
    Jsont.Object.map ~kind:"script.RemoveValue" Fun.id
    |> Jsont.Object.case_mem
      "type" Jsont.string cases ~enc_case ~enc:Fun.id
    |> Jsont.Object.finish
  end
  let jsont = Lazy.force lazy_jsont
  let binding_jsont = Lazy.force binding_jsont
  let array_remote_value_jsont = Lazy.force array_remote_value_jsont
  let object_remote_value_jsont = Lazy.force object_remote_value_jsont
  let map_remote_value_jsont = Lazy.force map_remote_value_jsont
  let set_remote_value_jsont = Lazy.force set_remote_value_jsont
  let node_list_remote_value_jsont = Lazy.force node_list_remote_value_jsont
  let html_collection_remote_value_jsont =
    Lazy.force html_collection_remote_value_jsont
end

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

module Array_remote_value = struct
  include Base_remote_value
  type t = Remote_value.array_remote_value
  let make ~handle ~internal_id ~value () =
    { handle; internal_id; other = value }
  let value a = a.other
  let jsont = Remote_value.array_remote_value_jsont
end

module Object_remote_value = struct
  type binding = Remote_value.binding
  include Base_remote_value
  type t = Remote_value.object_remote_value
  let make ~handle ~internal_id ~value () =
    { handle; internal_id; other = value }
  let value a = a.other
  let jsont = Remote_value.object_remote_value_jsont
end

module Map_remote_value = struct
  type binding = Remote_value.binding
  include Base_remote_value
  type t = Remote_value.object_remote_value
  let make ~handle ~internal_id ~value () =
    { handle; internal_id; other = value }
  let value a = a.other
  let jsont = Remote_value.map_remote_value_jsont
end

module Set_remote_value = struct
  include Base_remote_value
  type t = Remote_value.set_remote_value
  let make ~handle ~internal_id ~value () =
    { handle; internal_id; other = value }
  let value a = a.other
  let jsont = Remote_value.set_remote_value_jsont
end

module Node_list_remote_value = struct
  include Base_remote_value
  type t = Remote_value.node_list_remote_value
  let make ~handle ~internal_id ~value () =
    { handle; internal_id; other = value }
  let value a = a.other
  let jsont = Remote_value.node_list_remote_value_jsont
end

module Html_collection_remote_value = struct
  include Base_remote_value
  type t = Remote_value.html_collection_remote_value
  let make ~handle ~internal_id ~value () =
    { handle; internal_id; other = value }
  let value a = a.other
  let jsont = Remote_value.html_collection_remote_value_jsont
end

module Exception_details = struct
  type t =
    { column_number : int;
      exception' : Remote_value.t;
      line_number : int;
      stack_trace : Stack_trace.t;
      text : string }
  let column_number e = e.column_number
  let exception' e = e.exception'
  let line_number e = e.line_number
  let stack_trace e = e.stack_trace
  let text e = e.text
  let jsont =
    let make column_number exception' line_number stack_trace text =
      { column_number; exception'; line_number; stack_trace; text; }
    in
    Jsont.Object.map ~kind:"script.ExceptionDetails" make
    |> Jsont.Object.mem "columnNumber" Js_uint.jsont ~enc:column_number
    |> Jsont.Object.mem "exception" Remote_value.jsont ~enc:exception'
    |> Jsont.Object.mem "lineNumber" Js_uint.jsont ~enc:line_number
    |> Jsont.Object.mem "stackTrace" Stack_trace.jsont ~enc:stack_trace
    |> Jsont.Object.mem "text" Jsont.string ~enc:text
    |> Jsont.Object.finish
end

module Evaluate_result = struct
  module Success = struct
    type t =
      { result : Remote_value.t;
        realm : Realm.t }

    let result s = s.result
    let realm s = s.realm
    let jsont =
      let make result realm = { result; realm } in
      Jsont.Object.map ~kind:"script.EvaluateResultSuccess" make
      |> Jsont.Object.mem "result" Remote_value.jsont ~enc:result
      |> Jsont.Object.mem "realm" Realm.jsont ~enc:realm
      |> Jsont.Object.finish
  end

  module Exception = struct
    type t =
      { exception_details : Exception_details.t;
        realm : Realm.t }
    let exception_details s = s.exception_details
    let realm s = s.realm
    let jsont =
      let make exception_details realm = { exception_details; realm } in
      Jsont.Object.map ~kind:"script.EvaluateResultException" make
      |> Jsont.Object.mem
        "exceptionDetails" Exception_details.jsont ~enc:exception_details
      |> Jsont.Object.mem "realm" Realm.jsont ~enc:realm
      |> Jsont.Object.finish
  end

  type t =
  [ `Success of Success.t
  | `Exception of Exception.t ]

  let jsont =
    let success =
      Jsont.Object.Case.map "success" Success.jsont ~dec:(fun s -> `Success s)
    in
    let exception' =
      Jsont.Object.Case.map "exception" Exception.jsont
        ~dec:(fun e -> `Exception e)
    in
    let enc_case = function
    | `Success s -> Jsont.Object.Case.value success s
    | `Exception e -> Jsont.Object.Case.value exception' e
    in
    let cases = Jsont.Object.Case.[make success; make exception'] in
    Jsont.Object.map ~kind:"script.EvaluateResult" Fun.id
    |> Jsont.Object.case_mem "type"
      Jsont.string cases ~enc_case ~enc:Fun.id ~tag_to_string:Fun.id
    |> Jsont.Object.finish
end

module Message_parameters = struct
  type t =
    { channel : Channel.t;
      data : Remote_value.t;
      source : Source.t; }
  let channel m = m.channel
  let data m = m.data
  let source m = m.source
  let jsont =
    let make channel data source = { channel; data; source } in
    Jsont.Object.map ~kind:"script.MessageParameters" make
    |> Jsont.Object.mem "channel" Channel.jsont ~enc:channel
    |> Jsont.Object.mem "data" Remote_value.jsont ~enc:data
    |> Jsont.Object.mem "source" Source.jsont ~enc:source
    |> Jsont.Object.finish
end

module Add_preload_script_result = struct
  type t = { script : Preload_script.t }
  let preload_script r = r.script
  let jsont =
    let make script = { script } in
    Jsont.Object.map ~kind:"script.AddPreloadScriptResult" make
    |> Jsont.Object.mem "script" Preload_script.jsont ~enc:preload_script
    |> Jsont.Object.finish
end

module Get_realms_result = struct
  type t = { realms : Realm.t list }
  let realms p = p.realms
  let jsont =
    let make realms = { realms } in
    Jsont.Object.map ~kind:"script.GetRealmsResult" make
    |> Jsont.Object.mem "realms" (Jsont.list Realm.jsont) ~enc:realms
    |> Jsont.Object.finish
end

module Realm_destroyed_parameters = struct
  type t = { realm : Realm.t }
  let realm p = p.realm
  let jsont =
    let make realm = { realm } in
    Jsont.Object.map ~kind:"script.RealmDestroyedParameters" make
    |> Jsont.Object.mem "realm" Realm.jsont ~enc:realm
    |> Jsont.Object.finish
end

(* [script.addPreloadScript] *)

type add_preload_script_params =
  { function_declaration : string;
    arguments : Channel_value.t list option;
    contexts : Browsing_context.t list option;
    user_contexts : User_context.t list option;
    sandbox : string option }

let add_preload_script_params_jsont =
  let make
      function_declaration arguments contexts user_contexts sandbox
    =
    { function_declaration; arguments; contexts; user_contexts; sandbox }
  in
  let function_declaration p = p.function_declaration
  and arguments p = p.arguments and contexts p = p.contexts
  and user_contexts p = p.user_contexts and sandbox p = p.sandbox in
  Jsont.Object.map ~kind:"script.AddPreloadScriptParameters" make
  |> Jsont.Object.mem
    "functionDeclaration" Jsont.string ~enc:function_declaration
  |> Jsont.Object.opt_mem
    "arguments" (Jsont.list Channel_value.jsont) ~enc:arguments
  |> Jsont.Object.opt_mem
    "contexts" (Jsont.list Browsing_context.jsont) ~enc:contexts
  |> Jsont.Object.opt_mem
    "userContexts" (Jsont.list User_context.jsont) ~enc:user_contexts
  |> Jsont.Object.opt_mem
    "sandbox" Jsont.string ~enc:sandbox
  |> Jsont.Object.finish

let add_preload_script_command =
  let params_jsont = add_preload_script_params_jsont in
  let result_jsont = Add_preload_script_result.jsont in
  Command.define "script.addPreloadScript" ~params_jsont ~result_jsont

let add_preload_script
    c ?exts ~function_declaration ?arguments ?contexts ?user_contexts
    ?sandbox ()
  =
  let params =
    { function_declaration; arguments; contexts; user_contexts; sandbox }
  in
  call c ?exts add_preload_script_command params

(* [script.disown] *)

type disown_params =
  { handles : Handle.t list;
    target : Target.t }

let disown_params_jsont =
  let make handles target = { handles; target } in
  let handles p = p.handles and target p = p.target in
  Jsont.Object.map ~kind:"script.DisownParameters" make
  |> Jsont.Object.mem "handles" (Jsont.list Handle.jsont) ~enc:handles
  |> Jsont.Object.mem "target" Target.jsont ~enc:target
  |> Jsont.Object.finish

let disown_command =
  let params_jsont = disown_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "script.disown" ~params_jsont ~result_jsont

let disown c ?exts ~handles ~target () =
  call c ?exts disown_command { handles; target }

(* [script.callFunction] *)

type call_function_params =
  { function_declaration : string;
    await_promise : bool;
    target : Target.t;
    arguments : Local_value.t list option;
    result_ownership : Result_ownership.t option;
    serialization_options : Serialization_options.t option;
    this : Local_value.t option;
    user_activation : bool option }

let call_function_params_jsont =
  let make
      function_declaration await_promise target arguments result_ownership
      serialization_options this user_activation
    =
    { function_declaration; await_promise; target; arguments; result_ownership;
      serialization_options; this; user_activation; }
  in
  let function_declaration p = p.function_declaration
  and await_promise p = p.await_promise
  and target p = p.target and arguments p = p.arguments
  and result_ownership p = p.result_ownership
  and serialization_options p = p.serialization_options
  and this p = p.this and user_activation p = p.user_activation in
  Jsont.Object.map ~kind:"CallFunctionParameters" make
  |> Jsont.Object.mem
    "functionDeclaration" Jsont.string ~enc:function_declaration
  |> Jsont.Object.mem "awaitPromise" Jsont.bool ~enc:await_promise
  |> Jsont.Object.mem "target" Target.jsont ~enc:target
  |> Jsont.Object.opt_mem
    "arguments" (Jsont.list Local_value.jsont) ~enc:arguments
  |> Jsont.Object.opt_mem
    "resultOwnership" Result_ownership.jsont ~enc:result_ownership
  |> Jsont.Object.opt_mem
    "serializationOptions" Serialization_options.jsont
    ~enc:serialization_options
  |> Jsont.Object.opt_mem
    "this" Local_value.jsont ~enc:this
  |> Jsont.Object.opt_mem
    "userActivation" Jsont.bool ~enc:user_activation
  |> Jsont.Object.finish

let call_function_command =
  let params_jsont = call_function_params_jsont in
  let result_jsont = Evaluate_result.jsont in
  Command.define "script.callFunction" ~params_jsont ~result_jsont

let call_function
    c ?exts ~function_declaration ~await_promise ~target ?arguments
    ?result_ownership ?serialization_options ?this ?user_activation () =
  let params =
    { function_declaration; await_promise; target; arguments; result_ownership;
      serialization_options; this; user_activation; }
  in
  call c ?exts call_function_command params

(* [script.evaluate] *)

type evaluate_params =
  { expression : string;
    target : Target.t;
    await_promise : bool;
    result_ownership : Result_ownership.t option;
    serialization_options : Serialization_options.t option;
    user_activation : bool option }

let evaluate_params_jsont =
  let make
      expression target await_promise result_ownership serialization_options
      user_activation
    =
    { expression; target; await_promise; result_ownership;
      serialization_options; user_activation; }
  in
  let expression p = p.expression and target p = p.target
  and await_promise p = p.await_promise
  and result_ownership p = p.result_ownership
  and serialization_options p = p.serialization_options
  and user_activation p = p.user_activation in
  Jsont.Object.map ~kind:"EvaluateParameters" make
  |> Jsont.Object.mem "expression" Jsont.string ~enc:expression
  |> Jsont.Object.mem "target" Target.jsont ~enc:target
  |> Jsont.Object.mem "awaitPromise" Jsont.bool ~enc:await_promise
  |> Jsont.Object.opt_mem
    "resultOwnership" Result_ownership.jsont ~enc:result_ownership
  |> Jsont.Object.opt_mem
    "serializationOptions" Serialization_options.jsont
    ~enc:serialization_options
  |> Jsont.Object.opt_mem
    "userActivation" Jsont.bool ~enc:user_activation
  |> Jsont.Object.finish

let evaluate_command =
  let params_jsont = evaluate_params_jsont in
  let result_jsont = Evaluate_result.jsont in
  Command.define "script.evaluate" ~params_jsont ~result_jsont

let evaluate
    c ?exts ~expression ~target ~await_promise ?result_ownership
    ?serialization_options ?user_activation () =
  let params =
    { expression; target; await_promise; result_ownership;
      serialization_options; user_activation; }
  in
  call c ?exts evaluate_command params

(* [script.getRealms] *)

type get_realms_params =
  { context : Browsing_context.t option;
    type' : Realm_type.t option }

let get_realms_params_jsont =
  let make context type' = { context; type' } in
  let context p = p.context and type' p = p.type' in
  Jsont.Object.map ~kind:"GetRealmsParameters" make
  |> Jsont.Object.opt_mem "context" Browsing_context.jsont ~enc:context
  |> Jsont.Object.opt_mem "type" Realm_type.jsont ~enc:type'
  |> Jsont.Object.finish

let get_realms_command =
  let params_jsont = get_realms_params_jsont in
  let result_jsont = Get_realms_result.jsont in
  Command.define "script.getRealms" ~params_jsont ~result_jsont

let get_realms c ?exts ?context ?type' () =
  call c ?exts get_realms_command { context; type' }

(* [script.removePreloadScript] *)

type remove_preload_script_params =
  { script : Preload_script.t; }

let remove_preload_script_params_jsont =
  let make script = { script } in
  let script p = p.script in
  Jsont.Object.map ~kind:"RemovePreloadScriptParameters" make
  |> Jsont.Object.mem "script" Preload_script.jsont ~enc:script
  |> Jsont.Object.finish

let remove_preload_script_command =
  let params_jsont = remove_preload_script_params_jsont in
  let result_jsont = Empty_result.jsont in
  Command.define "script.removePreloadScript" ~params_jsont ~result_jsont

let remove_preload_script c ?exts ~script () =
  call c ?exts remove_preload_script_command { script }

(* Events *)

let message =
  let params_jsont = Message_parameters.jsont in
  Event.define "script.message" ~params_jsont

let realm_created =
  let params_jsont = Realm_info.jsont in
  Event.define "script.realmCreated" ~params_jsont

let realm_destroyed =
  let params_jsont = Realm_destroyed_parameters.jsont in
  Event.define "script.realmDestroyed" ~params_jsont
