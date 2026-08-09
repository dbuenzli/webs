(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module String_map = Map.Make (Stdlib.String)

let int_min = float Int.min_int
let int_max = float Int.max_int

let err_not_an_int meta v =
  Jsont.Error.msgf meta "value %g not an integer" v

let err_not_in_ocaml_range meta v =
  Jsont.Error.msgf meta "integer %g not in OCaml int range [%d;%d]"
    v Int.min_int Int.max_int

let err_not_in_range meta v ~min ~max =
  Jsont.Error.msgf meta "integer %g not in range [%.0f;%.0f]" v min max

module Js_int = struct
  (* https://www.w3.org/TR/webdriver-bidi/#cddl-type-js-int *)
  type t = int
  let min = -9007199254740991.
  let max =  9007199254740991.
  let jsont =
    let dec m v =
      if not (Float.is_integer v) then err_not_an_int m v else
      if not (min <= v && v <= max) then err_not_in_range m v ~min ~max else
      if not (int_min <= v && v <= int_max) then err_not_in_ocaml_range m v else
      Int.of_float v
    in
    let enc i =
      let v = Int.to_float i in
      if min <= v && v <= max then v else
      err_not_in_range Jsont.Meta.none v ~min ~max
    in
    Jsont.Base.number (Jsont.Base.map ~kind:"js-uint" ~dec ~enc ())
end

module Js_uint = struct
  (* https://www.w3.org/TR/webdriver-bidi/#cddl-type-js-uint *)
  type t = int
  let min = 0.
  let max = 9007199254740991.
  let jsont =
    let dec m v =
      if not (Float.is_integer v) then err_not_an_int m v else
      if not (min <= v && v <= max) then err_not_in_range m v ~min ~max else
      if not (int_min <= v && v <= int_max) then err_not_in_ocaml_range m v else
      Int.of_float v
    in
    let enc i =
      let v = Int.to_float i in
      if min <= v && v <= max then v else
      err_not_in_range Jsont.Meta.none v ~min ~max
    in
    Jsont.Base.number (Jsont.Base.map ~kind:"js-uint" ~dec ~enc ())
end

module Exts = struct
  (* https://www.w3.org/TR/webdriver-bidi/#cddl-type-extensible *)
  type t = Jsont.json String_map.t
  let none = String_map.empty
  let jsont = Jsont.Object.Mems.string_map Jsont.json
end

module Empty_params = struct
  (* https://www.w3.org/TR/webdriver-bidi/#cddl-type-emptyparams *)
  (* The exts here is fishy, we do not surface it at the API level
     for now and simply use [empty]. Let's see what comes out of
     https://github.com/w3c/webdriver-bidi/issues/1108 *)
  type t = { exts : Exts.t }
  let make ?(exts = Exts.none) () = { exts }
  let empty = make ()
  let exts v = v.exts
  let jsont =
    Jsont.Object.map ~kind:"EmptyParams" (fun exts -> { exts })
    |> Jsont.Object.keep_unknown Exts.jsont ~enc:(fun e -> e.exts)
    |> Jsont.Object.finish
end

module Empty_result = struct
  (* https://www.w3.org/TR/webdriver-bidi/#cddl-type-emptyresult *)
  type t = { exts : Exts.t }
  let make ?(exts = Exts.none) () = { exts }
  let empty = make ()
  let exts v = v.exts
  let jsont =
    Jsont.Object.map ~kind:"EmptyResult" (fun exts -> { exts })
    |> Jsont.Object.keep_unknown Exts.jsont ~enc:(fun e -> e.exts)
    |> Jsont.Object.finish
end

module Command = struct
  type 'params instance =
    { id : int;
      method' : string;
      params : 'params;
      exts : Exts.t }

  let make ?(exts = Exts.none) id method' params = { id; method'; params; exts}
  let id c = c.id
  let method' c = c.method'
  let params c = c.params
  let exts c = c.exts

  type ('params, 'result) t =
    { name : string;
      encode_jsont : 'params Jsont.t;
      result_jsont : 'result Jsont.t; }

  let name command = command.name
  let encode_jsont command = command.encode_jsont
  let result_jsont command = command.result_jsont

  let define name ~params_jsont ~result_jsont =
    let encode_jsont =
      Jsont.Object.enc_only ~kind:"Command" ()
      |> Jsont.Object.mem "id" Js_uint.jsont ~enc:id
      |> Jsont.Object.mem "method" Jsont.string ~enc:method'
      |> Jsont.Object.mem "params" params_jsont ~enc:params
      |> Jsont.Object.keep_unknown Exts.jsont ~enc:exts
      |> Jsont.Object.finish
    in
    { name : string; encode_jsont; result_jsont }
end

module Command_response = struct
  (* Responses are not self-describing and their case depend on [id]. In
     Jsont we don't have dynamic case members (or a catch all case) so
     we parse [result] to a generic JSON representation we parse using
     the json type of the method definition which is stored alongside
     the the continuation of the command (see [kontinuator] type).

     We could improve this with https://github.com/dbuenzli/jsont/issues/15
     the jsont could take an atomic on an evolving id to type map.
     With a few additional [Type.Id] it should be workable (we did
     it via a terrible hack at some point). *)

  type t = { id : Js_uint.t; result : Jsont.json; exts : Exts.t; }
  let make ?(exts = Exts.none) ~id ~result () = { id; result; exts }
  let id r = r.id
  let result r = r.result
  let exts r = r.exts
  let jsont =
    let make id result exts = { id; result; exts } in
    Jsont.Object.map ~kind:"CommandResponse" make
    |> Jsont.Object.mem "id" Js_uint.jsont ~enc:id
    |> Jsont.Object.mem "result" Jsont.json ~enc:result
    |> Jsont.Object.keep_unknown Exts.jsont ~enc:exts
    |> Jsont.Object.finish
end

module Error_response = struct
  (* https://www.w3.org/TR/webdriver-bidi/#cddl-type-errorresponse *)
  type t =
    { id : Js_uint.t option;
      error : string;
      message : string;
      stacktrace : string option;
      exts : Exts.t }

  let make ?(exts = Exts.none) ?stacktrace ?id ~error ~message () =
    { id; error; message; stacktrace; exts }

  let make' id error message stacktrace exts =
    { id; error; message; stacktrace; exts }

  let id r = r.id
  let error r = r.error
  let message r = r.message
  let stacktrace r = r.stacktrace
  let exts r = r.exts
  let jsont =
    Jsont.Object.map ~kind:"ErrorResponse" make'
    |> Jsont.Object.mem "id" (Jsont.option Js_uint.jsont) ~enc:id
    |> Jsont.Object.mem "error" Jsont.string ~enc:error
    |> Jsont.Object.mem "message" Jsont.string ~enc:message
    |> Jsont.Object.opt_mem "stacktrace" Jsont.string ~enc:stacktrace
    |> Jsont.Object.keep_unknown Exts.jsont ~enc:exts
    |> Jsont.Object.finish
end

module Event = struct
  (* In contrast to [Command_response] we could have used a case member
     howver without a case catch all handler in Jsont this make our
     implementation brittle if the peer sends unknown (which may happen
     at some point since there's no protocol versioning). This
     could be improved once we get https://github.com/dbuenzli/jsont/issues/15
     in *)

  type 'a t = { name : string; params_jsont : 'a Jsont.t }
  let define name ~params_jsont = { name; params_jsont }
  let name e = e.name
  let params_jsont e = e.params_jsont

  type instance =
    { method' : string;
      params : Jsont.json;
      exts : Exts.t }

  let method' e = e.method'
  let params e = e.params
  let exts e = e.exts
  let jsont =
    let make method' params exts = { method'; params; exts } in
    Jsont.Object.map ~kind:"Event" make
    |> Jsont.Object.mem "method" Jsont.string ~enc:method'
    |> Jsont.Object.mem "params" Jsont.json ~enc:params
    |> Jsont.Object.keep_unknown Exts.jsont ~enc:exts
    |> Jsont.Object.finish
end

module Message = struct
  type t =
  | Command_response : Command_response.t -> t
  | Error_response : Error_response.t -> t
  | Event : Event.instance -> t

  let jsont =
    let command_response =
      let dec r = Command_response r in
      Jsont.Object.Case.map "success" Command_response.jsont ~dec
    in
    let error_response =
      let dec r = Error_response r in
      Jsont.Object.Case.map "error" Error_response.jsont ~dec
    in
    let event =
      let dec r = Event r in
      Jsont.Object.Case.map "event" Event.jsont ~dec
    in
    let enc_case = function
    | Command_response r -> Jsont.Object.Case.value command_response r
    | Error_response r -> Jsont.Object.Case.value error_response r
    | Event ev -> Jsont.Object.Case.value event ev
    in
    let cases = Jsont.Object.Case.[
        make error_response; make command_response; make event ]
    in
    Jsont.Object.map ~kind:"Message" Fun.id
    |> Jsont.Object.case_mem
      "type" Jsont.string cases ~enc:Fun.id ~enc_case ~tag_to_string:Fun.id
    |> Jsont.Object.finish
end

module User_context = struct
  (* https://www.w3.org/TR/webdriver-bidi/#cddl-type-browserusercontext *)
  type t = string
  let jsont = Jsont.with_doc ~kind:"browser.UserContext" Jsont.string
end

module Browsing_context = struct
  (* https://www.w3.org/TR/webdriver-bidi/\
     #cddl-type-browsingcontextbrowsingcontext *)
  type t = string
  let jsont =
    Jsont.with_doc ~kind:"browsingContext.BrowsingContext" Jsont.string
end
