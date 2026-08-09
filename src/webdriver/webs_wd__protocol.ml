(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Result.Syntax
open More
open Bytesrw
open Webs_wd__types

module Int_map = Map.Make (Stdlib.Int)

let rec atomic_add add k v m =
  let old = Atomic.get m in
  let new' = add k v old in
  if Atomic.compare_and_set m old new' then () else atomic_add add k v m

let rec atomic_remove remove k m =
  let old = Atomic.get m in
  let new' = remove k old in
  if Atomic.compare_and_set m old new' then () else atomic_remove remove k m

(* Errors *)

module Error = struct
  module Response = Webs_wd__types.Error_response
  type t =
  | Response of Response.t
  | Protocol of string
  | Connection_closing

  let pp ppf = function
  | Protocol e ->
      Fmt.pf ppf "@[<v>WebDriver protocol error:@,%a@]" Fmt.lines e
  | Response e ->
      Fmt.pf ppf "@[<v>WebDriver response error: %s@,@[%a@]@]"
        (Response.error e) Fmt.text (Response.message e)
  | Connection_closing ->
      Fmt.pf ppf "@[WebDriver connection is closing]"

  let to_string e = Fmt.str "%a" pp e
end

module Exn = struct
  exception Error of Error.t

  let init () =
    let printer = function Error e -> Some (Error.to_string e) | _ -> None in
    Printexc.register_printer printer

  let () = init ()
end

(* Driver connection *)

module Connection = struct

  (* Errors *)

  let err_close e =
    Fmt.error "@[<v>WebDriver connection close:@,%a@]" Fmt.lines e

  let err_send_encode name e =
    Fmt.str "@[<v>Command %s encode error:@,%a@]" name Fmt.lines e

  let err_message_decode json e =
    let json = Fmt.str "%a" (Fmt.truncated ~max:1024) json in
    Fmt.str "@[<v>Response message decode error:@,%a@,%a@]"
      Fmt.lines json Fmt.lines e

  let err_command_result_decode name json e =
    let json = Fmt.str "%a" Jsont.pp_json json in
    let json = Fmt.str "%a" (Fmt.truncated ~max:1024) json in
    Fmt.str "@[<v>Command %s result decode error:@,%a@,%a@]"
      name Fmt.lines json Fmt.lines e

  let err_event_params_decode name json e =
    let json = Fmt.str "%a" Jsont.pp_json json in
    let json = Fmt.str "%a" (Fmt.truncated ~max:1024) json in
    Fmt.str "@[<v>Event %s params decode error:@,%a@,%a@]"
      name Fmt.lines json Fmt.lines e

  let response_error err = raise (Exn.Error (Response err))
  let protocol_error err = raise (Exn.Error (Protocol err))

  (* Driver specification *)

  module Config = Webs_wd__driver.Config
  type custom =
    Config.t -> (Webs.Url.t * (unit -> (unit, string) result), string) result

  type driver =
  | Chrome
  | Edge
  | Firefox
  | Safari
  | Custom of custom

  let start_driver config = function
  | Custom cmd -> cmd config
  | Chrome -> Webs_wd__driver.Chrome.spawn config
  | Edge -> Error "Edge support is TODO"
  | Firefox -> Webs_wd__driver.Firefox.spawn config
  | Safari ->
      Error "Safari is unsupported as of writing:\n\
             See https://caniuse.com/wf-webdriver-bidi"

  (* Definitions for the protocol concurrency *)

  type cmd_id = int
  type _ Effect.t +=
  | Await_command : cmd_id * (_, 'result) Command.t -> 'result Effect.t
  | Await_event : 'params Event.t -> 'params Effect.t

  type 'a handler_id = 'a Type.Id.t
  type 'a handler_map_by_id = 'a Int_map.t Atomic.t

  type ready =
  | Ready : 'a handler_id * 'r * ('r, 'a) Effect.Deep.continuation -> ready
  | Disready : 'a handler_id * exn * ('r, 'a) Effect.Deep.continuation -> ready

  type blocked_on_command =
  | Command :
      'a handler_id * (_, 'r) Command.t * ('r, 'a) Effect.Deep.continuation ->
      blocked_on_command

  type blocked_on_event =
  | Event :
      'a handler_id * 'r Event.t * ('r, 'a) Effect.Deep.continuation ->
      blocked_on_event

  type 'a command_map_by_id = 'a Int_map.t Atomic.t
  type 'a event_map_by_method = 'a String_map.t Atomic.t

  (* Connections *)

  type t =
    { config : Config.t;
      close : (unit -> (unit, string) result);
      driver : driver;
      next_cmd_id : cmd_id Atomic.t;
      url : Webs.Url.t;
      websocket : Bytesrw_websocket.t;
      (* Protocol concurrency scheduler *)
      blocked_on_commands : blocked_on_command command_map_by_id;
      blocked_on_events : blocked_on_event list event_map_by_method;
      ready : ready Queue.t Atomic.t handler_map_by_id }

  let config c = c.config
  let driver c = c.driver
  let url c = c.url

  let open' ?(config = Config.make ()) driver =
    let* url, close = start_driver config driver in
    try
      Result.map_error failwith @@
      let max_recv_message_byte_size =
        Config.websocket_max_recv_message_byte_size config
      in
      let* websocket =
        Webs_wd__driver.websocket_handshake ~max_recv_message_byte_size ~url
      in
      let next_cmd_id = Atomic.make 0 in
      let blocked_on_commands = Atomic.make Int_map.empty in
      let blocked_on_events = Atomic.make String_map.empty in
      let ready = Atomic.make Int_map.empty in
      Ok { config; close; driver; next_cmd_id; url; websocket;
           blocked_on_commands; blocked_on_events; ready }
    with
    | Failure e -> ignore (close ()); Error e

  let close c =
    let ws = Bytesrw_websocket.close c.websocket in
    let close = c.close () in
    match ws, close with
    | Ok (), Ok () -> Ok ()
    | Error e, Ok () | Ok (), Error e -> err_close e
    | Error e0, Error e1 -> err_close (String.concat "\n" [e0; e1])

  (* Network IO *)

  let send_command ?exts c command params =
    try
      let id = Atomic.fetch_and_add c.next_cmd_id 1 in
      let cmd = Command.make ?exts id (Command.name command) params in
      let json =
        let t = Command.encode_jsont command in
        match Jsont_bytesrw.encode_string t cmd with
        | Ok json -> json
        | Error e -> protocol_error (err_send_encode (Command.name command) e)
      in
      (Config.protocol_tracer c.config) `Send json;
      Bytesrw_websocket.send ~text:true c.websocket json;
      id
    with
    | Bytes.Stream.Error e -> protocol_error (Bytes.Stream.error_message e)

  let recv_message c =
    try
      let json = Bytesrw_websocket.recv c.websocket in
      (Config.protocol_tracer c.config) `Recv json;
      match Jsont_bytesrw.decode_string Message.jsont json with
      | Ok msg -> msg
      | Error e -> protocol_error (err_message_decode json e)
    with
    | Bytes.Stream.Error e -> protocol_error (Bytes.Stream.error_message e)

  (* Handling the protocol concurrency *)

  let make_handler c =
    let handler_id = Type.Id.make () in
    let ready_queue = Atomic.make (Queue.create ()) (* FIXME mutex *)  in
    atomic_add Int_map.add (Type.Id.uid handler_id) ready_queue c.ready;
    handler_id

  let finish_handler c handler_id =
    (* TODO we don't have the right data structure to search
       for stuff in flight here. But we should lookup
       blocked_on_commands or blocked_on_events or should we
       rather let lookups gc ? But how do we discontinue ?
       It's unclear to me how we can end in the finish handler and have
       continuations in flight. *)
    atomic_remove Int_map.remove (Type.Id.uid handler_id) c.ready

  let remove_and_get_blocked_on_command c id =
    match Int_map.find_opt id (Atomic.get c.blocked_on_commands) with
    | None -> None
    | Some _ as b -> atomic_remove Int_map.remove id c.blocked_on_commands; b

  let remove_and_get_one_blocked_on_event c method' =
    (* Concurrently fishy + FIXME remove all *)
    match String_map.find_opt method' (Atomic.get c.blocked_on_events) with
    | None -> None
    | Some [] -> None
    | Some (b :: bs) (* TODO *) ->
        atomic_add String_map.add method' bs c.blocked_on_events; Some b

  let get_ready_queue c handler_id =
    match Int_map.find_opt (Type.Id.uid handler_id) (Atomic.get c.ready) with
    | None -> assert false
    | Some queue -> queue

  let add_ready c handler_id ready =
    let queue = get_ready_queue c handler_id in
    Queue.add ready (Atomic.get queue) (* FIXME *)

  let find_ready c handler_id =
    let queue = get_ready_queue c handler_id in
    Queue.take_opt (Atomic.get queue) (* FIXME *)

  let continue_ready : type a. a handler_id -> ready -> a =
  fun handler_id ready -> match ready with
  | Ready (ready_id, v, k) ->
      begin match Type.Id.provably_equal handler_id ready_id with
      | None -> assert false
      | Some Type.Equal -> Effect.Deep.continue k v
      end
  | Disready (ready_id, exn, k) ->
      begin match Type.Id.provably_equal handler_id ready_id with
      | None -> assert false
      | Some Type.Equal -> Effect.Deep.discontinue k exn
      end

  let rec handle_error_response :
    type a. t -> a handler_id -> Error_response.t -> a
    =
    fun c handler_id error ->
    let exn = Exn.Error (Response error) in
    match Error_response.id error with
    | None -> raise exn
    | Some id ->
        match remove_and_get_blocked_on_command c id with
        | Some Command (cmd_handler_id, _, k) ->
            begin match Type.Id.provably_equal handler_id cmd_handler_id with
            | Some Type.Equal -> Effect.Deep.discontinue k exn
            | None ->
                add_ready c cmd_handler_id (Disready (cmd_handler_id, exn, k));
                recv_next c handler_id ()
            end
        | None ->
            let e = Error_response.error error in
            (Config.drop_tracer c.config) (`Response (`Error e, id));
            recv_next c handler_id ()

  and handle_command_response :
    type a. t -> a handler_id -> Command_response.t -> a
    =
    fun c handler_id cmd ->
    let id = Command_response.id cmd in
    match remove_and_get_blocked_on_command c id with
    | None ->
        (Config.drop_tracer c.config) (`Response (`Command, id));
        recv_next c handler_id ()
    | Some Command (cmd_handler_id, command, k) ->
        let json = Command_response.result cmd in
        match Jsont.Json.decode (Command.result_jsont command) json with
        | Ok result ->
            begin match Type.Id.provably_equal handler_id cmd_handler_id with
            | Some Type.Equal -> Effect.Deep.continue k result
            | None ->
                add_ready c cmd_handler_id (Ready (cmd_handler_id, result, k));
                recv_next c handler_id ()
            end
        | Error e ->
            (* FIXME do that with a disready *)
            let name = Command.name command in
            protocol_error (err_command_result_decode name json e)

  and handle_event :
    type a. t -> a handler_id -> Event.instance -> a
    =
    fun c handler_id ev ->
    let method' = Event.method' ev in
    match remove_and_get_one_blocked_on_event c method' with
    | None ->
        (Config.drop_tracer c.config) (`Event method');
        (recv_next c handler_id () : a)
    | Some Event (ev_handler_id, event, k) ->
        let json = Event.params ev in
        match Jsont.Json.decode (Event.params_jsont event) json with
        | Ok params ->
            begin match Type.Id.provably_equal handler_id ev_handler_id with
            | Some Type.Equal -> Effect.Deep.continue k params
            | None ->
                add_ready c ev_handler_id (Ready (ev_handler_id, params, k));
                recv_next c handler_id ()
            end
        | Error e ->
            (* FIXME do that with a disready *)
            let name = Event.name event in
            protocol_error (err_event_params_decode name json e)

  and recv_next : type a. t -> a handler_id -> unit -> a =
  fun c handler_id () ->
    match find_ready c handler_id with
    | Some ready -> continue_ready handler_id ready
    | None ->
        match recv_message c with
        | Error_response error -> handle_error_response c handler_id error
        | Command_response cmd -> handle_command_response c handler_id cmd
        | Event event -> handle_event c handler_id event

  let rec handle c f =
    let rec loop c handler_id f = match f () with
    | v -> v
    | effect Await_command (id, command), k ->
        let blocked = Command (handler_id, command, k) in
        atomic_add Int_map.add id blocked c.blocked_on_commands;
        loop c handler_id (recv_next c handler_id)
    | effect Await_event event, k ->
        let blocked = Event (handler_id, event, k) in
        atomic_add String_map.add_to_list (Event.name event) blocked
          c.blocked_on_events;
        loop c handler_id (recv_next c handler_id)
    in
    let handler_id = make_handler c in
    let finally () = finish_handler c handler_id in
    Fun.protect ~finally @@ fun () -> loop c handler_id f

  let with_open ?config driver f =
    let* c = open' ?config driver in
    match handle c (fun () -> f c) with
    | v -> (match close c with Ok () -> Ok v | Error _ as e -> e)
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        let res = close c in
        match exn with
        | Exn.Error e ->
            let msg = match res with Ok () -> "" | Error e -> e in
            let msg = String.concat "\n" [Error.to_string e; msg] in
            Error msg
        | exn -> Printexc.raise_with_backtrace exn bt
end

let call ?exts c command params =
  let id = Connection.send_command c command params in
  Effect.perform (Connection.Await_command (id, command))

let await_event event_method =
  Effect.perform (Connection.Await_event event_method)
