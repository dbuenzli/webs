(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let err_service_path ~service_path ~path ~raw_path =
  Webs__base.Fmt.str "Cannot strip service path %a from request path %a (%s)"
    Webs__path.pp service_path Webs__path.pp path raw_path

(* Requests *)

type t =
  { body : Webs__body.t;
    headers : Webs__headers.t;
    log : string;
    method' : Webs__method.t;
    path : Webs__path.t;
    query : string option;
    raw_path : string;
    scheme : Webs__scheme.t;
    service_path : Webs__path.t;
    version : Webs__version.t; }

let make
    ?(headers = Webs__headers.empty) ?(log = "") ?(path = Webs__path.root)
    ?(query = None) ?(scheme = Webs__scheme.Https)
    ?(service_path = Webs__path.root) ?(version = Webs__version.none) method'
    ~raw_path body
  =
  { body; headers; log; method'; path; query; raw_path; scheme; service_path;
    version }

let for_service_connector
    ?log ?scheme ~service_path ~version method' ~raw_path ~headers body
  =
  match Webs__path.and_query_string_of_request_target raw_path with
  | Error e -> Webs__body.dismiss body; Webs__response.bad_request_400 ~log:e ()
  | Ok (path, query) ->
      if Webs__path.is_none path (* raw_path is "*" *)
      then Ok (make ?log ?scheme ~headers ~path ~query ~service_path ~version
                 method' ~raw_path body)
      else
      let path = Webs__path.strip_prefix ~prefix:service_path path in
      if not (Webs__path.is_none path)
      then Ok (make ?log ?scheme ~headers ~path ~query ~service_path ~version
                 method' ~raw_path body)
      else
      let log = err_service_path ~service_path ~path ~raw_path in
      Webs__body.dismiss body; Webs__response.bad_gateway_502 ~log ()

let of_url
    ?(body = Webs__body.empty) ?(headers = Webs__headers.empty) ?log method'
    ~url
  =
  try
    let url =
      if Webs__url.is_likely_percent_decoded url
      then Webs__url.Percent.encode Uri url
      else url
    in
    let scheme = Webs__scheme.decode_of_url url |> Result.error_to_failure in
    let host = match Webs__url.authority url with
    | Some authority ->
        let host = Webs__url.Authority.host authority in
        let port = Webs__url.Authority.port authority in
        begin match port with
        | None -> host
        | Some port -> String.concat ":" [host; Int.to_string port]
        end
    | None ->
        Webs__base.Fmt.failwith "No authority found in URL %s" url
    in
    let headers = Webs__headers.(define host) host headers in
    let path = match Webs__url.path url with None -> "/" | Some path -> path in
    let query = Webs__url.query url in
    let raw_path = match query with
    | None -> path
    | Some query -> String.concat "?" [path; query]
    in
    let path, query =
      Webs__path.and_query_string_of_request_target raw_path
      |> Result.error_to_failure
    in
    let service_path = Webs__path.root in
    Ok (make ~headers ?log ~scheme ~service_path ~path ~query method' ~raw_path
          body)
  with
  | Failure e -> Webs__body.dismiss body; Error e

let to_url request =
  match Webs__headers.(find_or_error host) request.headers with
  | Error _ as e -> e
  | Ok host ->
      let scheme = Webs__scheme.encode request.scheme in
      Ok (String.concat "" [scheme; "://"; host; request.raw_path])

let with_body body request =
  Webs__body.dismiss request.body; { request with body }

let with_headers headers request = { request with headers }
let override_headers ~by:headers request =
  { request with headers = Webs__headers.override request.headers ~by:headers }

(* Properties *)

let body request = request.body
let headers request = request.headers
let log request = request.log
let method' request = request.method'
let path request = request.path
let query request = request.query
let raw_path request = request.raw_path
let scheme request = request.scheme
let service_path request = request.service_path
let version request = request.version

(* Deconstructing and responding *)

let redirect_to_path ?body:b ?headers ?log ?reason request status path =
  let loc = Webs__path.concat (service_path request) path in
  let loc = Webs__path.encode loc in
  Webs__body.dismiss (body request);
  Webs__response.redirect ?body:b ?headers ?log ?reason status loc

let decode_header name dec request =
  match Webs__headers.find name (headers request) with
  | None -> Ok None
  | Some v ->
      match dec v with
      | Ok v -> Ok (Some v)
      | Error e ->
          let reason = Webs__base.Fmt.str "%s: %s" (name :> string) e in
          Webs__body.dismiss (body request);
          Webs__response.bad_request_400 ~reason ()

let allow allowed request =
  match Webs__method.constrain ~allowed (method' request) with
  | Ok _ as v -> v
  | Error allow ->
      Webs__body.dismiss (body request);
      Webs__response.method_not_allowed_405 ~allowed:(List.map fst allow) ()

let find_cookie ~name request =
  let cs = match Webs__headers.(find cookie) (headers request) with
  | None -> Ok []
  | Some s -> Webs__cookie.decode_list  s
  in
  match cs with Error _ as e -> e | Ok cs -> Ok (List.assoc_opt name cs)

let to_query request =
  let url_query request = match query request with
  | None -> Webs__query.empty
  | Some q -> Webs__query.decode q
  in
  let body_query request =
    let headers = headers request in
    let content_type =
      match Webs__headers.(find ~lowervalue:true content_type) headers with
      | Some t -> t
      | None -> Webs__body.content_type (body request)
    in
    if Webs__media_type.is_none content_type then begin
      Webs__body.dismiss (body request);
      Webs__response.bad_request_400 ~reason:"missing content type" ()
    end else begin
      let t = Webs__media_type.get_type content_type in
      if Webs__media_type.(equal t application_x_www_form_urlencoded) then
        let body = body request in
        if Webs__body.is_custom body then begin
          let log = "Don't know how to read custom body" in
          Webs__body.dismiss body;
          Webs__response.server_error_500 ~log ()
        end
        else Ok (Webs__query.decode (Webs__body.to_string body))
      else begin
        Webs__body.dismiss (body request);
        Error (Webs__response.empty
                 Webs__status.unsupported_media_type_415 ~log:t)
      end
    end
  in
  match method' request with
  | `GET | `HEAD -> Ok (url_query request)
  | _ -> body_query request

let clean_path request =
  let not_empty s = not (String.equal s "") in
  match path request with
  | [] | [""] -> Ok ()
  | p when List.for_all not_empty p -> Ok ()
  | p ->
      let p = match (List.filter not_empty p) with [] -> [""] | p -> p in
      let loc = Webs__path.concat (service_path request) p in
      let loc = Webs__path.encode loc in
      let log = "path cleaning" in
      Webs__body.dismiss (body request);
      Error (Webs__response.redirect
               ~log Webs__status.moved_permanently_301 loc)

let to_absolute_filepath ?(strip = [""]) ~file_root request =
  match Webs__path.strip_prefix ~prefix:strip (path request) with
  | [] ->
      let log =
        Webs__base.Fmt.str "could not strip prefix %a" Webs__path.pp strip
      in
      Webs__body.dismiss (body request);
      Webs__response.not_found_404 ~log ()
  | p ->
      match Webs__path.to_absolute_filepath p with
      | Error e ->
          Webs__body.dismiss (body request);
          Webs__response.bad_request_400 ~log:e ()
      | Ok filepath ->
          Ok (Webs__path.prefix_filepath ~prefix:file_root filepath)

let eval_if_none_match request etag ~headers:hs =
  let headers = Webs__headers.(define etag) (Webs__etag.encode etag) hs in
  match
    decode_header Webs__headers.if_none_match Webs__etag.decode_cond request
  with
  | Error _ as e -> e
  | Ok None -> Ok headers
  | Ok (Some cond) ->
      if Webs__etag.eval_if_none_match cond (Some etag) then Ok headers else
      begin
        Webs__body.dismiss (body request);
        Error (Webs__response.empty ~headers Webs__status.not_modified_304)
      end

(* Predicates and comparisons *)

let is_body_empty request = Webs__body.is_empty request.body
let compare r0 r1 =
  let c = Webs__body.compare r0.body r1.body in
  if c <> 0 then c else
  let c = Webs__headers.compare r0.headers r1.headers in
  if c <> 0 then c else
  let c = String.compare r0.log r1.log in
  if c <> 0 then c else
  let c = Webs__method.compare r0.method' r1.method' in
  if c <> 0 then c else
  let c = Webs__path.compare r0.path r1.path in
  if c <> 0 then c else
  let c = Repr.compare r0.query r1.query in
  if c <> 0 then c else
  let c = String.compare r0.raw_path r1.raw_path in
  if c <> 0 then c else
  let c = Webs__scheme.compare r0.scheme r1.scheme in
  if c <> 0 then c else
  let c = Webs__path.compare r0.service_path r1.service_path in
  if c <> 0 then c else
  Webs__version.compare r0.version r1.version

let equal r0 r1 = compare r0 r1 = 0

(* Formatting *)

let pp ppf request =
  let open Webs__base in
  let pp_query ppf = function
  | None -> Fmt.pf ppf "<none>"
  | Some q -> Fmt.pf ppf "%S" q
  in
  Format.pp_open_vbox ppf 0;
  Fmt.field "method" Webs__method.pp ppf request.method'; Fmt.cut ppf ();
  Fmt.field "path" Webs__path.pp_dump ppf request.path; Fmt.cut ppf ();
  Fmt.field "query" pp_query ppf request.query; Fmt.cut ppf ();
  Fmt.field "version" Webs__version.pp ppf request.version; Fmt.cut ppf ();
  Fmt.field "raw-path" Fmt.qstring ppf request.raw_path; Fmt.cut ppf ();
  Fmt.field "scheme" Webs__scheme.pp ppf request.scheme; Fmt.cut ppf ();
  Fmt.field "service-path" Webs__path.pp_dump ppf request.service_path;
  Fmt.cut ppf ();
  Webs__headers.pp ppf request.headers;
  if not (Webs__headers.is_empty request.headers) then Fmt.cut ppf ();
  Fmt.field "body" Webs__body.pp ppf request.body;
  Format.pp_close_box ppf ()

(* Echo *)

let echo ?(status = Webs__status.ok_200) request =
  let body = body request in
  let body =
    if Webs__body.is_custom body
    then "<unknown custom body>"
    else Webs__body.to_string body
  in
  let body = Format.asprintf "@[<v>%a@,%s@]" pp request body in
  Webs__response.text status body
