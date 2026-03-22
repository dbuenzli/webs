(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Result.Syntax

module Session = struct
  type t =
    { headers : Webs__headers.t;
      (* TODO cookie jar *) }

  let make ?(headers = Webs__headers.empty) () = { headers }
end

module type T = sig
  type t
  val id : t -> string
  val request : t -> Webs__request.t -> (Webs__response.t, string) result
end

type t = V : (module T with type t = 'a) * 'a * Session.t option -> t
let make m c session = V (m, c, session)

let id (V ((module C), c, _)) = C.id c
let session (V (_, _, session)) = session

(* TODO we could likely expose some of the redirection logic here. *)

let x_follow_location = Webs__headers.Name.make "x-follow-location"

let find_rel_location ~loc rel request response =
  let scheme = Webs__scheme.encode (Webs__request.scheme request) in
  let* host =
    Webs__headers.(find_or_error host) (Webs__request.headers request)
  in
  try match (rel : Webs__url.relative_kind) with
  | Absolute_path -> Ok (String.concat "" [scheme; "://"; host; loc])
  | Relative_path ->
      let path = Webs__request.raw_path request in
      begin match String.rindex_opt path '/' with
      | None -> Ok (String.concat "" [scheme; "://"; host; "/"; loc])
      | Some i ->
          let path = String.sub path 0 i in
          Ok (String.concat "" [scheme; "://"; host; path; "/"; loc ])
      end
  | Empty | Scheme -> raise Exit
  with
  | Exit ->
      Webs__base.Fmt.error "Could not construct redirect from %s to %s"
        (Webs__request.to_url request |> Result.retract)
        loc

let find_location request response =
  let* loc =
    Webs__headers.(find_or_error location) (Webs__response.headers response)
  in
  match Webs__url.kind loc with
  | Absolute -> Ok loc
  | Relative rel -> find_rel_location ~loc rel request response

let request_host request =
  let scheme = Webs__request.scheme request in
  Webs__headers.decode_host scheme (Webs__request.headers request)

let unconditional_redirection_drops headers =
  headers
  |> Webs__headers.(undefine referer)
  |> Webs__headers.(undefine origin)
  |> Webs__headers.(undefine connection)
  |> Webs__headers.(undefine if_match)
  |> Webs__headers.(undefine if_none_match)
  |> Webs__headers.(undefine if_modified_since)
  |> Webs__headers.(undefine if_unmodified_since)
  |> Webs__headers.(undefine if_range)

let host_change_drops headers =
  headers
  |> Webs__headers.(undefine authorization)
  |> Webs__headers.(undefine proxy_authorization)
  |> Webs__headers.(undefine cookie)

let redirect_response visited request response =
  match Webs__response.status response with
  | 301 | 302 | 303 | 305 | 307 | 308 ->
      let* url = find_location request response in
      if List.mem url visited
      then Webs__base.Fmt.error "Redirection loop: %s" url else
      let method' = Webs__request.method' request in
      let headers = Webs__request.headers request in
      let headers = unconditional_redirection_drops headers in
      let* last_host = request_host request in
      let* request = Webs__request.of_url ~headers method' ~url in
      let* new_host = request_host request in
      let request =
        if last_host = new_host then request else
        let headers = host_change_drops (Webs__request.headers request) in
        Webs__request.with_headers headers request
      in
      Ok (Some (url, request))
  | _ -> Ok None

let default_max_redirection = 10

let request
    ?(max_redirections = default_max_redirection)
    (V ((module C), c, session)) ~follow request
  =
  let rec loop n follow visited request =
    if n <= 0
    then Webs__base.Fmt.error "Too many redirects (%d)" max_redirections else
    let method' = Webs__request.method' request in
    let follow = match method' with `GET | `HEAD -> follow | _ -> false in
    let* response = C.request c request in
    if not follow then Ok response else
    let* redirect = redirect_response visited request response in
    match redirect with
    | Some (url, request) -> loop (n - 1) follow (url :: visited) request
    | None ->
        begin match visited with
        | [] -> Ok response
        | last :: _ ->
            let hs = Webs__response.headers response in
            let hs = Webs__headers.(hs |> define x_follow_location last) in
            Ok (Webs__response.with_headers hs response)
        end
  in
  loop max_redirections follow [] request

let get httpc ~follow ~url =
  let* request' = Webs__request.of_url `GET ~url in
  let* response = request httpc ~follow request' in
  match Webs__response.status response with
  | 200 -> Ok (Webs__body.to_string (Webs__response.body response))
  | st -> Error (Format.asprintf "%a" Webs__status.pp st)
