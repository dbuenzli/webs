(* This code is in the public domain *)

open Webs

let revolt r = match Req.meth r with
| `GET ->
    let headers = HTTP.H.(empty |> def content_type "text/plain") in
    Resp.v HTTP.s200_ok headers (Resp.string_body "Revolt!\n")
| _ ->
    let headers = HTTP.H.(empty |> def allow (HTTP.encode_meth `GET)) in
    Resp.v HTTP.s405_not_allowed headers Resp.empty_body
