(* This code is in the public domain *)

open Webs

let revolt r = match Req.meth r with
| `GET ->
    let text = "text/plain; charset=UTF-8" in
    let headers = HTTP.H.(empty |> def content_type text) in
    Resp.v HTTP.s200_ok headers (Resp.string_body "Revolt!\n")
| _ ->
    let headers = HTTP.H.(empty |> def allow (HTTP.encode_meth `GET)) in
    Resp.v HTTP.s405_not_allowed headers Resp.empty_body

let conf = Hmap.(add Webs_cgi.vars ["SERVER_SOFTWARE"] empty)

let () = match Webs_cgi.connect conf revolt with
| Error e -> Format.eprintf "%a%!" Webs.Connector.pp_error e; exit 1
| Ok () -> exit 0
