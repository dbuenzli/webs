(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult

(* Errors *)

type error =
  [ `Webserver of R.msg | `Connector of R.msg | `Service of R.exn_trap ]

let pp_error ppf = function
| `Webserver (`Msg m) -> Format.fprintf ppf "@[webserver: %s@]" m
| `Connector (`Msg m) -> Format.fprintf ppf "@[connector: %s@]" m
| `Service t -> Format.fprintf ppf "@[service: %a@]" R.pp_exn_trap t

let error_to_msg = function
| Ok _ as v -> v
| Error e -> Error (`Msg (Format.asprintf "%a" pp_error e))

let open_error = function
| Ok _ as r -> r | Error #error as r -> r

(* Connectors *)

type conf = Hmap.t
type t = conf -> (Webs_req.t -> Webs_resp.t) -> (unit, error) result

(* Standard configuration keys *)

let sendfile_header : string Hmap.key = Hmap.Key.create ()
let service_exn_log : Format.formatter Hmap.key = Hmap.Key.create ()

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
