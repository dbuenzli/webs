(*---------------------------------------------------------------------------
   Copyright (c) 2020 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Gateway interaction. *)

open Webs

(** {1:sending_file File serving handoff}

    Tools for letting the gateway serve files. See
    {{!page-web_service_howto.serving_files}this section} of the web
    service howto. To let the service itself send them see
    {!Webs_fs}. *)

val send_file :
  header:Http.Headers.Name.t -> Http.Path.fpath -> (Http.Response.t, 'e) result
(** [send_file ~header file] is a response that instructs the gateway
    to serve file [file]. Use
    {!Webs.Http.Request.to_absolute_filepath} to safely determine
    a file path from a request.

    More precisely this is a {!Webs.Http.Status.ok_200} empty response
    with the header field name [header] set to [file]. The actual
    header name to use depends on your gateway, see below.*)

val x_accel_redirect : Http.Headers.Name.t
(** [x_accel_redirect] is the header name
    {{:https://www.nginx.com/resources/wiki/start/topics/examples/x-accel/}
    [x-accel-redirect]} used by nginx for internal redirection. *)

val x_sendfile : Http.Headers.Name.t
(** [x_sendfile] is the header name [x-sendfile]. Used by Apache and
    lighttpd for file serving. *)
