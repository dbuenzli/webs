(*---------------------------------------------------------------------------
   Copyright (c) 2015 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** File system responses.

    See {{!page-http_service_howto.serving_files}this section} of the
    HTTP service howto. See also {!Webs_gateway.send_file}.

    To use {!send_file} you need a connector that supports
    {!Webs_unix.Fd.Writer} body writers. For example {!Webs_http11_gateway} and
    {!Webs_cgi} and do. *)

open Webs

(** {1:etaggers Etaggers} *)

type etagger =
  Http.Path.fpath -> Unix.file_descr -> Unix.stats ->
  (Http.Etag.t, string) result
(** The type for functions for determining file etags. The function is
    given the filepath, a file descriptor open on it and its file stats
    record. *)

val default_etagger : etagger
(** [default_etagger] implements the [nginx] etag scheme. This is a
    strong etag with the file stat's mtime and size written in
    lowercase hexadecimal as [hex(mtime)-hex(size)]. *)

(** {1:dir_resp Directory responses} *)

type dir_response =
  etagger:etagger -> media_types:Media_type.file_ext_map option ->
  Http.Request.t -> Http.Path.fpath -> Unix.file_descr -> Unix.stats ->
  (Http.Response.t, Http.Response.t) result
(** The type for functions for directory responses.

    Functions of this signature are invoked by {!send_file} when they
    hit directories. They are given an [etagger], [media_types], the
    request, the file path to the (existing) directory, a file descriptor
    open on it and its file stats records.

    The function should follow up with the response for the directory.
    {b It is its duty to eventually close the given file descriptor
    whatever happens}. If the response ends up being a file, [etagger]
    and [media_types] should be used to determine its etag and media
    type. *)

val dir_404 : dir_response
(** [dir_404] is a directory response that always returns [Error r]
    with [r] an empty {!Webs.Http.Status.not_found_404} response. *)

val dir_index_file : string -> (dir_response, string) result
(** [dir_index_file filename] serves the file named [filename] in the
    directory via {!send_file}. Returns an [Error _] if [file]
    {{!Webs.Http.Path.has_dir_seps}contains a directory separator} or
    is [".."]. If you know that statically use {!Result.ok}. *)

(** {1:send File sending} *)

val send_file :
  ?dir_response:dir_response -> ?etagger:etagger ->
  ?media_types:Media_type.file_ext_map -> Http.Request.t ->
  Http.Path.fpath -> (Http.Response.t, Http.Response.t) result
(** [send_file request file] responds to request [request] by sending
    the file [file]. Use {!Webs.Http.Request.to_absolute_filepath} to
    determine a [file] from [request] safely.

    The long details are as follows.

    The function first checks if [request] is a GET or HEAD
    request. If not it returns [Error r] with [r] an empty
    {!Webs.Http.Status.method_not_allowed_405} response.

    It then opens [file] for reading and [stat(2)]s it:

    {ol
    {- If [file] is a directory, continues with [dir_response] (defaults to
       {!dir_404}).}
    {- If [file] is a file, an etag [etag] is computed for [file] using
      [etagger] (defaults to {!default_etagger}).}}

    Equipped with the [etag] the function proceeds to perform, in order:

    {ol
    {- {{!Webs.Http.Etag.eval_if_match}Evaluate} [request]'s
       {!Webs.Http.Headers.if_match} header
       condition (if any) with tag [etag]. If that is [false], return
       [Error r] with [r] an empty {!Webs.Http.Status.precondition_failed_412}
       response.}
    {- {{!Webs.Http.Etag.eval_if_none_match}Evaluate} [request]'s
       {!Webs.Http.Headers.if_none_match} header condition
       (if any) with tag [etag]. If that is [false], returns
       [Ok r] with [r] an empty {!Webs.Http.Status.not_modified_304}
       response.}
    {- If [request] is a [HEAD] request, returns
       with [Ok r] with [r] an empty {!Webs.Http.Status.ok_200}
       response.}
    {- If [request] has a {!Webs.Http.Headers.range} header,
       {{!Webs.Http.Etag.eval_if_range}evaluate} [request]'s
       {!Webs.Http.Headers.if_range} header
       (if any) with tag [t]. If that is [false] or if there is no
       {!Webs.Http.Headers.if_range}, the range request is
       turned into a full response (see next point). Otherwise the first
       satisfiable range is served in the body of [Ok r] with [r] a
       {!Webs.Http.Status.partial_content_206} response. Or if
       there is no satisfiable range, [Error r] is returned with [r] an empty
       {!Webs.Http.Status.range_not_satisfiable_416} response.}
    {- If [request] has no {!Webs.Http.Headers.range} header,
       responds with [Ok r] with [r] a {!Webs.Http.Status.ok_200} response
       with [file]'s content as a body.}}

    The content type of the response body is determined using
    {!Webs.Media_type.of_filepath} with [mime_types] (defaults to
    {!Webs.Media_type.default_file_ext_map}) and [file].  The body is
    sent using a {!Webs_unix.Fd.Writer} custom body content with the
    {!Webs_unix.val-sendfile} system call or a fallback if not
    available.

    In addition to the errors mentioned above, the function also
    returns [Error r] with [r]:
    {ul
    {- A {!Webs.Http.Status.bad_request_400} empty response on any header
       decoding errors.}
    {- A {!Webs.Http.Status.not_found_404} empty response, if there's any
       file system call error (e.g. [EPERM]).
       A human readable explanation can be found in the response's
       {{!Webs.Http.Response.explain}explanation} which stays on the server.}}

    {b Caching.} The responses do not add caching information. You should
    add them yourself on [Ok _] responses, for example as follows:

{[
let send_asset ~strip ~file_root request =
  let* file = Http.Request.to_absolute_filepath ~strip ~file_root request in
  let* response = Webs_fs.send_file request file in
  let forever = "public, max-age=31536000, immutable" in
  let hs = Http.Headers.(def cache_control) forever Http.Headers.empty in
  Http.Response.override_headers ~by:hs response
]}

    Responses however include an {!Webs.Http.Headers.last_modified}
    header derived from the file modification time. It seems without
    this header, one does not hit the browser memory cache in blink
    based browsers (not sure if this is still the case in 2023).

    {b Limitations.} For range requests a single range is responded with;
    there is no [multipart/byteranges] support for the time being. *)
