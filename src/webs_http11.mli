(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** HTTP/1.1 codecs. *)

open Bytesrw
open Webs

val decode_header_list : bytes -> crlfs:int list -> Http.Headers.t
(** [decode_list b crlfs] decodes the headers. [b] has the
    header section with the start line (either request or status
    line) or finished by the first [crlfs].

    {b TODO} Get rid of this. *)

(** Version codec. *)
module Version : sig
  val decode : string -> (Http.Version.t, string) result
  (** [decode s] decodes a version in
      {{:https://www.rfc-editor.org/rfc/rfc9112#name-http-version}HTTP/1.1
      syntax} from [s]. Single digit versions are also parsed, some
      serializations (e.g. the [curl] tool) do that. *)

  val encode : Http.Version.t -> string
  (** [encode v] encodes the version [v] in
      {{:https://www.rfc-editor.org/rfc/rfc9112#name-http-version}
      HTTP/1.1 syntax}. Assumes correct integer ranges. *)
end

(** Response codec. *)
module Response : sig

  val decode_status_line :
    bytes -> first:int -> last:int -> Http.Version.t * Http.Status.t * string
  (** [decode_status_line b ~first ~crlf] decodes a status line
      that starts at [first] and whose ending CRLF starts at [last+1].
      Raises [Failure] on errors.

      {b TODO} Do not expose this. *)

  (** {1:decoding Decoding} *)

  val read :
    ?head_buffer:bytes -> ?log:string -> Bytes.Reader.t ->
    (Http.Response.t, string) result
  (** [read r] reads an HTTP/1.1 response from [r]. The body of the response
      is an {!Webs.Http.Body.Bytes_reader} on [r].

      {ul
      {- [log] is used as the {!Webs.Http.Response.log}.}
      {- [head_buffer] is the temporary buffer used to store the HTTP head.
         Defaults to a fresh fresh buffer of size
         {!Webs.Http.Connector.Default.max_http_head_byte_size}.}}

      The function errors if the message head cannot fit in [head_buffer]
      or if a decoding error occurs. *)

  (** {1:encoding Encoding} *)

  val encode_head : Http.Response.t -> string
  (** [encode_head response] is the HTTP/1.1 message head of
      [reponse]: the status line, the [crlf]-terminated headers and the
      final [crlf]. The written headers go through
      {!Webs.Http.Headers.for_connector} with the body of [response]. *)

  val encode : Http.Response.t -> string
  (** [encode response] encodes [response] to an HTTP/1.1 response
      with {!encode_head} followed by the response body.

      Raises [Invalid_argument] on {!Webs.Http.Body.Custom} bodies. *)

  val write_head : Bytes.Writer.t -> Http.Response.t -> unit
  (** [write_head w response] writes the {!encode_head} of [response]
      on [w]. *)

  val write : eod:bool -> Bytes.Writer.t -> Http.Response.t -> unit
  (** [write ~eod w response] writes [response] as an HTTP/1.1 response on
      [w] with {!write_head}, followed by the response body and a final
      {!Bytes.Slice.eod} iff [eod] is [true].

      Raises [Invalid_argument] on {!Webs.Http.Body.Custom} bodies. *)
end

(** Request codec. *)
module Request : sig

  val decode_line :
    bytes -> first:int -> last:int -> Http.Method.t * string * Http.Version.t
  (** [decode_line b ~first ~last] decodes a request line that
      starts at [first] and whose ending CRLF starts at [last+1]. Raises
      [Failure] on errors.

      {b TODO} Do not expose this. *)

  (** {1:decoding Decoding} *)

  val read :
    ?head_buffer:bytes -> ?log:string -> service_path:Http.Path.t ->
    Bytes.Reader.t -> (Http.Request.t, Http.Response.t) result
  (** [read r] reads an HTTP/1.1 request from [r]. The body of the request
      is an {!Http.Body.Bytes_reader} on [r].

      {ul
      {- [log] is used as the {!Webs.Http.Request.log}}
      {- [head_buffer] is the temporary buffer used to store the HTTP head.
         Defaults to a fresh fresh buffer of size
         {!Webs.Http.Connector.Default.max_http_head_byte_size}.}}

      The result is devised with {!Http.for_service_connector} with the given
      [service_path] and thus satisfies the
      {{!page-connector_conventions.service_requests}service requests
      conventions}.

      The function errors if the message header cannot fit in [header_buffer]
      or if a decoding error occurs. *)

  (** {1:encoding Encoding} *)

  val encode_head : Http.Request.t -> string
  (** [encode_head request] is the HTTP/1.1 message head of
      [request]: the request line, the [crlf]-terminated headers and the
      final [crlf]. The written headers go through
      {!Http.Headers.for_connector} with the body [request]. *)

  val encode : Http.Request.t -> string
  (** [encode request] encodes [request] to an HTTP/1.1 request with
      {!encode_head} followed by the request body.

      Raises [Invalid_argument] on {!Webs.Http.Body.Custom} bodies. *)

  val write_head : Bytes.Writer.t -> Http.Request.t -> unit
  (** [write_head w request] writes {!encode_head} of [request] on [w]. *)

  val write : eod:bool -> Bytes.Writer.t -> Http.Request.t -> unit
  (** [write ~eod w request] writes [request] as an HTTP/1.1 request
      on [w] with {!write_head}, followed by the request body, and
      a final {!Bytes.Slice.eod} iff [eod] is [true].

      Raises [Invalid_argument] on {!Webs.Http.Body.Custom} bodies. *)
end
