(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** HTTP interactions. Open to use it.

    Open the module to use it. It defines only these modules in your scope. *)

module Url = Webs__url
module Media_type = Webs__media_type

(** HTTP datatypes.

    Along with a few codecs and protocol logic fragments.

    {b References.}
    {ul
    {- {{:https://www.rfc-editor.org/rfc/rfc9110}HTTP semantics} –
    {{:https://www.rfc-editor.org/rfc/rfc9111}HTTP caching} -
    {{:https://www.rfc-editor.org/rfc/rfc9112}HTTP/1.1} –
    {{:https://www.rfc-editor.org/rfc/rfc9113}HTTP/2} –
    {{:https://www.rfc-editor.org/rfc/rfc9114}HTTP/3}.}} *)
module Http : sig

  (** {1:codecs Base codecs and types} *)

  module Digits = Webs__digits
  module Version = Webs__version
  module Method = Webs__method
  module Path = Webs__path
  module Query = Webs__query
  module Scheme = Webs__scheme
  module Status = Webs__status
  module Body = Webs__body

  (** {1:headers Headers} *)

  module Headers = Webs__headers
  module Cookie = Webs__cookie
  module Etag = Webs__etag
  module Range = Webs__range

  (** {1:responses_and_requests Responses and requests} *)

  module Response = Webs__response
  module Request = Webs__request

  module Client = Webs__client

  (** {1:connector_tools Connector tools} *)

  module Connector = Webs__connector

  (**/**)
  val string_subrange : ?first:int -> ?last:int -> string -> string
  val string_lowercase : string -> string
end
