(*---------------------------------------------------------------------------
   Copyright (c) 2012 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Url = Webs__url
module Media_type = Webs__media_type
module Http = struct
  module Digits = Webs__digits
  module Version = Webs__version
  module Method = Webs__method
  module Path = Webs__path
  module Query = Webs__query
  module Scheme = Webs__scheme
  module Status = Webs__status
  module Body = Webs__body

  module Headers = Webs__headers
  module Cookie = Webs__cookie
  module Etag = Webs__etag
  module Range = Webs__range

  module Response = Webs__response
  module Request = Webs__request
  module Client = Webs__client

  module Connector = Webs__connector

  let string_subrange = Webs__base.string_subrange
  let string_lowercase = Webs__base.string_lowercase
end
