(*---------------------------------------------------------------------------
   Copyright (c) 2026 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Wd = struct
  module Error = Webs_wd__protocol.Error
  include Webs_wd__protocol.Exn
  module Connection = Webs_wd__protocol.Connection
  module Event = struct
    type 'a t = 'a Webs_wd__types.Event.t
    let name ev = Webs_wd__types.Event.name ev
    let await ev = Webs_wd__protocol.await_event ev
    let define = Webs_wd__types.Event.define
  end

  (* Common types *)

  module Exts = Webs_wd__types.Exts
  module Empty_params = Webs_wd__types.Empty_params
  module Empty_result = Webs_wd__types.Empty_result
  module User_context = Webs_wd__types.User_context
  module Browsing_context = Webs_wd__types.Browsing_context

  (* Debug helpers *)

  let pp_json t ppf v = match Jsont.Json.encode t v with
  | Error msg -> Format.pp_print_text ppf msg
  | Ok json -> Jsont.pp_json ppf json
end

module Wd_session = Webs_wd__session
module Wd_browser = Webs_wd__browser
module Wd_browsing_context = Webs_wd__browsing_context
module Wd_emulation = Webs_wd__emulation
module Wd_network = Webs_wd__network
module Wd_script = Webs_wd__script
module Wd_storage = Webs_wd__storage
module Wd_log = Webs_wd__log
module Wd_input = Webs_wd__input
module Wd_web_extension = Webs_wd__web_extension
