* reponse body what about errors ?
* say something about content-length header for body.
* websocket upgrade
* status codec
* HTTP.decode_* use result
* Don't think we really want `Service of exn_trap, formally that would
  only be cgi usually we want long running. Connectors maybe always
  log on stderr or the formatter specified with service_exn_log.
* Problem on header decoding can we systematically apply decode_multi_value
  except on set-cookie ? I think not. So in fact the canonical representation
  should not see 3.2.2.
