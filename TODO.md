
# A laundry list

* Make the login example bearable
* Session and the error scheme don't compose well. Add a session
  or response monad that ties everything together in a neat way ?
* Body request/response need to correctly handle failures/resources.
* Websocket upgrading works. The question is how to fit it in the model 
  once we upgraded. Maybe via a new body response case. Read
  again https://tools.ietf.org/html/rfc7230#section-6 and 6.7.
* Keep-alive, we can likely hide that in the connector we just need 
  to fit it in the picture with connection limits (which are not implemented
  anyways).
* remote peer address, x-forwarded-for and/or sockaddr from accept
* Look into clone()/chroot isolation
* With a gateway there are many moving configuration parts e.g. 
  try to minimise them.
* Make clear that connector log is for connector and unexpected 
  connector errors. Services are supposed to have their own error
  mecanism and/or logging (which may turn out to be the same).
  Also clarify the explain mecanism, and should we keep it ? 
* In response body enumeration we might mislabel unexpected service 
  exceptions as connector exception, fix that.
* Abstraction mismatch for specifying response bodies, you usually want 
  to specify the content type aswell. Maybe it would be better 
  to that have packed together.
* Review and strictly validate request_target on [Req.v] at the moment
  it seems to accept spaces. The function should likely return an 
  error or introduce an indirect type for the constructor.
* Move to a unified connector API ? 
* Not yet fully convinced by the `Http` module. It's getting there though.
* Do not trip too much on Resp.t -> Req.t and middlware model. In particular 
  it makes erroring paths obscure. Rather look into composing function 
  that construct ('a, Resp.t) result which end with (Resp.t, Resp.t) result.
  However that beaks once we meld sessions in, review that.
* We make as if `Resp/Req` are pure, their bodies are not. Resources...
* Localisation, we do want that early on, e.g. error handling needs it.
  Something should be provided in request.
* Sessions, nonces + csrf.
  https://cheatsheetseries.owasp.org/cheatsheets/Cross-Site_Request_Forgery_Prevention_Cheat_Sheet.html
  https://en.wikipedia.org/wiki/Cross-site_request_forgery
  Distinguish actions from users.
* Etag logic for in-memory serves. Integrate it in content responses. 

# Request bodies 

* 100 continue. For now nothing is done. Decide what to do, we could let
  the connectors unconditionally handle them. That defeats a bit the purpose
  but avoids e.g. curl to wait for 1s before proceeding anyways.
  The decision should likely be done by the service, but that doesn't fit 
  the Req.t -> Resp.t model.
* https://tools.ietf.org/html/rfc7578
* Need to support chunked encoding

# Reponses bodies 

* Resource cleanup 
* Buffering/Flushing ? For now we send each chunk directly. This 
  works well e.g. for sse. Should we rather buffer and have explicitly 
  flushing ?
* Support multiple byte ranges in `Webs_unix.send_file`.
* Support for directory indexes. 

# `webs` tool

Things that could be done. 

* Directory, indexes.
* Under a cli flag, PUTing files.
