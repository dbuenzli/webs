Webs — HTTP client and server toolkit for OCaml
===============================================

Webs is a toolkit for programming with HTTP in OCaml. It provides:

* IO and HTTP version agnostic representations for HTTP requests and responses.
* A client connector abstraction for making client requests with HTTP client
  implementations. 
* A service connector abstraction for running HTTP services defined. 
  Simple CGI and HTTP/1.1 gateway connectors are provided, but you can 
  bring your own.
* Optional service building blocks. File serving, authenticated
  cookies, sessions, passkeys, etc.

Webs is not a framework, it provides composable building blocks.

Webs is distributed under the ISC license. The base library only
depends on the core [`bytesrw`] library.

Optional service building blocks and gateway connectors may add
further dependencies for example on OCaml's `Unix` and `Thread`
module. The `bytesrw.crypto` is needed for building blocks that need
cryptography and some higher-level modules like support for `passkeys`
need [`jsont`]. The command line interface support depends on
[`cmdliner`].

Homepage: <https://erratique.ch/software/webs>  

[`cmdliner`]: https://erratique.ch/software/cmdliner
[`bytesrw`]: https://erratique.ch/software/bytesrw
[`jsont`]: https://erratique.ch/software/jsont

## Installation

Webs can be installed with `opam`:

    opam install webs
    opam install webs cmdliner # cmdliner support
    opam install webs conf-mbedtls # cryptography support
    opam install webs conf-mbedtls jsont # passkey support

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation can be consulted [online] or via `odig doc webs`.

Questions are welcome but better asked on the [OCaml forum] than on
the issue tracker.

[online]: https://erratique.ch/software/webs/doc
[OCaml forum]: https://discuss.ocaml.org/

## Sample programs 

A few programs can be found in the [test](test) directory,
see `b0 list`. You  can run them for example with 

    b0 -- authedcookie

The [`webs`](test/webs_tool.ml) command line tool serves files over
unencrypted HTTP/1.1 and makes HTTP requests with a `curl` backend.
