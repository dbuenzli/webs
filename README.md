Webs — Web service toolkit for OCaml
====================================
%%VERSION%%

Webs is a toolkit for programming web services in OCaml. It provides:

* A generic low-level interface for defining web services in terms
  of HTTP request-responses cycles.
* Optional service building blocks. File serving, authenticated
  cookies, sessions, etc.
* Connectors. They run services and are in charge of talking 
  to the HTTP gateway or client. CGI and HTTP/1.1 gateway
  connectors are provided, but you can bring your own.

Webs tries as much as possible to be a library and not a framework.

Webs is distributed under the ISC license. It has no dependencies. The
connector libraries depend on OCaml's `Unix` and `Thread` modules.

Home page: http://erratique.ch/software/webs  

## Installation

Webs can be installed with `opam`:

    opam install webs
    opam install webs cmdliner   # with cmdliner support

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation can be consulted [online][doc] or via `odig doc webs`.

Questions are welcome but better asked on the [OCaml forum][ocaml-forum] 
than on the issue tracker.

[doc]: https://erratique.ch/software/webs/doc
[ocaml-forum]: https://discuss.ocaml.org/

## Sample programs 

A few programs can be found in the [test][test] directory. 

The [`webs`](test/webs_tool) command line tool serves your files
over HTTP/1.1.
