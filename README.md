Webs — HTTP toolkit for OCaml
=============================
%%VERSION%%

Webs is a toolkit for programming with HTTP in OCaml. It provides:

* IO agnostic representations for HTTP requests and responses.
* Optional web service building blocks. File serving, authenticated
  cookies, sessions, etc.
* Connectors for running HTTP services defined as functions mapping
  requests to responses. CGI and HTTP/1.1 gateway connectors are
  provided, but you can bring your own.

Webs is not a framework, it is a set of building blocks.

Webs is distributed under the ISC license. The base library has no
dependencies. Some building blocks and the built-in gateway connectors
depend on OCaml's `Unix` and `Thread` modules. The command line
support depends on [`cmdliner`].

Homepage: <http://erratique.ch/software/webs>  

[`cmdliner`]: https://erratique.ch/software/cmdliner

## Installation

Webs can be installed with `opam`:

    opam install webs
    opam install webs cmdliner # with cmdliner support

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation can be consulted [online] or via `odig doc webs`.

Questions are welcome but better asked on the [OCaml forum] than on
the issue tracker.

[online]: https://erratique.ch/software/webs/doc
[OCaml forum]: https://discuss.ocaml.org/

## Sample programs 

A few programs can be found in the [examples](examples) directory you 
can run them for example with `b0 -- authedcookie`, see `b0 list`. 

The [`webs`](examples/webs_tool.ml) command line tool serves files
over HTTP/1.1.
