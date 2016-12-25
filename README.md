Webs — Web service interface for OCaml
-------------------------------------------------------------------------------
%%VERSION%%

Webs defines a generic and *low-level* interface for web services
implemented in OCaml.

The interaction between the service and the HTTP web server that runs
it is mediated by a connector whose details depend on the
webserver.

Webs provides optional CGI and SCGI connectors.

Webs depends on [rresult][rresult], [hmap][hmap] and
[astring][astring].  The optional SCGI connector depend on [bos][bos]
and OCaml's Unix library.  Webs and its connectors are distributed the
ISC license.

[rresult]: http://erratique.ch/software/rresult
[hmap]: http://erratique.ch/software/hmap
[astring]: http://erratique.ch/software/astring
[bos]: http://erratique.ch/software/bos

Home page: http://erratique.ch/software/webs  

## Installation

Webs can be installed with `opam`:

    opam install webs

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc webs`.

[doc]: http://erratique.ch/software/webs/doc/Webs

## Sample programs

If you installed webs with `opam` sample programs are located in
the directory `opam config var webs:doc`.

In the distribution sample programs are located in the `test`
directory of the distribution. They can be built with:

    topkg build --tests true && topkg test

