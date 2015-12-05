#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let cmdliner = Env.bool "cmdliner"
let bos = Env.bool "bos"

let () =
  Pkg.describe "webs" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/webs";
    Pkg.lib ~exts:Exts.module_library "src/webs_cgi";
    Pkg.lib ~cond:bos ~exts:Exts.module_library "src/webs_scgi";
    Pkg.doc "test/examples.ml";
    Pkg.doc "test/revolt.ml";
    Pkg.doc "test/echo.ml";
    Pkg.doc "support/nginx-sample.conf";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
