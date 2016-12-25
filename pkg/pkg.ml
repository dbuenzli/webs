#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let cmdliner = Conf.with_pkg "cmdliner"
let bos = Conf.with_pkg "bos"
let unix = Conf.with_pkg "base-unix"

let () =
  Pkg.describe "webs" @@ fun c ->
  let cmdliner = Conf.value c cmdliner in
  let bos = Conf.value c bos in
  let unix = Conf.value c unix in
  Ok [
    Pkg.mllib ~api:["Webs"] "src/webs.mllib";
    Pkg.mllib "src/webs_cgi.mllib";
    Pkg.mllib ~cond:bos "src/webs_scgi.mllib";
    Pkg.mllib ~cond:unix "src/webs_unix.mllib";
    Pkg.mllib ~cond:cmdliner "src/webs_cli.mllib";
    Pkg.mllib "src/webs_use.mllib";
    Pkg.test ~run:false "test/examples";
    Pkg.test ~run:false "test/revolt";
    Pkg.test "test/test";
    Pkg.test "test/test_http";
    Pkg.doc "test/examples.ml";
    Pkg.doc "test/revolt.ml";
    Pkg.doc "test/echo.ml";
    Pkg.doc "support/nginx-sample.conf"; ]
