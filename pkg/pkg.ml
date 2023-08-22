#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let cmdliner = Conf.with_pkg "cmdliner"
let () =
  Pkg.describe "webs" @@ fun c ->
  let cmdliner = Conf.value c cmdliner in
  Ok [
    Pkg.mllib "src/webs.mllib";
    Pkg.mllib "src/kit/webs_kit.mllib" ~dst_dir:"kit";
    Pkg.clib "src/kit/libwebs_kit_stubs.clib" ~lib_dst_dir:"kit";
    Pkg.mllib "src/unix/webs_unix.mllib" ~dst_dir:"unix";
    Pkg.clib "src/unix/libwebs_unix_stubs.clib" ~lib_dst_dir:"unix";
    Pkg.mllib ~cond:cmdliner "src/cli/webs_cli.mllib" ~dst_dir:"cli";
    Pkg.bin ~cond:cmdliner "tool/webs_tool" ~dst:"webs";
    Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
    Pkg.doc "doc/http_service_howto.mld"
      ~dst:"odoc-pages/http_service_howto.mld";
    Pkg.doc "doc/connector_conventions.mld"
      ~dst:"odoc-pages/connector_conventions.mld";
    Pkg.doc "doc/tutorial.mld" ~dst:"odoc-pages/tutorial.mld";
    Pkg.doc "doc/todo.mld" ~dst:"odoc-pages/todo.mld";
]
