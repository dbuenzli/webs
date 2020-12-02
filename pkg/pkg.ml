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
    Pkg.mllib "src/webs_kit.mllib" ~dst_dir:"kit";
    Pkg.clib "src/libwebs_kit_stubs.clib" ~lib_dst_dir:"kit";
    Pkg.mllib "src/webs_websocket.mllib" ~dst_dir:"websocket";
    Pkg.mllib "src/webs_unix.mllib" ~dst_dir:"unix";
    Pkg.clib "src/libwebs_unix_stubs.clib" ~lib_dst_dir:"unix";
    Pkg.mllib "src/webs_cgi.mllib" ~dst_dir:"cgi";
    Pkg.mllib "src/webs_httpc.mllib" ~dst_dir:"httpc";
    Pkg.mllib ~cond:cmdliner "src/webs_cli.mllib" ~dst_dir:"cli";
    Pkg.bin ~cond:cmdliner "test/webs_tool" ~dst:"webs";
    Pkg.test ~run:false "test/httpc";
    Pkg.test ~run:false "test/cgi";
    Pkg.test "test/test_http";
    Pkg.test ~run:false "test/multic";
    Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
    Pkg.doc
      "doc/web_service_howto.mld" ~dst:"odoc-pages/web_service_howto.mld";
    Pkg.doc "doc/tutorial.mld" ~dst:"odoc-pages/tutorial.mld";
]
