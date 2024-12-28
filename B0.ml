open B0_kit.V000

(* OCaml library names *)

let b0_std = B0_ocaml.libname "b0.std"
let unix = B0_ocaml.libname "unix"
let threads = B0_ocaml.libname "threads.posix"
let cmdliner = B0_ocaml.libname "cmdliner"
let bytesrw = B0_ocaml.libname "bytesrw"

let webs = B0_ocaml.libname "webs"
let webs_kit = B0_ocaml.libname "webs.kit"
let webs_unix = B0_ocaml.libname "webs.unix"
let webs_cli = B0_ocaml.libname "webs.cli"

(* Libraries *)

let webs_lib =
  let srcs = [`Dir ~/"src"] in
  let requires = [bytesrw] in
  B0_ocaml.lib ~name:"webs-lib" webs ~srcs ~requires

let webs_kit_lib =
  let srcs = [`Dir ~/"src/kit"] in
  let requires = [webs] and exports = [webs] in
  B0_ocaml.lib webs_kit ~srcs ~requires ~exports

let webs_unix_lib =
  let srcs = [`Dir ~/"src/unix"] in
  let requires = [bytesrw; webs; unix; threads] in
  let exports = [webs] in
  B0_ocaml.lib webs_unix ~srcs ~requires ~exports

let webs_cli_lib =
  let srcs = [`Dir ~/"src/cli"] in
  let requires = [webs; webs_unix; cmdliner; unix] in
  let exports = [webs] in
  B0_ocaml.lib webs_cli ~srcs ~requires ~exports

(* Tools *)

let webs_tool =
  let srcs = Fpath.[`File (v "tool/webs_tool.ml")] in
  let requires = [cmdliner; unix; webs; webs_kit; webs_unix; webs_cli] in
  B0_ocaml.exe "webs" ~public:true ~doc:"Webs HTTP tool" ~srcs ~requires

(* Tests *)

let test
    ?long:(l = false) ?doc ?run:(r = true) ?(requires = []) ?(srcs = []) src
  =
  let srcs = (`File src) :: srcs in
  let requires = b0_std :: webs :: requires in
  let meta = B0_meta.(empty |> tag test |> ~~ run r |> ~~ long l) in
  let name = Fpath.basename ~strip_ext:true src in
  B0_ocaml.exe name ~srcs ~requires ~meta ?doc

let test_http = test ~/"test/test_http.ml" ~run:true
let test_base64 = test ~/"test/test_base64.ml" ~run:true
let test_url = test ~/"test/test_url.ml" ~run:true
let test_sha_256 =
  test ~/"test/test_sha_256.ml" ~run:true ~long:true ~requires:[webs_kit]

let test_authenticatable =
  test ~/"test/test_authenticatable.ml" ~run:true ~requires:[webs_kit]

let test_cryptorand =
  test ~/"test/test_cryptorand.ml" ~run:true ~requires:[webs_kit]


(* Examples *)

let base_libs = [webs; webs_kit]
let quick_libs = webs_cli :: base_libs
let unix_libs = webs_unix :: unix :: base_libs

let sample ?(doc = "Sample code") ?(srcs = []) src ~requires =
  let src = Fpath.(~/"examples"/src) in
  let srcs = `File src :: srcs in
  let meta = B0_meta.(empty |> tag sample) in
  let name = Fpath.basename ~strip_ext:true src in
  B0_ocaml.exe name ~srcs ~requires ~meta ~doc

let authedcookie = sample "authedcookie.ml" ~requires:(unix :: quick_libs)
let basicauth = sample "basicauth.ml" ~requires:quick_libs
let basicauth_sloppy = sample "basicauth_sloppy.ml" ~requires:quick_libs
let cgi = sample "cgi.ml" ~requires:unix_libs
let examples = sample "examples.ml" ~requires:(webs_unix :: quick_libs)
let form_methods = sample "form_methods.ml" ~requires:quick_libs
let gateway_send_file = sample "gateway_send_file.ml" ~requires:quick_libs
let http11_gateway = sample "http11_gateway.ml" ~requires:unix_libs
let login_cookie = sample "login_cookie.ml" ~requires:quick_libs
let min = sample "min.ml" ~requires:quick_libs
let multiconnector = sample "multiconnector.ml" ~requires:unix_libs
let naive_fetch = sample "naive_fetch.ml" ~requires:(cmdliner :: unix_libs)
let session = sample "session.ml" ~requires:quick_libs
let sse = sample "sse.ml" ~requires:(unix :: quick_libs)
let unix_send_file =
  sample "unix_send_file.ml" ~requires:(webs_unix::quick_libs)

let websocket = sample "websocket.ml" ~requires:quick_libs
let webpage = sample "webpage.ml" ~requires:quick_libs
let webpage_etag = sample "webpage_etag.ml" ~requires:quick_libs
let webpage_cache = sample "webpage_cache.ml" ~requires:quick_libs

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The webs programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/webs"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/webs/doc"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/webs.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/webs/issues"
    |> ~~ B0_meta.description_tags
      ["web"; "webserver"; "http"; "org:erratique"; ]
    |> B0_meta.tag B0_opam.tag
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
          "--with-cmdliner" "%{cmdliner:installed}%"]]|}
    |> ~~ B0_opam.depopts ["cmdliner", ""]
    |> ~~ B0_opam.conflicts [ "cmdliner", {|< "1.3.0"|}]
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "bytesrw", {||};
      ]
  in
  B0_pack.make "default" ~doc:"webs package" ~meta ~locked:true @@
  B0_unit.list ()
