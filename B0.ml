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
  B0_ocaml.lib webs_kit ~srcs ~requires:[webs] ~exports:[webs]

let webs_unix_lib =
  let srcs = [`Dir ~/"src/unix"] in
  let requires = [bytesrw; webs; unix; threads] in
  B0_ocaml.lib webs_unix ~srcs ~requires ~exports:[webs]

let webs_cli_lib =
  let srcs = [`Dir ~/"src/cli"] in
  let requires = [webs; webs_unix; cmdliner; unix] in
  B0_ocaml.lib webs_cli ~srcs ~requires ~exports:[webs]

(* Tools *)

let webs_tool =
  let srcs = Fpath.[`File (v "test/webs_tool.ml")] in
  let requires = [cmdliner; unix; webs; webs_kit; webs_unix; webs_cli] in
  B0_ocaml.exe "webs" ~public:true ~doc:"Webs HTTP tool" ~srcs ~requires

(* Tests *)

let test ?(requires = []) =
  B0_ocaml.test ~requires:(b0_std :: webs :: requires)

let test_http = test ~/"test/test_http.ml" ~run:true
let test_base64 = test ~/"test/test_base64.ml" ~run:true ~requires:[webs_kit]
let test_url = test ~/"test/test_url.ml" ~run:true
let test_sha_256 =
  test ~/"test/test_sha_256.ml" ~run:true ~requires:[webs_kit]

let test_authenticatable =
  test ~/"test/test_authenticatable.ml" ~run:true ~requires:[webs_kit]

let test_cryptorand =
  test ~/"test/test_cryptorand.ml" ~run:true ~requires:[webs_kit]

(* Examples *)

let base_libs = [webs; webs_kit]
let quick = webs_cli :: base_libs
let unix = webs_unix :: unix :: quick

let authedcookie = test ~/"test/authedcookie.ml" ~run:false ~requires:unix
let basicauth = test ~/"test/basicauth.ml" ~run:false ~requires:quick
let basicauth_sloppy =
  test ~/"test/basicauth_sloppy.ml" ~run:false ~requires:unix

let cgi = test ~/"test/cgi.ml" ~run:false ~requires:unix
let cookbook = test ~/"test/cookbook.ml" ~run:false ~requires:unix
let examples = test ~/"test/examples.ml" ~run:false ~requires:unix
let fetch = test ~/"test/fetch.ml" ~run:false ~requires:unix
let form_methods = test ~/"test/form_methods.ml" ~run:false ~requires:quick
let gateway_send_file =
  test ~/"test/gateway_send_file.ml" ~run:false ~requires:quick

let http11_gateway = test ~/"test/http11_gateway.ml" ~run:false ~requires:unix

let login_cookie = test ~/"test/login_cookie.ml" ~run:false ~requires:quick
let min = test ~/"test/min.ml" ~run:false ~requires:quick
let multiconnector = test ~/"test/multiconnector.ml" ~run:false ~requires:unix
let naive_fetch =
  test ~/"test/naive_fetch.ml" ~run:false ~requires:(cmdliner :: unix)

let serve = test ~/"test/serve.ml" ~run:false ~requires:quick
let session = test ~/"test/session.ml" ~run:false ~requires:quick
let sse = test ~/"test/sse.ml" ~run:false ~requires:unix
let unix_send_file =
  test ~/"test/unix_send_file.ml" ~run:false ~requires:unix

let websocket = test ~/"test/websocket.ml" ~run:false ~requires:quick
let webpage = test ~/"test/webpage.ml" ~run:false ~requires:quick
let webpage_etag = test ~/"test/webpage_etag.ml" ~run:false ~requires:quick
let webpage_cache = test ~/"test/webpage_cache.ml" ~run:false ~requires:quick

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
        "topkg", {|build & >= "1.1.0"|};
        "bytesrw", {||};
      ]
  in
  B0_pack.make "default" ~doc:"webs package" ~meta ~locked:true @@
  B0_unit.list ()
