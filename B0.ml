open B0_kit.V000

(* OCaml library names *)

let unix = B0_ocaml.libname "unix"
let threads = B0_ocaml.libname "threads.posix"
let cmdliner = B0_ocaml.libname "cmdliner"

let webs = B0_ocaml.libname "webs"
let webs_kit = B0_ocaml.libname "webs.kit"
let webs_unix = B0_ocaml.libname "webs.unix"
let webs_cli = B0_ocaml.libname "webs.cli"

(* Libraries *)

let webs_lib =
  let srcs = [`Dir (Fpath.v "src")] in
  B0_ocaml.lib ~name:"webs-lib" webs ~doc:"Webs library" ~srcs ~requires:[]

let webs_kit_lib =
  let srcs = [`Dir (Fpath.v "src/kit")] in
  let requires = [webs] in
  B0_ocaml.lib webs_kit ~doc:"Webs toolkit library" ~srcs ~requires

let webs_unix_lib =
  let srcs = [`Dir (Fpath.v "src/unix")] in
  let requires = [webs; unix; threads] in
  B0_ocaml.lib webs_unix ~doc:"Webs unix library" ~srcs ~requires

let webs_cli_lib =
  let srcs = [`Dir (Fpath.v "src/cli")] in
  let requires = [webs; webs_unix; cmdliner; unix] in
  B0_ocaml.lib webs_cli ~doc:"Webs command line library" ~srcs ~requires

(* Tools *)

let webs_tool =
  let srcs = Fpath.[`File (v "tool/webs_tool.ml")] in
  let requires = [cmdliner; unix; webs; webs_kit; webs_unix; webs_cli] in
  B0_ocaml.exe "webs" ~public:true ~doc:"Webs HTTP tool" ~srcs ~requires

(* Tests *)

let test ?doc base ~requires =
  let srcs = Fpath.[`File (v (Fmt.str "test/%s.ml" base))] in
  B0_ocaml.exe base ?doc ~srcs ~requires

let test_http = test "test_http" ~requires:[webs]
let test_http = test "test_sha_256" ~requires:[webs; webs_kit]
let test_authenticatable = test "test_authenticatable" ~requires:[webs;webs_kit]

(* Examples *)

let base_libs = [webs; webs_kit]
let quick_libs = webs_cli :: base_libs
let unix_libs = webs_unix :: unix :: base_libs

let example ?doc base ~requires =
  let srcs = Fpath.[`File (v (Fmt.str "examples/%s.ml" base))] in
  B0_ocaml.exe base ?doc ~srcs ~requires

let authedcookie = example "authedcookie" ~requires:(unix :: quick_libs)
let basicauth = example "basicauth" ~requires:quick_libs
let basicauth_sloppy = example "basicauth_sloppy" ~requires:quick_libs
let cgi = example "cgi" ~requires:unix_libs
let examples = example "examples" ~requires:(webs_unix :: quick_libs)
let form_methods = example "form_methods" ~requires:quick_libs
let gateway_send_file = example "gateway_send_file" ~requires:quick_libs
let http11_gateway = example "http11_gateway" ~requires:unix_libs
let login_cookie = example "login_cookie" ~requires:quick_libs
let min = example "min" ~requires:quick_libs
let multiconnector = example "multiconnector" ~requires:unix_libs
let naive_fetch = example "naive_fetch" ~requires:(cmdliner :: unix_libs)
let session = example "session" ~requires:quick_libs
let sse = example "sse" ~requires:(unix :: quick_libs)
let unix_send_file = example "unix_send_file" ~requires:(webs_unix::quick_libs)
let websocket = example "websocket" ~requires:quick_libs
let webpage = example "webpage" ~requires:quick_libs
let webpage_etag = example "webpage_etag" ~requires:quick_libs
let webpage_cache = example "webpage_cache" ~requires:quick_libs

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> B0_meta.(add authors) ["The webs programmers"]
    |> B0_meta.(add maintainers)
       ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> B0_meta.(add homepage) "https://erratique.ch/software/webs"
    |> B0_meta.(add online_doc) "https://erratique.ch/software/webs/doc"
    |> B0_meta.(add licenses) ["ISC"]
    |> B0_meta.(add repo) "git+https://erratique.ch/repos/webs.git"
    |> B0_meta.(add issues) "https://github.com/dbuenzli/webs/issues"
    |> B0_meta.(add description_tags)
      ["web"; "webserver"; "http"; "org:erratique"; ]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.add B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
          "--with-cmdliner" "%{cmdliner:installed}%"]]|}
    |> B0_meta.add B0_opam.depopts ["cmdliner", ""]
    |> B0_meta.add B0_opam.conflicts [ "cmdliner", {|< "1.0.0"|}]
    |> B0_meta.add B0_opam.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|}; ]
  in
  B0_pack.make "default" ~doc:"webs package" ~meta ~locked:true @@
  B0_unit.list ()
