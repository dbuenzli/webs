open B0_kit.V000
open B00_std

(* OCaml library names *)

let unix = B0_ocaml.libname "unix"
let threads = B0_ocaml.libname "threads.posix"
let cmdliner = B0_ocaml.libname "cmdliner"

let webs = B0_ocaml.libname "webs"
let webs_kit = B0_ocaml.libname "webs.kit"
let webs_websocket = B0_ocaml.libname "webs.websocket"
let webs_unix = B0_ocaml.libname "webs.unix"
let webs_tpool = B0_ocaml.libname "webs.tpool"
let webs_connector = B0_ocaml.libname "webs.connector"
let webs_cgi = B0_ocaml.libname "webs.cgi"
let webs_httpc = B0_ocaml.libname "webs.httpc"
let webs_cli = B0_ocaml.libname "webs.cli"
let webs_html = B0_ocaml.libname "webs.html"

(* Libraries *)

let mod_srcs m =
  let mli = Fmt.str "src/%s.mli" m and ml = Fmt.str "src/%s.ml" m in
  Fpath.[ `File (v mli); `File (v ml) ]

let webs_lib =
  let srcs = mod_srcs "webs" in
  B0_ocaml.lib webs ~doc:"Webs library" ~srcs ~requires:[]

let webs_kit_lib =
  let srcs =
    `File (Fpath.v "src/webs_stubs_sha.c") ::
    mod_srcs "webs_kit"
  in
  let requires = [webs] in
  B0_ocaml.lib webs_kit ~doc:"Webs toolkit library" ~srcs ~requires

let webs_websocket_lib =
  let srcs = mod_srcs "webs_websocket" in
  let requires = [webs; webs_kit] in
  B0_ocaml.lib webs_websocket ~doc:"Webs websocket library" ~srcs ~requires

let webs_unix_lib =
  let srcs =
    `File (Fpath.v "src/webs_stubs_sendfile.c") ::
    `File (Fpath.v "src/webs_stubs_realpath.c") ::
    `File (Fpath.v "src/webs_stubs_time.c") ::
    mod_srcs "webs_unix"
  in
  let requires = [unix; webs] in
  B0_ocaml.lib webs_unix ~doc:"Webs unix library" ~srcs ~requires

let webs_tpool_lib =
  let srcs = mod_srcs "webs_tpool" in
  let requires = [threads] in
  B0_ocaml.lib webs_tpool ~doc:"Webs thread pool" ~srcs ~requires

let webs_connector_lib =
  let srcs = mod_srcs "webs_connector" in
  let requires = [webs] in
  B0_ocaml.lib webs_connector ~doc:"Webs connector tools" ~srcs ~requires

let webs_cgi_lib =
  let srcs = mod_srcs "webs_cgi" in
  let requires = [unix; webs; webs_kit; webs_connector; webs_unix] in
  B0_ocaml.lib webs_cgi ~doc:"Webs CGI connector" ~srcs ~requires

let webs_httpc_lib =
  let srcs = mod_srcs "webs_httpc" in
  let requires = [threads; unix; webs; webs_kit; webs_connector; webs_unix;
                  webs_tpool]
  in
  B0_ocaml.lib webs_httpc ~doc:"Webs HTTP/1.1 connector" ~srcs ~requires

let webs_cli_lib =
  let srcs = mod_srcs "webs_cli" in
  let requires = [cmdliner; webs; webs_unix; webs_httpc] in
  B0_ocaml.lib webs_cli ~doc:"Webs command line library" ~srcs ~requires

let webs_html_lib =
  let srcs = mod_srcs "webs_html" in
  B0_ocaml.lib webs_html ~doc:"Webs HTML generation" ~srcs

(* Tools *)

let webs_tool =
  let srcs = Fpath.[`File (v "examples/webs_tool.ml")] in
  let requires = [cmdliner; webs; webs_kit; webs_unix; webs_cli; webs_httpc] in
  B0_ocaml.exe "webs_tool" ~doc:"Webs HTTP/1.1 server" ~srcs ~requires

(* Tests *)

let test ?doc base ~requires =
  let srcs = Fpath.[`File (v (Fmt.str "test/%s.ml" base))] in
  B0_ocaml.exe base ?doc ~srcs ~requires

let example ?doc base ~requires =
  let srcs = Fpath.[`File (v (Fmt.str "examples/%s.ml" base))] in
  B0_ocaml.exe base ?doc ~srcs ~requires

let test_http = test "test_http" ~requires:[webs]
let test_http = test "test_sha_256" ~requires:[webs; webs_kit]
let test_authenticatable =
  test "test_authenticatable" ~requires:[webs; webs_kit]

let example_httpc = example "httpc" ~requires:[webs; webs_kit; webs_httpc]
let example_cgi = example "cgi" ~requires:[webs; webs_kit; unix; webs_cgi]
let example_bauth = example "bauth" ~requires:[webs; webs_kit; webs_cli]
let example_sse = example "sse" ~requires:[webs; webs_kit; webs_cli]
let example_formality = example "formality" ~requires:[webs; webs_kit; webs_cli]
let example_min = example "min" ~requires:[webs; webs_cli]

let example_count_acookie =
  example "count_acookie" ~requires:[webs; webs_kit; webs_cli]

let example_count_session =
  example "count_session" ~requires:[webs; webs_kit; webs_cli]

let example_files_unix =
  example "files_unix" ~requires:[webs; webs_kit; webs_unix; webs_cli]

let example_files_gateway =
  example "files_gateway" ~requires:[webs; webs_kit; webs_cli]

let example_websocket =
  example "websocket" ~requires:[webs; webs_kit; webs_websocket; webs_cli]

let example_multic =
  example "multic" ~requires:[webs; webs_kit; webs_cgi; webs_httpc]

let example_login =
  example "login" ~requires:[webs; webs_kit; webs_cli]

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The webs programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/webs"
    |> add online_doc "https://erratique.ch/software/webs/doc"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/webs.git"
    |> add issues "https://github.com/dbuenzli/webs/issues"
    |> add description_tags ["www"; "webserver"; "org:erratique"; ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
          "--with-cmdliner" "%{cmdliner:installed}%"]]|}
    |> add B0_opam.Meta.depopts ["cmdliner", ""]
    |> add B0_opam.Meta.conflicts [ "cmdliner", {|< "1.0.0"|}]
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|}; ]
    |> tag B0_opam.tag
  in
  B0_pack.v "default" ~doc:"webs package" ~meta ~locked:true @@
  B0_unit.list ()
