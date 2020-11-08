open B0_kit.V000
open B00_std

(* OCaml library names *)

let unix = B0_ocaml.libname "unix"
let threads = B0_ocaml.libname "threads"
let cmdliner = B0_ocaml.libname "cmdliner"

let webs = B0_ocaml.libname "webs"
let webs_kit = B0_ocaml.libname "webs.kit"
let webs_websocket = B0_ocaml.libname "webs.websocket"
let webs_unix = B0_ocaml.libname "webs.unix"
let webs_cgi = B0_ocaml.libname "webs.cgi"
let webs_httpc = B0_ocaml.libname "webs.httpc"
let webs_cli = B0_ocaml.libname "webs.cli"

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
    mod_srcs "webs_unix"
  in
  let requires = [unix; webs; webs_kit] in
  B0_ocaml.lib webs_unix ~doc:"Webs unix library" ~srcs ~requires

let webs_cgi_lib =
  let srcs = mod_srcs "webs_cgi" in
  let requires = [unix; webs; webs_kit; webs_unix] in
  B0_ocaml.lib webs_cgi ~doc:"Webs CGI connector" ~srcs ~requires

let webs_httpc_lib =
  let srcs = mod_srcs "webs_httpc" in
  let requires = [threads; unix; webs; webs_kit; webs_unix] in
  B0_ocaml.lib webs_httpc ~doc:"Webs HTTP/1.1 connector" ~srcs ~requires

let webs_cli_lib =
  let srcs = mod_srcs "webs_cli" in
  let requires = [cmdliner; webs; webs_unix; webs_httpc] in
  B0_ocaml.lib webs_cli ~doc:"Webs command line library" ~srcs ~requires

(* Tools *)

let webs_tool =
  let srcs = Fpath.[`File (v "test/webs_tool.ml")] in
  let requires = [cmdliner; webs; webs_kit; webs_unix; webs_cli; webs_httpc] in
  B0_ocaml.exe "webs_tool" ~doc:"Webs HTTP/1.1 server" ~srcs ~requires

(* Tests *)

let test base ~requires =
  let srcs = Fpath.[`File (v (Fmt.str "test/%s.ml" base))] in
  B0_ocaml.exe base ~doc:(Fmt.str "test %s" base) ~srcs ~requires

let test_http = test "test_http" ~requires:[webs]
let test_http = test "test_sha_256" ~requires:[webs; webs_kit]
let test_authenticatable =
  test "test_authenticatable" ~requires:[webs; webs_kit]

let test_httpc = test "httpc" ~requires:[webs; webs_kit; webs_httpc]
let test_cgi = test "cgi" ~requires:[webs; webs_kit; unix; webs_cgi]
let test_bauth = test "bauth" ~requires:[webs; webs_kit; webs_cli]
let test_sse = test "sse" ~requires:[webs; webs_kit; webs_cli]
let test_formality = test "formality" ~requires:[webs; webs_kit; webs_cli]
let test_min = test "min" ~requires:[webs; webs_cli]

let test_count_acookie =
  test "count_acookie" ~requires:[webs; webs_kit; webs_cli]

let test_count_session =
  test "count_session" ~requires:[webs; webs_kit; webs_cli]

let test_files_unix =
  test "files_unix" ~requires:[webs; webs_kit; webs_unix; webs_cli]

let test_files_gateway =
  test "files_gateway" ~requires:[webs; webs_kit; webs_cli]

let test_websocket =
  test "websocket" ~requires:[webs; webs_kit; webs_websocket; webs_cli]

let test_multic =
  test "multic" ~requires:[webs; webs_kit; webs_cgi; webs_httpc]

let test_login =
  test "login" ~requires:[webs; webs_kit; webs_cli]

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
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> tag B0_opam.tag
  in
  B0_pack.v "default" ~doc:"webs package" ~meta ~locked:true @@
  B0_unit.list ()
