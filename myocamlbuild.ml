open Ocamlbuild_plugin
open Command

let lib s = match !Ocamlbuild_plugin.Options.ext_lib with
 | "" -> s ^ ".a"
 | x -> s ^ "." ^ x

let () =
  dispatch begin function
  | After_rules ->
      dep ["record_webs_unix_stubs"] [lib "src/libwebs_unix_stubs"];
      flag_and_dep
        ["link"; "ocaml"; "link_webs_unix_stubs"]
        (P (lib "src/libwebs_unix_stubs"));

      flag ["library"; "ocaml"; "byte"; "record_webs_unix_stubs"]
        (S ([A "-dllib"; A "-lwebs_unix_stubs"]));

      flag ["library"; "ocaml"; (* byte and native *)
            "record_webs_unix_stubs"]
        (S ([A "-cclib"; A "-lwebs_unix_stubs"]));

      ocaml_lib ~tag_name:"use_webs_unix_stubs"
        ~dir:"src" "src/webs_unix";

      flag ["link"; "ocaml"; "use_webs_unix_stubs"]
        (S [A "-ccopt"; A "-Lsrc"]);

      dep ["compile";"c"]
        ["src/vendor/sha256.h";
         "src/vendor/bitfn.h"; ];

      dep ["record_webs_kit_stubs"] [lib "src/libwebs_kit_stubs"];
      flag_and_dep
        ["link"; "ocaml"; "link_webs_kit_stubs"]
        (P (lib "src/libwebs_kit_stubs"));

      flag ["library"; "ocaml"; "byte"; "record_webs_kit_stubs"]
        (S ([A "-dllib"; A "-lwebs_kit_stubs"]));

      flag ["library"; "ocaml"; (* byte and native *)
            "record_webs_kit_stubs"]
        (S ([A "-cclib"; A "-lwebs_kit_stubs"]));

      ocaml_lib ~tag_name:"use_webs_kit_stubs"
        ~dir:"src" "src/webs_kit";

      flag ["link"; "ocaml"; "use_webs_kit_stubs"]
        (S [A "-ccopt"; A "-Lsrc"]);

  | _ -> ()
  end
