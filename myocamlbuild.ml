(* ocamlbuild script of libinput *)

open Ocamlbuild_plugin
open Ocamlbuild_pack

let run_and_read = Ocamlbuild_pack.My_unix.run_and_read
let mark_tag_used = Ocamlbuild_pack.Flags.mark_tag_used

let split s ch =
  let x = ref [] in
  let rec go s =
    let pos = String.index s ch in
    x := (String.before s pos)::!x;
    go (String.after s (pos + 1))
  in
  try go s
  with Not_found -> !x

let split_nl s = split s '\n'

let before_space s =
  try String.before s (String.index s ' ')
  with Not_found -> s

(* this lists all supported packages *)
let find_packages () =
  List.map before_space (split_nl & run_and_read "ocamlfind list")

(* ocamlfind command *)
let ocamlfind x = S[A"ocamlfind"; x]

let _ = dispatch begin function
  | Before_options ->
      (* override default commands by ocamlfind ones *)
      Options.ocamlc     := ocamlfind & A"ocamlc";
      Options.ocamlopt   := ocamlfind & A"ocamlopt";
      Options.ocamldep   := ocamlfind & A"ocamldep";
      Options.ocamldoc   := ocamlfind & A"ocamldoc";
      Options.ocamlmktop := ocamlfind & A"ocamlmktop";

      (* taggings *)
      tag_any
        ["pkg_str";
         "pkg_unix";
         "pkg_yojson";
         "pkg_batteries";
        ];

      tag_file "unittest/test_input.ml" ["pkg_oUnit"];
      tag_file "unittest/test_input.native" ["pkg_oUnit"];
      mark_tag_used "pkg_oUnit" (* to disable warning *)

  | After_rules ->

      (* When one link an OCaml library/binary/package, one should use -linkpkg *)
      flag ["ocaml"; "link"; "program"] & A"-linkpkg";

      (* For each ocamlfind package one inject the -package option when
       * compiling, computing dependencies, generating documentation and
       * linking. *)
      List.iter begin fun pkg ->
        flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S[A"-package"; A pkg];
      end (find_packages ());

      (* debugging & optimization *)
      flag ["ocaml"; "compile"] (S[A"-g"]);
      flag ["ocaml"; "link"] (S[A"-g"]);
      flag ["ocaml"; "compile"; "native"] (S[A"-inline";A"10"]);

  | _ -> ()
end

