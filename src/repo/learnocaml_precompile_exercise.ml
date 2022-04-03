(* Compile objects from an exercise *)

open Lwt.Infix

(* FIXME: make these configurable *)
let grading_cmis_dir, grading_ppx_dir =
  let prefix = Filename.dirname (Filename.dirname (Sys.executable_name)) in
  let ( / ) = Filename.concat in
  ref (prefix/"share"/"learn-ocaml"/"grading_cmis"),
  ref (prefix/"lib"/"learn-ocaml"/"grading_ppx")

let run ?dir cmd args =
  Lwt_process.exec ?cwd:dir ("", Array.of_list (cmd::args)) >>= function
  | Unix.WEXITED 0 -> Lwt.return_unit
  | _ -> Lwt.fail_with ("Compilation failed: " ^ String.concat " " (cmd::args))

let is_fresh =
  let mtime f = Unix.((stat f).st_mtime) in
  let exe_mtime =
    try mtime (Sys.executable_name) with Unix.Unix_error _ -> max_float
  in
  fun ?(dir=".") target srcs ->
    let target = Filename.concat dir target in
    let srcs = List.map (Filename.concat dir) srcs in
    try
      let mt = mtime target in
      mt > exe_mtime && List.for_all (fun f -> mt > mtime f) srcs
    with Unix.Unix_error _ -> false

let ocamlc ?(dir=Sys.getcwd ()) ?(opn=[]) ~source ~target args =
  let d = Filename.concat dir in
  if is_fresh ~dir target source then Lwt.return_unit else
  let args = "-I" :: dir :: "-I" :: !grading_cmis_dir :: args in
  let args = args @ List.map d source @ ["-o"; d target] in
  let args = List.fold_right (fun m acc -> "-open" :: m :: acc) opn args in
  run "ocamlc" args

let jsoo ?(dir=Sys.getcwd ()) ~source ~target args =
  let d = Filename.concat dir in
  if is_fresh ~dir target [source] then Lwt.return_unit else
  let args = "--wrap-with=dynload" :: args in
  let args = args @ [d source; "-o"; d target] in
  run "js_of_ocaml" args

let precompile ~exercise_dir =
  let dir = exercise_dir in
  ocamlc ~dir ["-c"] ~opn:["Learnocaml_callback"]
    ~source:["prelude.ml"] ~target:"prelude.cmo"
  >>= fun () ->
  ocamlc ~dir ["-c"] ~opn:["Learnocaml_callback"; "Prelude"]
    ~source:["prepare.ml"] ~target:"prepare.cmo"
  >>= fun () ->
  ocamlc ~dir ["-c"] ~opn:["Learnocaml_callback"; "Prelude"; "Prepare"]
    ~source:["solution.ml"] ~target:"solution.cmo"
  >>= fun () ->
  Lwt.join [
    (ocamlc ~dir ["-a"]
       ~source:["prelude.cmo"; "prepare.cmo"; "solution.cmo"]
       ~target:"exercise.cma"
     >>= fun () ->
     jsoo ~dir [] ~source:"exercise.cma" ~target:"exercise.js");
    (ocamlc ~dir ["-c";
                  "-I"; "+compiler-libs";
                  "-ppx"; Filename.concat !grading_ppx_dir "learnocaml-ppx-metaquot"]
       ~opn:["Learnocaml_callback"; "Prelude"; "Prepare"; "Test_lib.Open_me"]
       ~source:["test.ml"]
       ~target:"test.cmo"
     >>= fun () ->
     (* Todo: support for depends.txt *)
     ocamlc ~dir ["-a"; (* "-linkall" *)]
       ~source:["test.cmo"]
       ~target:"test.cma"
     >>= fun () ->
     jsoo ~dir [] ~source:"test.cma" ~target:"test.js");
  ]
