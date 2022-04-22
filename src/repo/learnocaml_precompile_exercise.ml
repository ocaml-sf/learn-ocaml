(* Compile objects from an exercise *)

open Lwt.Infix

(* FIXME: make these configurable *)
let grading_cmis_dir =
  let prefix = Filename.dirname (Filename.dirname (Sys.executable_name)) in
  let ( / ) = Filename.concat in
  ref (prefix/"lib"/"learn-ocaml"/"test_lib")

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

let ocamlc ?(dir=Sys.getcwd ()) ?(opn=[]) ?(ppx=[]) ~source ~target args =
  let d = Filename.concat dir in
  if is_fresh ~dir target source then Lwt.return_unit else
  let args =
    List.fold_right (fun ppx args ->
        "-ppx" :: Filename.concat !grading_cmis_dir (ppx^" --as-ppx") :: args)
      ppx args
  in
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

let read_lines fopen =
  try
    let ic = fopen () in
    let lines = ref [] in
    try while true do lines := input_line ic :: !lines done; []
    with End_of_file ->
      close_in ic;
      List.rev !lines
  with Sys_error _ -> []

let precompile ~exercise_dir =
  let dir = exercise_dir in
  let grader_libs =
    read_lines (fun () -> open_in (Filename.concat dir "test_libs.txt")) in
  let grader_flags =
    List.fold_right (fun lib flags ->
        let libflags =
          read_lines (fun () ->
              Printf.ksprintf Unix.open_process_in
                "ocamlfind query %s -predicates byte -format \"-I&%%d&%%a\"" lib)
          |> List.map (String.split_on_char '&')
          |> List.flatten
        in
        List.append libflags flags)
      grader_libs []
  in
  ocamlc ~dir ["-c"] ~opn:["Learnocaml_callback"]
    ~source:["prelude.ml"] ~target:"prelude.cmo"
  >>= fun () ->
  ocamlc ~dir ["-c"] ~opn:["Learnocaml_callback"; "Prelude"] ~ppx:["exercise-ppx"]
    ~source:["prepare.ml"] ~target:"prepare.cmo"
  >>= fun () ->
  ocamlc ~dir ["-c"] ~opn:["Learnocaml_callback"; "Prelude"; "Prepare"] ~ppx:["exercise-ppx"]
    ~source:["solution.ml"] ~target:"solution.cmo"
  >>= fun () ->
  Lwt.join [
    (ocamlc ~dir ["-a"]
       ~source:["prelude.cmo"; "prepare.cmo"; "solution.cmo"]
       ~target:"exercise.cma"
     >>= fun () ->
     jsoo ~dir [] ~source:"exercise.cma" ~target:"exercise.js");
    (ocamlc ~dir (["-c"; "-I"; "+compiler-libs"] @ grader_flags)
       ~ppx:["grader-ppx"]
       ~opn:["Learnocaml_callback"; "Prelude"; "Prepare"; "Test_lib.Open_me"]
       ~source:["test.ml"]
       ~target:"test.cmo"
     >>= fun () ->
     ocamlc ~dir (["-a"] @ grader_flags)
       ~source:["test.cmo"]
       ~target:"test.cma"
     >>= fun () ->
     jsoo ~dir [] ~source:"test.cma" ~target:"test.js");
  ]
