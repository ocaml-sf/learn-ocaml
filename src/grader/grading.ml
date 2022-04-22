(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2022 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(* Define a non-extensible type to allow marshalling *)
type error =
  | Internal_error of string * Toploop_ext.error
  | User_code_error of Toploop_ext.error
  | Invalid_grader

exception Grading_error of error

let string_of_err = function
  | Internal_error (msg, error) ->
      Format.asprintf [%if"Exercise definition error %s:\n%a\n%!"]
        msg Location.print_report (Toploop_results.to_error error)
  | User_code_error error ->
      Format.asprintf [%if"Error in user code:\n\n%a\n%!"]
        Location.print_report (Toploop_results.to_error error)
  | Invalid_grader ->
      [%i"The grader is invalid"]

let () =
  Location.register_error_of_exn (function
      | Grading_error e -> Some (Location.error (string_of_err e))
      | _ -> None)

let internal_error name err =
  raise (Grading_error (Internal_error (name, err)))

let user_code_error err =
  raise (Grading_error (User_code_error err))

let get_grade
    ?callback ?timeout ?(dirname="") ~divert ~load_code
    (exo : Learnocaml_exercise.t) code =

  let file f = String.concat Filename.dir_sep [dirname; f] in

  let print_outcome = true in
  let outcomes_buffer = Buffer.create 503 in
  let ppf_answer =
    Format.formatter_of_buffer outcomes_buffer in

  let stderr_buffer = Buffer.create 503 in
  let stdout_buffer = Buffer.create 503 in
  let flush_stderr = ref
      (divert "ERR" stderr (Buffer.add_string stderr_buffer)) in
  let flush_stdout = ref
      (divert "OUT" stdout (Buffer.add_string stdout_buffer)) in

  let callback =
    match callback with
    | None -> None
    | Some callback ->
        Some (fun msg ->
            !flush_stderr () ;
            !flush_stdout () ;
            callback msg ;
            flush_stderr :=
              divert "ERR" stderr (Buffer.add_string stderr_buffer) ;
            flush_stdout :=
              divert "OUT" stdout (Buffer.add_string stdout_buffer)) in

  let set_progress =
    match callback with
    | None -> (fun _ -> ())
    | Some set_progress -> set_progress in

  let handle_error ?(warn = fun _ -> ()) fail =
    function
    | Toploop_ext.Ok (s, w) ->
        warn w ;
        if not s then begin
          !flush_stderr () ;
          !flush_stdout () ;
          let msg =
            String.concat "\n"
              (List.map Buffer.contents [stderr_buffer; stdout_buffer; outcomes_buffer])
          in fail ((Location.none, msg), [])
        end
    | Toploop_ext.Error (err, w) ->
        warn w ;
        !flush_stderr () ;
        !flush_stdout () ;
        fail err in

  let result = try
      let saved_toplevel_state = Symtable.current_state () in
      let () =
        (* Prelude/Prepare might use these callbacks, but they shouldn't appear
           in the solutions: provide dummy implementations here *)
        Toploop_ext.load_cmi_from_string
          OCamlRes.(Res.find (Path.of_string "learnocaml_callback.cmi") Embedded_grading_lib.root) ;
        let module Learnocaml_callback: Learnocaml_internal_intf.CALLBACKS = struct
          let print_html s = output_string stdout s
          let print_svg s = output_string stdout s
        end in
        Toploop_ext.inject_global "Learnocaml_callback"
          (Obj.repr (module Learnocaml_callback: Learnocaml_internal_intf.CALLBACKS));
      in
      let () =
        let module Learnocaml_internal: Learnocaml_internal_intf.INTERNAL = struct
          let install_printer = Toploop_ext.install_printer
          exception Undefined
        end in
        Toploop_ext.inject_global "Learnocaml_internal"
          (Obj.repr (module Learnocaml_internal: Learnocaml_internal_intf.INTERNAL))
      in

      set_progress [%i"Preparing the test environment."] ;
      Toploop_ext.load_cmi_from_string (Learnocaml_exercise.(decipher File.prelude_cmi exo)) ;
      Toploop_ext.load_cmi_from_string (Learnocaml_exercise.(decipher File.prepare_cmi exo)) ;

      handle_error (internal_error [%i"while preparing the tests"]) @@
      load_code Learnocaml_exercise.{
          cma = decipher File.exercise_cma exo ;
          js = decipher File.exercise_js exo ;
        };

      handle_error (internal_error [%i"while preparing the tests"]) @@
      Toploop_ext.use_string ~print_outcome ~ppf_answer
        {|include Prelude|};
      handle_error (internal_error [%i"while preparing the tests"]) @@
      Toploop_ext.use_string ~print_outcome:false ~ppf_answer
        {|module Prelude = struct end|};
      handle_error (internal_error [%i"while preparing the tests"]) @@
      Toploop_ext.use_string ~print_outcome:false ~ppf_answer
        {|include Prepare|};
      handle_error (internal_error [%i"while preparing the tests"]) @@
      Toploop_ext.use_string ~print_outcome:false ~ppf_answer
        {|module Prepare = struct end|};

      set_progress [%i"Loading your code."] ;
      handle_error user_code_error @@
      Toploop_ext.use_mod_string ~print_outcome ~ppf_answer ~modname:"Code"
        ~filename:(file "solution.ml") code ;

      Toploop_ext.load_cmi_from_string (Learnocaml_exercise.(decipher File.solution_cmi exo)) ;

      set_progress [%i"Preparing to launch the tests."] ;
      let module Intro_inner =
        (val Introspection.allow_introspection ~divert)
      in
      let code_ast = Introspection.get_mod_ast ~var_name:"code_ast" code in
      let results: Learnocaml_report.t option ref = ref None in
      let get_result () = !results in
      let () =
        let module Pre_test: Introspection_intf.PRE_TEST = struct
          module Introspection = Intro_inner
          let code_ast = code_ast
          let results = results
          let set_progress = set_progress
          let timeout = timeout
        end in
        (* Hack: register Pre_test as a compilation unit usable by the compiled
           modules loaded later-on *)
        Toploop_ext.inject_global "Pre_test"
          (Obj.repr (module Pre_test: Introspection_intf.PRE_TEST));
      in
      Toploop_ext.load_cmi_from_string
        OCamlRes.(Res.find (Path.of_string "test_lib.cmi")
                    Embedded_grading_lib.root) ;
      handle_error (internal_error [%i"while preparing the tests"]) @@
      load_code
        { Learnocaml_exercise.
          cma = OCamlRes.(Res.find (Path.of_string "testing_dyn.cma")
                            Embedded_grading_lib.root) ;
          js = OCamlRes.(Res.find (Path.of_string "testing_dyn.js")
                           Embedded_grading_lib.root) };
      handle_error (internal_error [%i"while preparing the tests"]) @@
      Toploop_ext.use_string ~print_outcome:false ~ppf_answer {|open! Test_lib.Open_me|};
      (* Registering the samplers that may be defined in [test.ml] requires
         having their types and the definitions of the types they sample, hence
         the need for an opened [test_cmi]*)
      Toploop_ext.load_cmi_from_string (Learnocaml_exercise.(decipher File.test_cmi exo)) ;
      handle_error (internal_error [%i"while preparing the tests"]) @@
      Toploop_ext.use_string ~print_outcome:false ~ppf_answer {|open! Test|};
      handle_error (internal_error [%i"while testing your solution"]) @@
      load_code Learnocaml_exercise.{
          cma = decipher File.test_cma exo ;
          js = decipher File.test_js exo ;
        };

      (* Memory cleanup... *)
      Toploop.initialize_toplevel_env () ;
      Symtable.restore_state saved_toplevel_state;
      (* TODO: Also clear the object table, once the OCaml's Toploop allows to.
         Toploop.toplevel_value_bindings := String.Map.empty; (* not exported :( *)
         here we run in a forked sub-process then exit as a workaround *)
      !flush_stderr () ;
      !flush_stdout () ;
      match get_result () with
      | Some report -> Ok report
      | None -> Error Invalid_grader
    with
    | Grading_error err -> Error err
    | e -> Error (Internal_error (Printexc.to_string e,
                                  ((Location.none, ""),[])))
 in
  Format.fprintf ppf_answer "@." ;
  (result,
   Buffer.contents stdout_buffer,
   Buffer.contents stderr_buffer,
   Buffer.contents outcomes_buffer)
