(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

exception Internal_error of string * Toploop_ext.error
exception User_code_error of Toploop_ext.error
exception Invalid_grader

let string_of_exn = function
  | Internal_error (msg, error) ->
      let msg =
        Format.asprintf [%if"Exercise definition error %s:\n%a\n%!"]
          msg Location.print_report (Toploop_results.to_error error)
      in
      Some  msg
  | User_code_error error ->
      let msg =
        Format.asprintf [%if"Error in user code:\n\n%a\n%!"]
          Location.print_report (Toploop_results.to_error error)
      in
      Some msg
  | _ -> None

let () =
  Location.register_error_of_exn (fun exn ->
      match string_of_exn exn with
      | Some msg -> Some (Location.error msg)
      | None -> None)


let internal_error name err =
  raise (Internal_error (name, err))

let user_code_error err =
  raise (User_code_error err)

let get_grade
    ?callback ?timeout ?(dirname="") ~divert
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
      handle_error (internal_error [%i"while preparing the tests"]) @@
      Toploop_ext.use_string ~print_outcome ~ppf_answer
        {|let print_html _ = assert false|};

      set_progress [%i"Loading the prelude."] ;
      handle_error (internal_error [%i"while loading the prelude"]) @@
      Toploop_ext.use_string ~print_outcome ~ppf_answer ~filename:(file "prelude.ml")
        (Learnocaml_exercise.(decipher false File.prelude exo)) ;

      set_progress [%i"Preparing the test environment."] ;
      handle_error (internal_error [%i"while preparing the tests"]) @@
      Toploop_ext.use_string ~print_outcome ~ppf_answer ~filename:(file "prepare.ml")
        (Learnocaml_exercise.(decipher false File.prepare exo)) ;

      set_progress [%i"Loading your code."] ;
      handle_error user_code_error @@
      Toploop_ext.use_mod_string ~print_outcome ~ppf_answer ~modname:"Code"
        ~filename:(file "solution.ml") code ;

      set_progress [%i"Loading the solution."] ;
      handle_error (internal_error [%i"while loading the solution"]) @@
      Toploop_ext.use_mod_string ~print_outcome ~ppf_answer ~modname:"Solution"
        (Learnocaml_exercise.(decipher false File.solution exo)) ;

      set_progress [%i"Preparing to launch the tests."] ;
      Introspection.allow_introspection ~divert ;
      Introspection.insert_mod_ast_in_env ~var_name: "code_ast" code ;
      let get_result =
        Introspection.create_ref "results"
          [%ty: Learnocaml_report.t option]
          None in
      Introspection.register_callback "set_progress"
        [%ty: string]
        set_progress ;
      Introspection.insert_in_env "timeout" [%ty: int option] timeout ;
      handle_error (internal_error [%i"while preparing the tests"]) @@
      Toploop_ext.use_string ~print_outcome ~ppf_answer
        "module Test_lib = Test_lib.Make(struct\n\
        \  let results = results\n\
        \  let set_progress = set_progress\n\
        \  let timeout = timeout\n\
        \  module Introspection = Introspection\n\
         end)" ;
      handle_error (internal_error [%i"while preparing the tests"]) @@
      Toploop_ext.use_string ~print_outcome ~ppf_answer
        "module Report = Learnocaml_report" ;
      (* The following 3 lines are just a workaround for issue #457 *)
      handle_error (internal_error [%i"while preparing the tests"]) @@
      Toploop_ext.use_string ~print_outcome ~ppf_answer
        "module Introspection = Introspection" ;
      set_progress [%i"Launching the test bench."] ;

      let () =
        let open Learnocaml_exercise in
        let files = File.dependencies (access false File.depend exo) in
        let rec load_dependencies signatures = function
        | [] -> () (* signatures without implementation are ignored *)
        | file::fs ->
          let path = File.key file
          and content = decipher false file exo in
          let modname = String.capitalize_ascii @@
                        Filename.remove_extension @@ Filename.basename path in
          match Filename.extension path with
          | ".mli" -> load_dependencies ((modname,content) :: signatures) fs
          | ".ml" -> 
            let included,content = 
              (* the first line of an .ml file can contain an annotation       *)
              (* [@@@included] which denotes that this file has to be included *)
              (* directly in the toplevel environment, and not in an module.   *)
              match String.index_opt content '\n' with
              | None -> (false,content)
              | Some i -> 
                (match String.trim (String.sub content 0 i) with 
                 | "[@@@included]" -> 
                    let content' = String.sub content i @@ 
                                   (String.length content - i)
                    in (true,content')
                 | _ -> (false,content))
            in
            (handle_error (internal_error [%i"while loading user dependencies"]) @@
             match included with
             | true -> Toploop_ext.use_string ~print_outcome ~ppf_answer 
                                  ~filename:(Filename.basename path) content 
             | false ->
               let use_mod = 
                 Toploop_ext.use_mod_string ~print_outcome ~ppf_answer ~modname in
               match List.assoc_opt modname signatures with 
               | Some sig_code -> use_mod ~sig_code content
               | None -> use_mod content); 
               load_dependencies signatures fs
          | _ -> failwith ("uninterpreted dependency \"" ^ path ^
                           "\", file extension expected : .ml or .mli") in 
          load_dependencies [] files
      in

      handle_error (internal_error [%i"while testing your solution"]) @@
      Toploop_ext.use_string ~print_outcome ~ppf_answer ~filename:(file "test.ml")
        (Learnocaml_exercise.(decipher false File.test exo)) ;

      (* Memory cleanup... *)
      Toploop.initialize_toplevel_env () ;
      (* TODO: Also clear the object table, once the OCaml's Toploop allows to. *)
      !flush_stderr () ;
      !flush_stdout () ;
      match get_result () with
      | Some report -> Ok report
      | None -> Error Invalid_grader
    with exn ->
      Error exn in
  Format.fprintf ppf_answer "@." ;
  (result,
   Buffer.contents stdout_buffer,
   Buffer.contents stderr_buffer,
   Buffer.contents outcomes_buffer)
