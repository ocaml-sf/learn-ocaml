(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2016 OCamlPro.
 *
 * Learn-OCaml is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Learn-OCaml is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

exception Internal_error of string * Toploop_ext.error
exception User_code_error of Toploop_ext.error
exception Invalid_grader

let () =
  Location.register_error_of_exn
    (function
      | Internal_error (msg, error) ->
          let msg =
            Printf.sprintf "Internal error %s:\n\n%s\n%!"
              msg error.Toploop_ext.msg in
          Some {Location.loc = Location.none ; sub = [] ;
                msg ; if_highlight = msg }
      | User_code_error error ->
          let msg =
            Printf.sprintf "Error in user code:\n\n%s\n%!" error.Toploop_ext.msg in
          Some {Location.loc = Location.none ; sub = [] ;
                msg ; if_highlight = msg }
      | _ -> None)


let internal_error name err =
  raise (Internal_error (name, err))

let user_code_error err =
  raise (User_code_error err)

let get_grade ?callback ~divert exo code =

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
            Buffer.contents stderr_buffer ^
            "\n" ^
            Buffer.contents stdout_buffer in
          fail { Toploop_ext.msg ; locs = [] ; if_highlight = msg }
        end
    | Toploop_ext.Error (err, w) ->
        warn w ;
        !flush_stderr () ;
        !flush_stdout () ;
        fail err in

  let result = try
      handle_error (internal_error "while preparing the tests") @@
      Toploop_ext.use_string ~print_outcome ~ppf_answer
        {|let print_html _ = assert false|};

      set_progress "Loading the prelude." ;
      handle_error (internal_error "while loading the prelude") @@
      Toploop_ext.use_string ~print_outcome ~ppf_answer ~filename:"prelude.ml"
        (Exercise.(get prelude) exo) ;

      set_progress "Preparing the test environment." ;
      handle_error (internal_error "while preparing the tests") @@
      Toploop_ext.use_string ~print_outcome ~ppf_answer ~filename:"prepare.ml"
        (Exercise.(get prepare) exo) ;

      set_progress "Loading your code." ;
      handle_error user_code_error @@
      Toploop_ext.use_mod_string ~print_outcome ~ppf_answer ~modname:"Code" code ;

      set_progress "Loading the solution." ;
      handle_error (internal_error "while loading the solution") @@
      Toploop_ext.use_mod_string ~print_outcome ~ppf_answer ~modname:"Solution"
        (Exercise.(get solution) exo) ;

      set_progress "Preparing to launch the tests." ;
      Introspection.allow_introspection ~divert ;
      Introspection.insert_mod_ast_in_env ~var_name: "code_ast" code ;
      let get_result =
        Introspection.create_ref "results"
          [%ty: Report.report option]
          None in
      Introspection.register_callback "set_progress"
        [%ty: string]
        set_progress ;
      handle_error (internal_error "while preparing the tests") @@
      Toploop_ext.use_string ~print_outcome ~ppf_answer
        "module Test_lib = Test_lib.Make(struct\n\
        \  let results = results\n\
        \  let set_progress = set_progress\n\
        \  module Introspection = Introspection\n\
         end)" ;

      set_progress "Launching the test bench." ;
      handle_error (internal_error "while testing your solution") @@
      Toploop_ext.use_string ~print_outcome ~ppf_answer ~filename:"test.ml"
        (Exercise.(get test) exo) ;

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
