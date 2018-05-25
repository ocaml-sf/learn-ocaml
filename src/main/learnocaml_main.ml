(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2018 OCamlPro.
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

open Lwt.Infix

let ( / ) = Filename.concat

let readlink f =
  let cwd = Sys.getcwd () in
  let f =
    try
      Sys.chdir (Filename.dirname f);
      Filename.concat (Sys.getcwd ()) (Filename.basename f)
    with Sys_error _ -> f
  in
  try Sys.chdir cwd; f
  with Sys_error _ -> Sys.chdir (Filename.get_temp_dir_name ()); f

module Args = struct
  open Cmdliner
  open Arg

  type command = Grade | Build | Serve

  let commands =
    value & pos_all (Arg.enum [
        "grade", Grade;
        "build", Build;
        "serve", Serve;
      ]) [Build; Serve] &
    info [] ~docs:"COMMANDS" ~doc:
      "One or several of $(b,grade), $(b,build) and $(b,serve). If \
       unspecified, defaults to build and serve"

  let repo_dir =
    value & opt dir "." & info ["repo"] ~docv:"DIR" ~doc:
      "The path to the repository containing the exercises, lessons and \
       tutorials."

  let app_dir =
    value & opt string "./www" & info ["app-dir"; "o"] ~docv:"DIR" ~doc:
      "Directory where the app should be generated for the $(i,build) command, \
       and from where it is served by the $(i,serve) command."

  module Grader = struct
    let info = info ~docs:"GRADER OPTIONS"

    let exercises =
      value & opt_all (list dir) [["."]] & info ["exercises";"e"] ~docv:"DIRS" ~doc:
        "Directories where to find the exercises to be graded \
         (comma-separated). Can be repeated."

    let output_json =
      value & opt (some dir) None & info ["output-json"] ~docv:"DIR" ~doc:
        "save the graded exercise in JSON format in the given file"

    let grade_student =
      value & opt (some file) None & info ["grade-student"] ~docv:"FILE" ~doc:
        "grade the given student file instead of 'solution.ml'"

    let display_outcomes =
      value & flag & info ["display-outcomes"] ~doc:
        "display the toplevel's outcomes"

    let display_callback =
      value & flag & info ["display-progression"] ~doc:
        "display grading progression messages"

    let display_std_outputs =
      value & flag & info ["display-stdouts"] ~doc:
        "display the toplevel's standard outputs"

    let dump_outputs =
      value & opt (some string) None & info ["dump-outputs"] ~docv:"PREFIX" ~doc:
        "save the outputs in files with the given prefix"

    let dump_reports =
      value & opt (some string) None & info ["dump-reports"] ~docv:"PREFIX" ~doc:
        "save the reports in files with the given prefix"

    type t = {
      exercises: string list;
      output_json: string option;
    }

    let term =
      let apply
          repo_dir exercises
          output_json grade_student display_outcomes display_callback
          display_std_outputs dump_outputs dump_reports =
        let exercises = List.flatten exercises in
        Grader_cli.output_json := output_json;
        Grader_cli.grade_student := grade_student;
        Grader_cli.display_outcomes := display_outcomes;
        Grader_cli.display_callback := display_callback;
        Grader_cli.display_std_outputs := display_std_outputs;
        Grader_cli.dump_outputs := dump_outputs;
        Grader_cli.dump_reports := dump_reports;
        Learnocaml_process_exercise_repository.dump_outputs := dump_outputs;
        Learnocaml_process_exercise_repository.dump_reports := dump_reports;
        { exercises; output_json }
      in
      Term.(const apply $repo_dir
            $exercises $output_json $grade_student $display_outcomes
            $display_callback $display_std_outputs $dump_outputs $dump_reports)
  end

  module Builder = struct
    let info = info ~docs:"BUILDER OPTIONS"

    let contents_dir =
      let default =
        readlink (Filename.dirname (Filename.dirname (Sys.argv.(0)))
                  /"share"/"learn-ocaml"/"www")
      in
      value & opt dir default & info ["contents-dir"] ~docv:"DIR" ~doc:
        "directory containing the base learn-ocaml app contents"

    type t = {
      contents_dir: string;
    }

    let term =
      let apply app_dir repo_dir contents_dir =
        Learnocaml_process_exercise_repository.exercises_dir :=
          repo_dir/"exercises";
        Learnocaml_process_tutorial_repository.tutorials_dir := 
          repo_dir/"tutorials";
        { contents_dir }
      in
      Term.(const apply $app_dir $repo_dir $contents_dir)

  end

  module Server = struct
    let info = info ~docs:"SERVER OPTIONS"

    let sync_dir =
      value & opt string "./sync" & info ["sync-dir"] ~docv:"DIR" ~doc:
        "Directory where to store user sync tokens."

    let port =
      value & opt int 8080 & info ["port";"p"] ~docv:"PORT" ~doc:
        "The TCP port on which to run the server"

    type t = {
      sync_dir: string;
      port: int;
    }

    let term =
      let apply app_dir sync_dir port =
        Learnocaml_simple_server.static_dir := app_dir;
        Learnocaml_simple_server.sync_dir := sync_dir;
        Learnocaml_simple_server.port := port;
        { sync_dir; port }
      in
      Term.(const apply $app_dir $sync_dir $port)
  end

  type t = {
    commands: command list;
    app_dir: string;
    repo_dir: string;
    grader: Grader.t;
    builder: Builder.t;
    server: Server.t;
  }

  let term =
    let apply commands app_dir repo_dir grader builder server =
      { commands; app_dir; repo_dir; grader; builder; server }
    in
    Term.(const apply $commands $app_dir $repo_dir
          $Grader.term $Builder.term $Server.term)
end

let copy_tree src dst =
  Lwt.catch (fun () ->
      Lwt_unix.file_exists dst >>= (function
          | true -> Lwt.return_unit
          | false -> Lwt_unix.mkdir dst 0o755) >>= fun () ->
      let cmd =
        Array.concat
          [[|"cp"; "-PR"|];
           Array.map (Filename.concat src) (Sys.readdir src);
           [|dst|]]
      in
      Lwt_process.exec ("", cmd) >>= fun r ->
      if r <> Unix.WEXITED 0 then Lwt.fail_with "copy_tree"
      else Lwt.return_unit)
    (function
      | Sys_error _ | Unix.Unix_error _ -> Lwt.fail_with "copy_tree"
      | e -> raise e)

open Args

let main o =
  let grade () =
    if List.mem Grade o.commands then
      (if List.mem Build o.commands || List.mem Serve o.commands then
         failwith "The 'grade' command is incompatible with 'build' and \
                   'serve'";
       Lwt_list.fold_left_s (fun i ex ->
           Grader_cli.grade ex o.grader.Grader.output_json >|= max i)
         0 o.grader.Grader.exercises
       >|= fun i -> Some i)
    else Lwt.return None
  in
  let generate () =
    if List.mem Build o.commands then
      (Printf.printf "Updating app at %s\n%!" o.app_dir;
       Lwt.catch
         (fun () -> copy_tree o.builder.Builder.contents_dir o.app_dir)
         (function
           | Failure _ ->
               Lwt.fail_with @@ Printf.sprintf
                 "Failed to copy base app contents from %s"
                 (readlink o.builder.Builder.contents_dir)
           | e -> Lwt.fail e)
       >>= fun () ->
       Lwt.catch
         (fun () -> copy_tree (o.repo_dir/"lessons") o.app_dir)
         (function Failure _ -> Lwt.return_unit
                 | e -> Lwt.fail e)
       >>= fun () ->
       Learnocaml_process_tutorial_repository.main o.app_dir >>= fun e_ret ->
       Learnocaml_process_exercise_repository.main o.app_dir >>= fun t_ret ->
       Lwt.return (e_ret && t_ret))
    else
      Lwt.return true
  in
  let run_server () =
    if List.mem Serve o.commands then
      (Printf.printf "Starting server on port %d\n%!"
         !Learnocaml_simple_server.port;
       Learnocaml_simple_server.launch ())
    else
      Lwt.return true
  in
  let ret =
    Lwt_main.run
      (grade () >>= function
        | Some i -> Lwt.return i
        | None ->
            generate () >>= fun success ->
            if success then
              run_server () >>= fun r ->
              if r then Lwt.return 0 else Lwt.return 10
            else
              Lwt.return 1)
  in
  exit ret

let man = [
  `S "DESCRIPTION";
  `P "This program performs various tasks related to generating, serving and \
      administrating a learn-ocaml web-app.";
  `S "COMMANDS";
  `I ("$(b,grade)", "Runs the automatic grader on exercise solutions.");
  `I ("$(b,build)", "Generates the application based on a repository \
                     containing the lessons, tutorials and exercises (see \
                     $(b,REPOSITORY FORMAT)).");
  `I ("$(b,serve)", "Run a web-server providing access to the learn-ocaml app, \
                     as well as user file synchronisation.");
  `S "OPTIONS";
  `S "GRADER OPTIONS";
  `S "BUILDER OPTIONS";
  `S "SERVER OPTIONS";
  `S "REPOSITORY FORMAT";
  `P "The repository specified by $(b,--repo) is expected to contain \
      sub-directories $(b,lessons), $(b,tutorials) and $(b,exercises).";
  `S "AUTHORS";
  `P "Learn OCaml is written by OCamlPro. Its main authors are Benjamin Canou, \
      Çağdaş Bozman and Grégoire Henry. It is licensed under the GNU Affero \
      General Public License version 3: any instance of the app must provide \
      its source code to its users.";
  `S "BUGS";
  `P "Bugs should be reported to \
      $(i,https://github.com/ocaml-sf/learn-ocaml/issues)";
]

let main_cmd =
  Cmdliner.Term.(const main $ Args.term),
  Cmdliner.Term.info
    ~man
    ~doc:"Learn-ocaml web-app manager"
    "learn-ocaml"

let () =
  match
    Cmdliner.Term.eval ~catch:false main_cmd
  with
  | exception Failure msg ->
      Printf.eprintf "[ERROR] %s\n" msg;
      exit 1
  | `Error _ -> exit 2
  | _ -> exit 0
