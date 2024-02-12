(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2023 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Lwt.Infix
open Cmdliner

module StringSet = Set.Make(String)

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

let absolute_filename path =
  (* Note: symlinks are not taken into account *)
  if Filename.is_relative path
  then Filename.concat (Sys.getcwd ()) path
  else path

let dflt_build_dir = "_learn-ocaml-build"

module Args = struct
  open Arg

  type command = Grade | Build | Serve

  let commands =
    value & pos_all (Arg.enum [
        "grade", Grade;
        "build", Build;
        "serve", Serve;
      ]) [Build; Serve] &
    info [] ~docs:"COMMANDS" ~docv:"COMMAND"

  let docs = Manpage.s_common_options

  let repo_dir =
    value & opt dir "." & info ["repo"] ~docs ~docv:"DIR" ~doc:
      "The path to the repository containing the exercises, lessons and \
       tutorials."

  let build_dir =
    value & opt dir ("./" ^ dflt_build_dir) & info ["build-dir"] ~docs ~docv:"DIR" ~doc:
    (Printf.sprintf
       "Directory where the repo exercises are copied and precompiled. \
        When $(docv) takes its default value (e.g. when it is omitted in CLI), \
        '$(b,learn-ocaml build)' first erases the '$(docv)/exercises' subfolder. \
        Note that the default value for $(docv), './%s', is generally a sensible choice. \
        But passing the same argument as the one for $(i,--repo) is also a valid value for $(docv)."
     dflt_build_dir)

  let app_dir =
    value & opt string "./www" & info ["app-dir"; "o"] ~docs ~docv:"DIR" ~doc:
      "Directory where the app should be generated for the $(i,build) command, \
       and from where it is served by the $(i,serve) command."

  let base_url =
    value & opt string "" &
      info ["base-url"] ~docs ~docv:"BASE_URL" ~env:(Cmd.Env.info "LEARNOCAML_BASE_URL") ~doc:
        "Set the base URL of the website. \
         Should not end with a trailing slash. \
         Currently, this has no effect on the backend - '$(b,learn-ocaml serve)'. \
         Mandatory for '$(b,learn-ocaml build)' if the site is not hosted in path '/', \
         which typically occurs for static deployment."

  module Grader = struct
    let info = info ~docs:"GRADER OPTIONS"

    let exercises =
      value & opt_all (list dir) [["."]] & info ["exercises";"e"] ~docv:"DIRS" ~doc:
        "Directories where to find the exercises to be graded \
         (comma-separated). Can be repeated."

    let output_json =
      value & opt (some dir) None & info ["output-json"] ~docv:"DIR" ~doc:
        "save the processed exercises in JSON format in `.json` files, \
         in the given directory."

    let grade_student =
      value & opt (some file) None & info ["grade-student";"s"] ~docv:"FILE" ~doc:
        "grade the given student file instead of 'solution.ml'"

    let display_outcomes =
      value & flag & info ["display-outcomes"] ~doc:
        "display the toplevel's outcomes"

    let quiet =
      value & flag & info ["quiet";"q"] ~doc:
        "Don't display grading progression messages"

    let display_std_outputs =
      value & flag & info ["display-stdouts"] ~doc:
        "display the toplevel's standard outputs"

    let dump_outputs =
      value & opt (some string) None & info ["dump-outputs"] ~docv:"PREFIX" ~doc:
        "save the outputs in files with the given prefix"

    let dump_reports =
      value & opt (some string) None & info ["dump-reports"] ~docv:"PREFIX" ~doc:
        "save the reports in files with the given prefix"

    let timeout =
      value & opt (some int) None & info ["timeout"] ~docv:"SECONDS" ~doc:
        "Limit every test to the given timeout"

    let verbose =
      value & flag & info ["verbose"; "v"] ~doc:
        "Display detailed grading reports to stdout"

    let dump_dot =
      value & opt (some string) None & info ["dump-dot"] ~doc:
        "Generates a dependency graph of the repository and dumps it into the \
         given file"

    type t = {
      exercises: string list;
      output_json: string option;
      display_callback: bool;
      dump_outputs: string option;
      dump_reports: string option;
    }

    let grader_conf =
      let apply exercises output_json quiet dump_outputs dump_reports =
        let exercises = List.flatten exercises in
        { exercises; output_json; display_callback = not quiet;
          dump_outputs; dump_reports }
      in
      Term.(const apply $exercises $output_json $quiet $dump_outputs $dump_reports)

    let grader_cli =
      let apply
          grade_student display_outcomes display_std_outputs
          timeout verbose dump_dot
        =
        Grader_cli.grade_student := grade_student;
        Grader_cli.display_outcomes := display_outcomes;
        Grader_cli.display_std_outputs := display_std_outputs;
        Grader_cli.individual_timeout := timeout;
        Grader_cli.display_reports := verbose;
        Grader_cli.dump_dot := dump_dot;
        ()
      in
      Term.(const apply $grade_student $display_outcomes $display_std_outputs
            $timeout $verbose $dump_dot)

    let term =
      let apply conf () =
        Learnocaml_process_exercise_repository.dump_outputs := conf.dump_outputs;
        Learnocaml_process_exercise_repository.dump_reports := conf.dump_reports;
        conf in
      Term.(const apply $grader_conf $grader_cli)
  end

  module Builder = struct
    let info = info ~docs:"BUILDER OPTIONS"

    let contents_dir =
      let default =
        readlink (Filename.dirname (Filename.dirname (Sys.executable_name))
                  /"share"/"learn-ocaml"/"www")
      in
      value & opt dir default & info ["contents-dir"] ~docv:"DIR" ~doc:
        "directory containing the base learn-ocaml app contents"

    (** [enable "opt" ~old["old";"very old"](*backward-compat opts*) "the.."] *)
    let enable opt ?(old: string list = []) doc =
      let f_on_off opt = ("enable-"^opt, "disable-"^opt) in
      let on_opt, off_opt =
        List.split (List.map f_on_off (opt :: old)) in
      value & vflag None [
        Some true, info on_opt ~doc:("Enable "^doc);
        Some false, info off_opt ~doc:("Disable "^doc);
      ]

    let try_ocaml = enable "tutorials" ~old:["tryocaml"]
        "the 'Tutorials' tab (enabled by default if the repository contains a \
         $(i,tutorials) directory)"

    let playground = enable "playground"
        "the 'Playground' tab (enabled by default if the repository contains a \
         $(i,playground) directory)"

    let lessons = enable "lessons"
        "the 'Lessons' tab (enabled by default if the repository contains a \
         $(i,lessons) directory)"

    let exercises = enable "exercises"
        "the 'Exercises' tab (enabled by default if the repository contains an \
         $(i,exercises) directory)"

    let toplevel = enable "toplevel"
        "the 'Toplevel' tab (enabled by default)"

    let exercises_filtered =
      value & opt_all (list string) [[]] & info ["exercises-filtered"; "f"] ~docv:"DIRS" ~doc:
        "Exercises to build (comma-separated), instead of taking \
         the entire repository. Can be repeated."

    let jobs =
      value & opt int 8 & info ["jobs";"j"] ~docv:"INT" ~doc:
        "Number of building jobs to run in parallel"

    type t = {
      contents_dir: string;
      try_ocaml: bool option;
      lessons: bool option;
      exercises: bool option;
      playground: bool option;
      toplevel: bool option;
      base_url: string
    }

    let builder_conf =
      let apply
        contents_dir try_ocaml lessons exercises playground toplevel base_url
        = { contents_dir; try_ocaml; lessons; exercises; playground; toplevel; base_url }
      in
      Term.(const apply $contents_dir $try_ocaml $lessons $exercises $playground $toplevel $base_url)

    let repo_conf =
      let apply repo_dir build_dir exercises_filtered jobs =
        Learnocaml_process_exercise_repository.exercises_dir :=
          (* not repo_dir/"exercises" here - since we need write permissions *)
          build_dir/"exercises";
        Learnocaml_process_exercise_repository.exercises_filtered :=
          Learnocaml_data.SSet.of_list (List.flatten exercises_filtered);
        Learnocaml_process_tutorial_repository.tutorials_dir :=
          repo_dir/"tutorials";
        Learnocaml_process_playground_repository.playground_dir :=
          repo_dir/"playground";
        Learnocaml_process_exercise_repository.n_processes := jobs;
        ()
      in
      Term.(const apply $repo_dir $build_dir $exercises_filtered $jobs)

    let term =
      let apply conf () = conf in
      Term.(const apply $builder_conf $repo_conf)
  end

  module Server = struct
    module Args = Learnocaml_server_args.Args(struct let section = "SERVER OPTIONS" end)
    include Args
  end

  type t = {
    commands: command list;
    app_dir: string;
    repo_dir: string;
    build_dir: string;
    grader: Grader.t;
    builder: Builder.t;
    server: Server.t;
  }

  let term =
    let apply commands app_dir repo_dir build_dir grader builder server =
      { commands; app_dir; repo_dir; build_dir; grader; builder; server }
    in
    Term.(const apply $commands $app_dir $repo_dir $build_dir
          $Grader.term $Builder.term $Server.term app_dir base_url)
end

open Args

let process_html_file orig_file dest_file base_url no_secret =
  let add_prefix_unless_http prefix url =
    let https = "https://" and http = "http://" in
    let starts_with proto =
      String.(sub url 0 (min (length proto) (length url))) = proto in
    if starts_with https || starts_with http then url
    else prefix ^ url in
  let transform_tag e tag attrs attr =
    let attr_pair = ("", attr) in
    match List.assoc_opt attr_pair attrs with
    | Some url -> `Start_element ((e, tag),
                                  (attr_pair, add_prefix_unless_http base_url url)
                                  :: (List.remove_assoc attr_pair attrs))
    | None -> `Start_element ((e, tag), attrs) in
  Lwt_io.open_file ~mode:Lwt_io.Input orig_file >>= fun ofile ->
  Lwt_io.open_file ~mode:Lwt_io.Output dest_file >>= fun wfile ->
  let document = Markup_lwt.lwt_stream (Lwt_io.read_chars ofile) in
  Markup.parse_html document
  |> Markup.signals
  |> Markup.map (function
         | `Start_element ((e, "link"), attrs) -> transform_tag e "link" attrs "href"
         | `Start_element ((e, "script"), attrs) -> transform_tag e "script" attrs "src"
         | `Start_element ((e, "img"), attrs) -> transform_tag e "img" attrs "src"
         | `Start_element ((e, "a"), attrs) -> transform_tag e "a" attrs "href"
         | `Start_element ((e, "div"),attrs)
              when no_secret && List.mem (("", "id"), "secret-section") attrs ->
            `Start_element ((e, "div"), (("", "style"), "display:none")::attrs)
         | t -> t)
  |> Markup.write_html
  |> Markup_lwt.to_lwt_stream
  |> Lwt_io.write_chars wfile >>= fun () ->
  Lwt_io.close ofile >>= fun () ->
  Lwt_io.close wfile

let temp_app_dir o =
  let open Filename in
  concat
    (dirname o.app_dir)
    ((basename o.app_dir) ^ ".temp")

let main o =
  Printf.printf "Learnocaml v.%s running.\n%!" Learnocaml_api.version;
  let grade o =
    if List.mem Grade o.commands then
      (if List.mem Build o.commands || List.mem Serve o.commands then
         failwith "The 'grade' command is incompatible with 'build' and \
                   'serve'";
       Lwt_list.fold_left_s (fun i ex ->
           let json_output = match o.grader.Grader.output_json with
             | None -> None
             | Some o ->
                 Some (Filename.concat o
                         (String.map (function '/' -> '_' | c -> c) ex
                          ^ ".json"))
           in
           Lwt.catch
             (fun () ->
                Grader_cli.grade_from_dir ~print_result:true
                  ~dump_outputs:o.grader.Grader.dump_outputs
                  ~dump_reports:o.grader.Grader.dump_reports
                  ~display_callback:o.grader.Grader.display_callback
                  ex json_output
                >|= function Ok () -> i | Error _ -> 1)
             (fun e ->
                Printf.ksprintf failwith
                  "Could not load exercise at %s: %s" ex (Printexc.to_string e)))
         0 o.grader.Grader.exercises
       >|= fun i -> Some i)
    else Lwt.return_none
  in
  let copy_build_exercises o =
    (* NOTE: if `--build` = `--repo`, then no copy is needed.
       Before checking path equality, we need to get canonical paths *)
    let repo_exos_dir = readlink o.repo_dir / "exercises" in
    let build_exos_dir = readlink o.build_dir / "exercises" in
    if repo_exos_dir <> build_exos_dir then begin
        (* NOTE: if the CLI arg is "./_learn-ocaml-build" or "_learn-ocaml-build"
           then the exercises subdirectory is erased beforehand *)
        begin
          if (o.build_dir = dflt_build_dir || o.build_dir = "./" ^ dflt_build_dir)
             && Sys.file_exists build_exos_dir then
            Lwt.catch (fun () ->
                Lwt_process.exec ("rm",[|"rm";"-rf"; build_exos_dir|]) >>= fun r ->
                if r <> Unix.WEXITED 0 then
                  Lwt.fail_with "Remove command failed"
                else Lwt.return_unit)
              (fun ex ->
                Printf.eprintf
                  "Error: while removing previous build-dir \
                   %s:\n    %s\n%!"
                  build_exos_dir (Printexc.to_string ex);
                exit 1)
          else
            Lwt.return_unit
        end >>= fun () ->
        Printf.printf "Building %s\n%!" (o.build_dir / "exercises");
        (* NOTE: we choose to reuse Lwt_utils.copy_tree,
           even if we could use "rsync" (upside: "--delete-delay",
           but downside: would require the availability of rsync). *)
        Lwt.catch
          (fun () -> Lwt_utils.copy_tree repo_exos_dir build_exos_dir)
          (function
           | Failure _ ->
              Lwt.fail_with @@ Printf.sprintf
                                 "Failed to copy repo exercises to %s"
                                 (build_exos_dir)
           | e -> Lwt.fail e)
      (* NOTE: no chown is needed,
         but we may want to run "chmod -R u+w exercises"
         if the source repository has bad permissions... *)
      end
    else Lwt.return_unit
  in
  let generate o =
    if List.mem Build o.commands then
      (let get_app_dir o =
         if not (List.mem Serve o.commands) then
           Lwt.return o.app_dir
         else if o.server.Server.replace then
           let app_dir = temp_app_dir o in
           (if Sys.file_exists app_dir then
             (Printf.eprintf "Warning: temporary directory %s already exists\n%!"
                app_dir;
              Lwt.return_unit)
            else if Sys.file_exists o.app_dir then
              Lwt_utils.copy_tree o.app_dir app_dir
            else
              Lwt.return_unit)
           >>= fun () -> Lwt.return app_dir
         else if Learnocaml_server.check_running () <> None then
           (Printf.eprintf
              "Error: another server is already running on port %d \
               (consider using option `--replace`)\n%!"
              !Learnocaml_server.port;
            exit 10)
         else Lwt.return o.app_dir
       in
       get_app_dir o >>= fun app_dir ->
       let o = { o with app_dir } in
       Learnocaml_store.static_dir := app_dir;
       Printf.printf "Updating app at %s\n%!" o.app_dir;
       Lwt.catch
         (fun () -> Lwt_utils.copy_tree o.builder.Builder.contents_dir o.app_dir)
         (function
           | Failure _ ->
               Lwt.fail_with @@ Printf.sprintf
                 "Failed to copy base app contents from %s"
                 (readlink o.builder.Builder.contents_dir)
           | e -> Lwt.fail e)
       >>= fun () ->
       let server_config = o.repo_dir/"server_config.json"
       and www_server_config = o.app_dir/"server_config.json" in
       let module ServerData = Learnocaml_data.Server in
       Random.self_init ();
       Lwt.catch
         (fun () ->
           Learnocaml_store.get_from_file ServerData.preconfig_enc server_config)
         (function 
           | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return ServerData.empty_preconfig
           | exn -> Lwt.fail exn) 
       >>= fun preconfig ->
         let json_config = ServerData.build_config preconfig in
         Learnocaml_store.write_to_file ServerData.config_enc json_config www_server_config
       >>= fun () ->
         if o.builder.Builder.base_url <> "" then
           Printf.printf "Base URL: %s\n%!" o.builder.Builder.base_url;
       Lwt_unix.files_of_directory o.builder.Builder.contents_dir
       |> Lwt_stream.iter_s (fun file ->
              let config_secret = json_config.ServerData.secret in
              if Filename.extension file = ".html" then
                  process_html_file (o.builder.Builder.contents_dir/file)
                    (o.app_dir/file) o.builder.Builder.base_url (config_secret = None)
              else
                Lwt.return_unit) >>= fun () ->
       let if_enabled opt dir f = (match opt with
           | None ->
               Lwt.catch (fun () ->
                   Lwt_unix.stat dir >|= fun st -> st.Unix.st_kind = Unix.S_DIR)
                 (function Unix.Unix_error _ -> Lwt.return_false
                         | e -> Lwt.fail e)
           | Some opt -> Lwt.return opt)
         >>= fun enabled ->
         if enabled then f dir >>= Lwt.return_some else Lwt.return_none
       in
       if_enabled o.builder.Builder.lessons (o.repo_dir/"lessons")
         (fun dir ->
            Lwt_utils.copy_tree dir (o.app_dir/"lessons") >>= fun () ->
            Lwt_unix.rename (o.app_dir/"lessons"/"lessons.json") (o.app_dir/"lessons.json")
            >|= fun () -> true)
       >>= fun lessons_ret ->
       if_enabled o.builder.Builder.try_ocaml (o.repo_dir/"tutorials")
         (fun _ -> Learnocaml_process_tutorial_repository.main (o.app_dir))
       >>= fun tutorials_ret ->
       if_enabled o.builder.Builder.playground (o.repo_dir/"playground")
         (fun _ -> Learnocaml_process_playground_repository.main (o.app_dir))
       >>= fun playground_ret ->
       if_enabled o.builder.Builder.exercises (o.repo_dir/"exercises")
         (fun _ ->
           copy_build_exercises o >>= fun () ->
           Learnocaml_process_exercise_repository.main (o.app_dir))
       >>= fun exercises_ret ->
       Lwt_io.with_file ~mode:Lwt_io.Output (o.app_dir/"js"/"learnocaml-config.js")
         (fun oc ->
            Lwt_io.fprintf oc
              "var learnocaml_config = {\n\
              \  enableTutorials: %b,\n\
              \  enablePlayground: %b,\n\
              \  enableLessons: %b,\n\
              \  enableExercises: %b,\n\
              \  enableToplevel: %b,\n\
              \  baseUrl: \"%s\"\n\
               }\n"
              (tutorials_ret <> None)
              (playground_ret <> None)
              (lessons_ret <> None)
              (exercises_ret <> None)
              (o.builder.Builder.toplevel <> Some false)
              o.builder.Builder.base_url >>= fun () ->
       Lwt.return (tutorials_ret <> Some false && exercises_ret <> Some false)))
    else
      Lwt.return true
  in
  let run_server o =
    if List.mem Serve o.commands then
      let () =
        if o.server.Server.replace then
          let running = Learnocaml_server.check_running () in
          Option.iter Learnocaml_server.kill_running running;
          let temp = temp_app_dir o in
          let app_dir = absolute_filename o.app_dir in
          let bak =
            let f =
              Filename.temp_file
                ~temp_dir:(Filename.dirname app_dir)
                (Filename.basename app_dir ^ ".bak.")
                ""
            in
            Unix.unlink f; f
          in
          if Sys.file_exists app_dir then Sys.rename app_dir bak;
          Sys.rename temp o.app_dir;
          Learnocaml_store.static_dir := app_dir;
          if Sys.file_exists bak then
            Lwt.dont_wait (fun () ->
                Lwt.pause () >>= fun () ->
                Lwt_process.exec ("rm",[|"rm";"-rf";bak|]) >>= fun r ->
                if r <> Unix.WEXITED 0 then
                  Lwt.fail_with "Remove command failed"
                else Lwt.return_unit
              )
              (fun ex ->
                 Printf.eprintf
                   "Warning: while cleaning up older application \
                    directory %s:\n    %s\n%!"
                   bak (Printexc.to_string ex))
      in
      let native_server = Sys.executable_name ^ "-server" in
      if Sys.file_exists native_server then
        let server_args =
          let open Server in
          ("--app-dir="^o.app_dir) ::
          ("--sync-dir="^o.server.sync_dir) ::
          ("--base-url="^o.builder.Builder.base_url) ::
          ("--port="^string_of_int o.server.port) ::
          (match o.server.cert with None -> [] | Some c -> ["--cert="^c])
        in
        Lwt.return
          (`Continuation
             (fun () ->
                Unix.execv native_server
                  (Array.of_list (native_server::server_args))))
      else begin
          Printf.printf "Starting server on port %d\n%!"
            !Learnocaml_server.port;
          if o.builder.Builder.base_url <> "" then
            Printf.printf "Base URL: %s\n%!" o.builder.Builder.base_url;
          Learnocaml_server.launch () >>= fun ret ->
          Lwt.return (`Success ret)
        end
    else
      Lwt.return (`Success true)
  in
  let ret =
    Lwt_main.run
      (grade o >>= function
        | Some i -> Lwt.return (`Code i)
        | None ->
            generate o >>= fun success ->
            if success then
              run_server o >>= function
                | `Success true -> Lwt.return (`Code 0)
                | `Success false -> Lwt.return (`Code 10)
                | `Continuation f -> Lwt.return (`Continuation f)
            else
              Lwt.return (`Code 1))
  in
  match ret with
  | `Code n -> exit n
  | `Continuation f -> f ()

let man = 
  let open Manpage in
  [
  `S s_description;
  `P "This program performs various tasks related to generating, serving and \
      administrating a learn-ocaml web-app.";
  `S s_commands;
  `P "The $(i,COMMAND) argument may be one or more of the following. If no \
      command is specified, '$(b,build) $(b,serve)' is assumed.";
  `I ("$(b,grade)", "Runs the automatic grader on exercise solutions.");
  `I ("$(b,build)", "Generates the application based on a repository \
                     containing the lessons, tutorials, playground and exercises (see \
                     $(b,REPOSITORY FORMAT)).");
  `I ("$(b,serve)", "Run a web-server providing access to the learn-ocaml app, \
                     as well as user file synchronisation.");
  `S s_common_options;
  `S "GRADER OPTIONS";
  `S "BUILDER OPTIONS";
  `S "SERVER OPTIONS";
  `S "REPOSITORY FORMAT";
  `P "The repository specified by $(b,--repo) is expected to contain \
      sub-directories $(b,lessons), $(b,tutorials), $(b,playground) and $(b,exercises).";
  `S s_exit_status;
  `S s_environment;
  `S s_authors;
  `P "The original authors are Benjamin Canou, \
      Çağdaş Bozman, Grégoire Henry and Louis Gesbert (OCamlPro). \
      Learn-OCaml is licensed under the MIT License.";
  `S s_bugs;
  `P "Bugs should be reported to \
      $(i,https://github.com/ocaml-sf/learn-ocaml/issues)";
]

let exits =
  let open Cmd.Exit in
  [ info ~doc:"Default exit." ok
  ; info ~doc:"Client-side failure whose cause is printed on stderr. Caused by $(b,grade) or $(b,build) commands." 1
  ; info ~doc:"Uncaught exception." 2
  ; info ~doc:"Server error whose cause is printed on stderr. Caused by $(b,serve) command." 10
  ]

let main_info =
  Cmd.info 
    ~man
    ~exits
    ~doc:"Learn-ocaml web-app manager"
    ~version:Learnocaml_api.version 
    "learn-ocaml"

let main_term = Term.(const main $ Args.term)

let () =
  match
    Cmd.eval_value ~catch:false (Cmd.v main_info main_term)
  with
  | exception Failure msg ->
      Printf.eprintf "[ERROR] %s\n" msg;
      exit 1
  | Error _ -> exit 2
  | _ -> exit 0
