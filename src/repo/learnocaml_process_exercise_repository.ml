(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2023 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Learnocaml_process_common
open Learnocaml_data
open Exercise

open Lwt.Infix

let read_exercise exercise_dir =
  let open Lwt.Infix in
  let read_field field =
    let fn = Filename.concat exercise_dir field in
    Lwt_unix.file_exists fn >>= fun exists ->
    if not exists then
      Lwt.return None
    else
      Lwt_io.with_file ~mode:Lwt_io.Input fn Lwt_io.read >>= fun content ->
      Lwt.return (Some content)
  in
  Learnocaml_exercise.read_lwt ~read_field
    ~id:(Filename.basename exercise_dir)
    ()

let exercises_dir = ref "./exercises"

let exercises_index = ref None

let exercises_filtered = ref SSet.empty

let dump_outputs = ref None

let dump_reports = ref None

let dump_dot exs =
  match !Grader_cli.dump_dot with
    None -> Lwt.return ()
  | Some filename ->
      let graph = Exercise.Graph.compute_graph ~filters:[] exs in
      Lwt_io.with_file ~mode:Lwt_io.Output filename
        (fun oc -> Lwt_io.write oc (Format.asprintf "%a" Exercise.Graph.dump_dot graph))

let n_processes = ref 1

let grading_status, grading_status_add, grading_status_remove =
  let in_progress = ref [] in
  let tty = Unix.isatty Unix.stderr in
  let show () =
    match !in_progress with
    | [] -> flush stderr
    | prog ->
        Printf.eprintf "Grading in progress: %s" (String.concat " " prog);
        if tty then (flush stderr; prerr_string "\r\027[K") else prerr_newline ()
  in
  show,
  (fun id -> in_progress := !in_progress @ [id]; show ()),
  (fun id ->
     in_progress := List.filter (fun x -> not (String.equal x id)) !in_progress;
     show ())

let print_grader_error exercise = function
  | Ok () -> ()
  | Error (-1) -> ()
  | Error n ->
      Format.eprintf "[ERROR] %s: the solution has errors! (%d points%s)@."
        Learnocaml_exercise.(access File.id exercise)
        n
        (if !Grader_cli.display_reports then ""
         else ". Run with '-v' to see the report")

let spawn_grader
    dump_outputs dump_reports
    ?print_result ?dirname id meta ex_dir output_json =
  let rec sleep () =
    if !n_processes <= 0 then
      Lwt.pause () >>= sleep
    else (
      decr n_processes; Lwt.return_unit
    )
  in
  sleep () >>= fun () ->
  Lwt.catch (fun () ->
      read_exercise ex_dir >>= fun exercise ->
      grading_status_add id;
      Grader_cli.grade
        ~dump_outputs ~dump_reports ~display_callback:false
        ?print_result ?dirname meta exercise output_json
      >|= fun r ->
      grading_status_remove id;
      print_grader_error exercise r;
      incr n_processes;
      r)
    (fun e ->
       incr n_processes;
       grading_status_remove id;
       Printf.eprintf "Grader error: %s\n%!" (Printexc.to_string e);
       Lwt.return (Error 0))

let exe_mtime =
  try Unix.((stat (Sys.executable_name)).st_mtime)
  with Unix.Unix_error _ -> max_float

let main dest_dir =
  let exercises_index =
    match !exercises_index with
    | Some exercises_index -> exercises_index
    | None -> !exercises_dir / "index.json" in
  let exercises_dest_dir = dest_dir / Learnocaml_index.exercises_dir in
  Lwt_utils.mkdir_p exercises_dest_dir >>= fun () ->
  Lwt.catch
    (fun () ->
       (if Sys.file_exists exercises_index then
          from_file Exercise.Index.enc exercises_index
        else if Sys.file_exists !exercises_dir then
          let rec auto_index path =
            let entries = Sys.readdir path in
            Array.sort compare entries;
            Array.fold_left (fun acc id ->
                let f = path / id in
                let full_id =
                  String.sub f (String.length !exercises_dir + 1)
                    (String.length f - String.length !exercises_dir - 1)
                in
                if Sys.file_exists (f / "meta.json") then
                  match acc with
                  | None -> Some (Index.Exercises [full_id, None])
                  | Some (Index.Exercises e) ->
                      Some (Index.Exercises (e @ [full_id, None]))
                  | _ -> None
                else if Sys.is_directory f then
                  match acc, auto_index f with
                  | None, None -> None
                  | None, Some contents ->
                      Some (Index.Groups
                              ([full_id, Index.{title = id; contents}]))
                  | Some (Index.Groups g), Some contents ->
                      Some (Index.Groups
                              (g @ [full_id, Index.{title = id; contents}]))
                  | Some _, None -> acc
                  | _ -> None
                else acc)
              None
              entries
          in
          match auto_index !exercises_dir with
          | None -> Lwt.fail_with "Missing index file and malformed repository"
          | Some i ->
              Format.eprintf "Missing index file, using all exercise directories.@." ;
              Lwt.return i
        else
          (Format.eprintf "No index file, no exercise directory.@." ;
           Format.eprintf "This does not look like a LearnOCaml exercise repository.@." ;
           Lwt.fail (Failure "cannot continue")))
       >>= fun structure ->

       (* Exercises must be unique, since their id refer to the directory. *)
       let rec fill_structure all_exercises = function
         | Index.Groups groups ->
             (* Ensures groups of a same parent are unique *)
             Lwt_list.fold_left_s
               (fun (all_exercises, subgroups, acc) (id, gr) ->
                  if SMap.mem id subgroups then
                    Lwt.return (all_exercises, subgroups, acc)
                  else
                    fill_structure all_exercises gr.Index.contents
                    >|= fun (all_exercises, contents) ->
                    all_exercises,
                    SMap.add id gr.Index.title subgroups,
                    ((id, Index.{ title = gr.title; contents }) :: acc))
               (all_exercises, SMap.empty, []) (List.rev groups)
             >|= fun (all_exercises, _subgroups, groups) ->
             all_exercises, Index.Groups groups
         | Index.Exercises ids ->
             let filtered id =
               !exercises_filtered <> SSet.empty
               && not (SSet.mem id !exercises_filtered) in
             Lwt_list.fold_left_s
               (fun (all_exercises, acc) (id, _) ->
                  if SMap.mem id all_exercises || filtered id then
                    Lwt.return (all_exercises, acc)
                  else
                    from_file Meta.enc
                      (!exercises_dir / id / "meta.json")
                    >|= fun meta ->
                    let exercise_dir = !exercises_dir / id in
                    SMap.add id exercise_dir all_exercises,
                    (id, Some meta) :: acc)
               (all_exercises, []) (List.rev ids)
             >>= fun (all_exercises, exercises) ->
             Lwt.return (all_exercises, Index.Exercises exercises)
       in
       fill_structure SMap.empty structure >>= fun (all_exercises, index) ->
       to_file Index.enc (dest_dir / Learnocaml_index.exercise_index_path) index >>= fun () ->
       dump_dot index >>= fun () ->
       Learnocaml_store.Exercise.Index.get_from_index index >>= fun index ->
       to_file Json_encoding.(tup2 Learnocaml_store.Exercise.Index.enc (assoc float)) (dest_dir / "exercise-index.json") (index, [])
       >>= fun () ->
       SSet.iter (fun id ->
           if not (SMap.mem id all_exercises) then
             Format.printf "[Warning] Filtered exercise '%s' not found.@." id)
         !exercises_filtered;

       let processes_arguments =
         List.rev @@ SMap.fold
           (fun id exercise_dir acc ->
              let json_path = dest_dir / Learnocaml_index.exercise_path id in
              let changed = try
                  let { Unix.st_mtime = json_time ; _ } = Unix.stat json_path in
                  exe_mtime >= json_time ||
                  Sys.readdir exercise_dir |>
                  Array.to_list |>
                  List.map (fun f -> (Unix.stat (exercise_dir / f)).Unix.st_mtime ) |>
                  List.exists (fun t -> t >= json_time)
                with _ -> true in
              let dump_outputs =
                match !dump_outputs with
                | None -> None
                | Some dir -> Some (dir / id) in
              let dump_reports =
                match !dump_reports with
                | None -> None
                | Some dir -> Some (dir / id) in
              (id, exercise_dir, json_path,
               changed, dump_outputs, dump_reports) :: acc)
           all_exercises [] in
       begin
         let listmap, grade =
           if !n_processes = 1 then
             Lwt_list.map_s,
             fun dump_outputs dump_reports ?print_result ?dirname
               _id meta ex_dir json_path ->
               read_exercise ex_dir >>= fun exercise ->
               Grader_cli.grade
                 ~dump_outputs ~dump_reports ~display_callback:true
                 ?print_result ?dirname
                 meta exercise json_path
               >|= fun r -> print_grader_error exercise r; r
           else
             Lwt_list.map_p,
             spawn_grader
         in
         listmap (fun (id, ex_dir, json_path, changed, dump_outputs,dump_reports) ->
             let dst_ex_dir = String.concat Filename.dir_sep [dest_dir; "static"; id] in
             Lwt_utils.mkdir_p dst_ex_dir >>= fun () ->
               Lwt_stream.iter_p (fun base ->
                 let d = Filename.concat ex_dir base in
                 let dst = String.concat Filename.dir_sep [dst_ex_dir; base] in
                 if Sys.is_directory d && base.[0] <> '.' then
                    Lwt_utils.copy_tree d dst
                 else Lwt.return_unit)
                 (Lwt_unix.files_of_directory ex_dir) >>= fun () ->
               if not changed then begin
                 Format.eprintf "%-24s (no changes)@." id ;
                 Lwt.return_true
               end else begin
                 Learnocaml_precompile_exercise.precompile ~exercise_dir:ex_dir
                 >>= fun () ->
                 grade dump_outputs dump_reports
                   ~dirname:ex_dir id (Index.find index id) ex_dir (Some json_path)
                 >>= function
                 | Ok () ->
                     Format.eprintf "%-24s     [OK]@." id ;
                     Lwt.return true
                 | Error _ ->
                     Format.eprintf "%-24s   [FAILED]@." id ;
                     Lwt.return false
               end
                 >|= fun r -> grading_status (); r)
             processes_arguments
       end >>= fun results ->
       Lwt.return (List.for_all ((=) true) results))
    (fun exn ->
       let print_unknown ppf = function
         | Unix.Unix_error (Unix.EMFILE, _, _) ->
             Format.fprintf ppf
               "Too many open files. Try reducing the number of concurrent jobs \
                (with the `-j` flag) or use `ulimit -n` with a higher value"
         | Failure msg -> Format.fprintf ppf "Cannot process exercises: %s" msg
         | exn -> Format.fprintf ppf "Cannot process exercises: %s"  (Printexc.to_string exn) in
       Json_encoding.print_error ~print_unknown Format.err_formatter exn ;
       Format.eprintf "@." ;
       Lwt.return false)
