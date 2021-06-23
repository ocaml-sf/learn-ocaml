(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
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
    ~decipher:false ()

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
    ?print_result ?dirname meta exercise output_json =
  let rec sleep () =
    if !n_processes <= 0 then
      Lwt_main.yield () >>= sleep
    else (
      decr n_processes; Lwt.return_unit
    )
  in
  sleep () >>= fun () ->
  Lwt_io.flush_all () >>= fun () ->
  match Lwt_unix.fork () with
  | 0 ->
      Grader_cli.dump_outputs := dump_outputs;
      Grader_cli.dump_reports := dump_reports;
      Grader_cli.display_callback := false;
      Lwt_main.run
        (Lwt.catch (fun () ->
             (match exercise with
              | Learnocaml_exercise.Subexercise (_id,exs) ->
                 Lwt_list.map_p
                   (fun exo -> Grader_cli.grade ?print_result ?dirname meta
                                 (Learnocaml_exercise.Exercise exo) output_json)
                   exs
              | exo -> Lwt_list.map_p
                         (fun exo -> Grader_cli.grade ?print_result ?dirname meta
                                       exo output_json)
                         [exo]
             )
             >|= fun l ->
             let rec aux = function
               | [] -> []
               | r ::  l -> ( print_grader_error exercise r; r :: aux l)
             in
             let rec result = function
               | Ok () :: l -> result l
               | Error _ :: _ -> exit 1
               | [] -> exit 0
              in
             result @@ aux l)
           (fun e ->
             Printf.eprintf "%!Grader error: %s\n%!" (Printexc.to_string e);
             exit 10))
  | pid ->
      Lwt_unix.waitpid [] pid >>= fun (_pid, ret) ->
      incr n_processes;
      match ret with
      | Unix.WEXITED 0 -> Lwt.return ([Ok ()])
      | _ -> Lwt.return ([Error (-1)])

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
                  | None -> Some (Index.Exercises [full_id, None, None])
                  | Some (Index.Exercises e) ->
                      Some (Index.Exercises (e @ [full_id, None, None]))
                  | _ -> None
                else if Sys.file_exists (f / "subindex.json") then
                  match acc with
                  | None -> Some (Index.Exercises [full_id, None, None])
                  | Some (Index.Exercises e) ->
                     Some (Index.Exercises (e @ [full_id, None, None]))
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
            print_string "fill_structure groups\n";
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
            print_string "fillstructure exe\n";
             let filtered id =
               !exercises_filtered <> SSet.empty
               && not (SSet.mem id !exercises_filtered) in
             Lwt_list.fold_left_s
               (fun (all_exercises, acc) (id, _ , _) ->
                  if SMap.mem id all_exercises || filtered id then
                    Lwt.return (all_exercises, acc)
                  else
                    if Sys.file_exists (!exercises_dir / id / "meta.json") then
                     (from_file Meta.enc
                        (!exercises_dir / id / "meta.json")
                      >>= fun meta ->
                      read_exercise (!exercises_dir / id)
                      >|= fun exercise ->
                      SMap.add id (Learnocaml_exercise.Exercise exercise) all_exercises,
                      (id, Some meta, None) :: acc)
                    else
                      (from_file (Subindex.enc)
                         (!exercises_dir / id / "subindex.json")
                       >>= fun meta ->
                       let subexercise_list = Exercise.Subindex.to_part meta
                       in
                       let rec aux = function
                         | [] -> []
                         | part::l ->
                            let (_,subexercise,_,_,_) =
                              Exercise.Subindex.get_part_field part
                            in (id,subexercise,Some meta)::aux l
                       in
                       let listing = aux subexercise_list
                       in
                       let subexercises =
                         (Lwt_list.fold_left_s
                            (fun (acc) (sup_id,sub_id,_) ->
                              let long_id = (sup_id / sub_id) in
                              if SMap.mem long_id all_exercises || filtered long_id then
                                Lwt.return acc
                              else
                                read_exercise (!exercises_dir / long_id)
                                >|= fun exercise ->
                                print_string (long_id^"\n");
                                exercise :: acc))
                           ([]) (List.rev listing)
                       in subexercises >|= fun exercise ->
                       SMap.add id (Learnocaml_exercise.Subexercise (id,exercise)) all_exercises,
                       (id, None, Some meta) :: acc))
               (all_exercises, []) (List.rev ids)
             >>= fun (all_exercises, exercises) ->
             Lwt.return (all_exercises, Index.Exercises exercises)
       in
       fill_structure SMap.empty structure >>= fun (all_exercises, index) ->
       to_file Index.enc (dest_dir / Learnocaml_index.exercise_index_path)
         index >>= fun () ->
       dump_dot index >>= fun () ->
       Learnocaml_store.Exercise.Index.get_from_index index >>= fun index ->
       to_file Json_encoding.(tup2 Index.enc (assoc float))
         (dest_dir / "exercise-index.json") (index, [])
       >>= fun () ->
       SSet.iter (fun id ->
           if not (SMap.mem id all_exercises) then
             Format.printf "[Warning] Filtered exercise '%s' not found.@." id)
         !exercises_filtered;

       let processes_arguments =
         List.rev @@ SMap.fold
                       (fun id exercise acc ->
             let exercise_dir = !exercises_dir / id in
             let json_path = dest_dir / Learnocaml_index.exercise_path id in
             let changed = try
                 let { Unix.st_mtime = json_time ; _ } = Unix.stat json_path in
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
             (id, exercise_dir, exercise, json_path,
              changed, dump_outputs, dump_reports) :: acc)
           all_exercises [] in
       begin
         let listmap, grade =
           if !n_processes = 1 then
             (Lwt_list.map_s,
             fun dump_outputs dump_reports ?print_result ?dirname
                 meta exercise json_path ->
             Grader_cli.dump_outputs := dump_outputs;
             Grader_cli.dump_reports := dump_reports;
             (match exercise with
              | Learnocaml_exercise.Subexercise (_id,exs) ->
                 Lwt_list.map_p
                   (fun exo -> Grader_cli.grade ?print_result ?dirname meta
                                 (Learnocaml_exercise.Exercise exo) json_path)
                   exs
              | exo -> Lwt_list.map_p
                         (fun exo -> Grader_cli.grade ?print_result ?dirname meta
                                       exo json_path)
                         [exo]
             )
             >|= fun l ->
             let rec aux = function
               | [] -> []
               | r ::  l -> ( print_grader_error exercise r; r :: aux l)
              in aux l )
           else
             Lwt_list.map_p,
             spawn_grader
         in
         listmap (fun (id, ex_dir, exercise, json_path, changed, dump_outputs,dump_reports) ->
             print_string ("ID : "^id^"\nEx_dir : "^ex_dir^"\n");
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
                 Format.printf "%-24s (no changes)@." id ;
                 Lwt.return true
               end else begin
                 grade dump_outputs dump_reports
                   ~dirname:(!exercises_dir / id) (Index.find index id) exercise (Some json_path)
                 >>=
                   function
                 | Ok () :: _ (* à changer *) ->
                    Format.printf "%-24s     [OK]@." id ;
                    Lwt.return true
                 | Error _ :: _->
                    Format.printf "%-24s   [FAILED]@." id ;
                    Lwt.return false
                 | [] -> Lwt.return false
               end)
           processes_arguments
       end >>= fun results ->
       Lwt.return (List.for_all ((=) true) results))
    (fun exn ->
      let print_unknown ppf = function
        | Failure msg -> Format.fprintf ppf "Cannot process exercises: %s" msg
        | exn -> Format.fprintf ppf "Cannot process exercises: %s"  (Printexc.to_string exn) in
      Json_encoding.print_error ~print_unknown Format.err_formatter exn ;
      Format.eprintf "@." ;
      Lwt.return false)
