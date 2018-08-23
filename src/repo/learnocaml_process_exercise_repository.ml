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

open Learnocaml_data
open Exercise

open Lwt.Infix

let to_file encoding fn value =
  Lwt_io.(with_file ~mode: Output) fn @@ fun chan ->
  let json = Json_encoding.construct encoding value in
  let json = match json with
    | `A _ | `O _ as d -> d
    | v -> `A [ v ] in
  let str = Ezjsonm.to_string ~minify:false (json :> Ezjsonm.t) in
  Lwt_io.write chan str

let from_file encoding fn =
  Lwt_io.(with_file ~mode: Input) fn @@ fun chan ->
  Lwt_io.read chan >>= fun str ->
  let json = Ezjsonm.from_string str in
  Lwt.return (Json_encoding.destruct encoding json)

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

let spawn_grader ?print_result ?dirname exercise output_json =
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
      Grader_cli.display_callback := false;
      Lwt_main.run
        (Grader_cli.grade ?print_result ?dirname exercise output_json
         >|= fun r ->
         print_grader_error exercise r;
         match r with
         | Ok () -> exit 0
         | Error _ -> exit 1)
  | pid ->
      Lwt_unix.waitpid [] pid >>= fun (_pid, ret) ->
      incr n_processes;
      match ret with
      | Unix.WEXITED 0 -> Lwt.return (Ok ())
      | _ -> Lwt.return (Error (-1))

let main dest_dir =
  let (/) dir f =
    String.concat Filename.dir_sep [ dir ; f ] in
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
                    >>= fun meta ->
                    read_exercise (!exercises_dir / id)
                    >|= fun exercise ->
                    SMap.add id exercise all_exercises,
                    (id, Some meta) :: acc)
               (all_exercises, []) (List.rev ids)
             >>= fun (all_exercises, exercises) ->
             Lwt.return (all_exercises, Index.Exercises exercises)
       in



       (* (\* Exercises must be unique, since their id refer to the directory. *\)
        * let all_exercises = ref SMap.empty in
        * let rec fill_structure = function
        *   | Index.Groups groups ->
        *       Lwt_list.fold_left_s
        *         (fun (subgroups, acc) (id, (title, group)) ->
        *            if SMap.mem id subgroups then Lwt.return (subgroups, acc)
        *            (\* Ensures groups of a same parent are unique *\)
        *            else begin
        *              let subgroups = SMap.add id title subgroups in
        *              fill_structure group >>= fun contents ->
        *              acc >>= fun acc ->
        *              Lwt.return ((id, Index.{title; contents}) :: acc)
        *            end)
        *         (SMap.empty, [])
        *         (List.rev groups)
        *       >>= fun groups ->
        *       Lwt.return (Index.Groups groups)
        *   | Index.Exercises ids ->
        *       let filtered id =
        *         !exercises_filtered <> SSet.empty
        *         && not (SSet.mem id !exercises_filtered) in
        *       List.fold_left
        *         (fun acc id ->
        *            if SMap.mem id !all_exercises || filtered id then acc
        *            else begin
        *              let exercise =
        *                read_exercise (!exercises_dir / id) in
        *              all_exercises := SMap.add id exercise !all_exercises ;
        *              acc >>= fun acc ->
        *              Lwt.return
        *                ((id, exercise) :: acc)
        *            end)
        *         (Lwt.return []) (List.rev ids) >>= fun exercises ->
        *       Lwt.return (Index.Exercises exercises) in *)
       fill_structure SMap.empty structure >>= fun (all_exercises, index) ->
       to_file Index.enc (dest_dir / Learnocaml_index.exercise_index_path) index >>= fun () ->
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
             Lwt_list.map_s,
             fun ?print_result ?dirname exercise json_path ->
               Grader_cli.grade ?print_result ?dirname exercise json_path
               >|= fun r -> print_grader_error exercise r; r
           else
             Lwt_list.map_p,
             spawn_grader
         in
         listmap (fun (id, _, exercise, json_path, changed, dump_outputs,dump_reports) ->
               if not changed then begin
                 Format.printf "%-24s (no changes)@." id ;
                 Lwt.return true
               end else begin
                 Grader_cli.dump_outputs := dump_outputs ;
                 Grader_cli.dump_reports := dump_reports ;
                 grade ~dirname:(!exercises_dir / id) exercise (Some json_path)
                 >>= function
                 | Ok () ->
                     Format.printf "%-24s     [OK]@." id ;
                     Lwt.return true
                 | Error _ ->
                     Format.printf "%-24s   [FAILED]@." id ;
                     Lwt.return false
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
