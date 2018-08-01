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

open Learnocaml_index

open Lwt.Infix

let index_enc =
  let contents_enc =
    let open Json_encoding in
    mu "group" @@ fun group_enc ->
    union
      [ case
          (obj2
             (req "title" string)
             (req "exercises" (list string)))
          (function
            | (title, `Exercises map) -> Some (title, map)
            | _ -> None)
          (fun (title, map) -> (title, `Exercises map)) ;
        case
          (obj2
             (req "title" string)
             (req "groups" (assoc group_enc)))
          (function
            | (title, `Groups map) -> Some (title, map)
            | _ -> None)
          (fun (title, map) -> (title, `Groups map)) ] in
  let open Json_encoding in
  check_version_2 @@
  union
    [ case
        (obj1 (req "exercises" (list string)))
        (function | `Exercises map -> Some map | _ -> None)
        (fun map -> `Exercises map) ;
      case
        (obj1 (req "groups" (assoc contents_enc)))
        (function | `Groups map -> Some map | _ -> None)
        (fun map -> `Groups map) ]

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

module StringMap = Map.Make (String)

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
  Lwt_main.run
    (Learnocaml_exercise.read_lwt ~read_field
       ~id:(Filename.basename exercise_dir)
       ~decipher:false ())

let exercises_dir = ref "./exercises"

let exercises_index = ref None

let dump_outputs = ref None

let dump_reports = ref None

let n_processes = ref 1

let args = Arg.align @@
  [ "-exercises-dir", Arg.Set_string exercises_dir,
    "PATH path to the exercise repository (default: [./exercises])" ;
    "-exercises-index", Arg.String (fun fn -> exercises_index := Some fn),
    "PATH path to the exercises index (default: [<exercises-dir>/index.json])" ;
    "-display-outcomes", Arg.Set Grader_cli.display_outcomes,
    " display the toplevel's outcomes" ;
    "-display-progression", Arg.Set Grader_cli.display_callback,
    " display grading progression messages" ;
    "-display-stdouts", Arg.Set Grader_cli.display_std_outputs,
    " display the toplevel's standard outputs" ;
    "-dump-outputs", Arg.String (fun s -> dump_outputs := Some s),
    "PATH save the outputs in the given directory" ;
    "-dump-reports", Arg.String (fun s -> dump_reports := Some s),
    "PATH save the reports in the given directory" ;
    "-j", Arg.Set_int n_processes,
    "NUMBER grader processes to launch in parallel" ]


let () =
  match Unix.getenv "LEARNOCAML_PROCESS_REPOSITORY_TASK" with
  | _ -> Grader_cli.main () ; exit 0
  | exception Not_found -> ()

let spawn_grader args =
  Lwt_process.exec
    ~env: (Array.concat [ [| "LEARNOCAML_PROCESS_REPOSITORY_TASK=YES" |] ;
                          Unix.environment () ])
    (Sys.argv.(0), Array.concat [ [| Sys.argv.(0) |] ; args ])

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
          from_file index_enc exercises_index
        else if Sys.file_exists !exercises_dir then
          let rec auto_index path =
            Array.fold_left (fun acc f ->
                let rel_f = if path = "" then f else path / f in
                if Sys.file_exists (!exercises_dir/rel_f/"meta.json") then
                  match acc with
                  | None -> Some (`Exercises [rel_f])
                  | Some (`Exercises e) -> Some (`Exercises (e @ [rel_f]))
                  | _ -> None
                else if Sys.is_directory (!exercises_dir/rel_f) then
                  match acc, auto_index (rel_f) with
                  | None, None -> None
                  | None, Some g' -> Some (`Groups ([f, (f, g')]))
                  | Some (`Groups g), Some g' -> Some (`Groups (g @ [f, (f, g')]))
                  | _ -> None
                else acc)
              None (Sys.readdir (!exercises_dir/path))
          in
          match auto_index "" with
          | None -> failwith "Missing index file and malformed repository"
          | Some i -> 
              Format.eprintf "Missing index file, using all exercise directories.@." ;
              Lwt.return i
        else
          (Format.eprintf "No index file, no exercise directory.@." ;
           Format.eprintf "This does not look like a LearnOCaml exercise repository.@." ;
           Lwt.fail (Failure "cannot continue")))
       >>= fun structure ->
       (* Exercises must be unique, since their id refer to the directory. *)
       let all_exercises = ref StringMap.empty in
       let rec fill_structure = function
         | `Groups groups ->
             (* Ensures groups of a same parent are unique *)
             let subgroups : string StringMap.t ref = ref StringMap.empty in
             List.fold_left
               (fun acc (id, (group_title, str)) ->
                  if StringMap.mem id !subgroups then acc
                  else begin
                    subgroups := StringMap.add id group_title !subgroups ;
                    fill_structure str >>= fun group_contents ->
                    acc >>= fun acc ->
                    Lwt.return ((id, { group_title ; group_contents }) :: acc)
                  end)
               (Lwt.return []) (List.rev groups) >>= fun groups ->
             Lwt.return (Groups groups)
         | `Exercises ids ->
             List.fold_left
               (fun acc id ->
                  if StringMap.mem id !all_exercises then acc
                  else begin
                    let exercise =
                      read_exercise (!exercises_dir / id) in
                    all_exercises := StringMap.add id exercise !all_exercises ;
                    let exercise_indexed = Learnocaml_exercise.to_index exercise in
                    acc >>= fun acc ->
                    Lwt.return
                      ((id, exercise_indexed) :: acc)
                  end)
               (Lwt.return []) (List.rev ids) >>= fun exercises ->
             Lwt.return (Learnocaml_exercises exercises) in
       fill_structure structure >>= fun index ->
       to_file exercise_index_enc (dest_dir / exercise_index_path) index >>= fun () ->
       let processes_arguments =
         StringMap.fold
           (fun id exercise acc ->
              let exercise_dir = !exercises_dir / id in
              let json_path = dest_dir / exercise_path id in
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
           !all_exercises [] in
       begin if !n_processes = 1 then
           Lwt_list.map_s (fun (id, _, exercise, json_path, changed, dump_outputs,dump_reports) ->
               if not changed then begin
                 Format.printf "%-12s (no changes)@." id ;
                 Lwt.return true
               end else begin
                 Grader_cli.dump_outputs := dump_outputs ;
                 Grader_cli.dump_reports := dump_reports ;
                 Grader_cli.grade exercise (Some json_path) >>= fun result ->
                 match result with
                 | 0 ->
                     Format.printf "%-12s     [OK]@." id ;
                     Lwt.return true
                 | _ ->
                     Format.printf "%-12s   [FAILED]@." id ;
                     Lwt.return false
               end)
             processes_arguments
         else
           let pool = Lwt_pool.create !n_processes (fun () -> Lwt.return ()) in
           Lwt_list.map_p (fun (id, exercise_dir, _, json_path, changed, dump_outputs, dump_reports) ->
               Lwt_pool.use pool @@ fun () ->
               if not changed then begin
                 Format.printf "%-12s (no changes)@." id ;
                 Lwt.return true
               end else begin
                 let args = Array.concat [
                     (match dump_outputs with
                      | None -> [||]
                      | Some prefix -> [| "-dump-outputs" ; prefix |]) ;
                     (match dump_reports with
                      | None -> [||]
                      | Some prefix -> [| "-dump-reports" ; prefix |]) ;
                     (if !Grader_cli.display_outcomes then [| "-display-outcomes" |] else [||]) ;
                     (if !Grader_cli.display_callback then [| "-display-progression" |] else [||]) ;
                     (if !Grader_cli.display_std_outputs then [| "-display-stdouts"  |] else [||]) ;
                     [| "-output-json" ; json_path |] ;
                     [| exercise_dir |] ]in
                 spawn_grader args >>= function
                 | Unix.WEXITED 0 ->
                     Format.printf "%-12s     [OK]@." id ;
                     Lwt.return true
                 | _ ->
                     Format.printf "%-12s   [FAILED]@." id ;
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
