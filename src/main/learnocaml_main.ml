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

let repo_dir = ref (Sys.getcwd ())
let contents_dir =
  ref (readlink (Filename.dirname (Filename.dirname (Sys.argv.(0)))/"share"/"learn-ocaml"/"www"))
let dest_dir = ref (readlink ("www"))
let generate_only = ref false
let server_only = ref false

let args = Arg.align @@
  [ "-repo", Arg.String (fun s ->
        repo_dir := s;
        Learnocaml_process_exercise_repository.exercises_dir := s/"exercises";
        Learnocaml_process_tutorial_repository.tutorials_dir := s/"tutorials"),
    Printf.sprintf
      "PATH repository root containing the exercises, tutorials and lessons \
       (default: [%s])"
      !repo_dir] @
  [ "-dest-dir", Arg.Set_string dest_dir,
    Printf.sprintf
      "PATH target path for the webapp (default: [%s])"
      !dest_dir] @
  [ "-contents-dir", Arg.Set_string contents_dir,
    Printf.sprintf
      "PATH directory containing the base learn-ocaml app contents (default: \
       [%s])"
      !contents_dir] @
  [ "-generate", Arg.Set generate_only,
    "Only generate the app, don't run the server" ] @
  [ "-server-only", Arg.Set server_only,
    "Only run the server, assume the app is up-to-date" ] @
  Learnocaml_process_exercise_repository.args @
  Learnocaml_process_tutorial_repository.args @
  Learnocaml_simple_server.args

let copy_tree src dst =
  Lwt_process.exec ("", [|"cp"; "-PRpT"; src; dst|]) >>= fun r ->
  if r <> Unix.WEXITED 0 then
    Lwt.fail_with "Failed to copy base app contents"
  else Lwt.return_unit

let () =
  Arg.parse args
    (fun anon -> raise (Arg.Bad "unexpected anonymous argument"))
    (Printf.sprintf "Usage: %s [options]" Sys.argv.(0));
  let generate =
    if !generate_only || not !server_only then
      (if not (Sys.file_exists !contents_dir) then
         Lwt.fail_with @@
         Printf.sprintf "Could not find base app contents at %s" !contents_dir
       else
         Lwt.return_unit) >>= fun () ->
      Printf.printf "Updating app at %s\n%!" !dest_dir;
      copy_tree !contents_dir !dest_dir >>= fun () ->
      (if Sys.file_exists (!repo_dir/"lessons") then
         copy_tree (!repo_dir/"lessons") !dest_dir
       else Lwt.return_unit) >>= fun () ->
      Learnocaml_process_tutorial_repository.main !dest_dir >>= fun e_ret ->
      Learnocaml_process_exercise_repository.main !dest_dir >>= fun t_ret ->
      Lwt.return (e_ret && t_ret)
    else
      Lwt.return true
  in
  let run_server () =
    if !server_only || not !generate_only then
      (Printf.printf "Starting server on port %d\n%!"
         !Learnocaml_simple_server.port;
       Learnocaml_simple_server.launch ())
    else
      Lwt.return true
  in
  let ret =
    Lwt_main.run
      (generate >>= fun success ->
       if success then
         run_server () >>= fun r ->
         if r then Lwt.return 0 else Lwt.return 10
       else
         Lwt.return 1)
  in
  exit ret
