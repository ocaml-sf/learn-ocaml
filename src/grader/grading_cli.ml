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

open Lwt

let rec remove_dir dir =
  Lwt_stream.iter_p (remove dir) (Lwt_unix.files_of_directory dir) >>= fun () ->
  Lwt_unix.rmdir dir
and remove dir name =
  if name = "." || name = ".." then
    Lwt.return_unit
  else
    let file = Filename.concat dir name in
    if Sys.is_directory file then remove_dir file else Lwt_unix.unlink file

let with_temp_dir f =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "grader_%6X" (Random.int 0xFFFFFF)) in
  Lwt_unix.mkdir dir 0o700 >>= fun () ->
  Lwt.catch
    (fun () -> f dir >>= fun res -> remove_dir dir >>= fun () -> Lwt.return res)
    (fun e -> remove_dir dir >>= fun () -> Lwt.fail e)

let get_grade ?callback ?timeout exo solution =
  with_temp_dir @@ fun cmis_dir ->
  let module ResDump =
    OCamlResFormats.Files (OCamlResSubFormats.Raw) in
  let dump_cmis =
    ResDump.output { OCamlResFormats.base_output_dir = cmis_dir } in
  dump_cmis Embedded_cmis.root ;
  dump_cmis Embedded_grading_cmis.root ;
  Config.load_path := [ cmis_dir ] ;
  Toploop_unix.initialize () ;
  let divert name chan cb =
    let redirection = Toploop_unix.redirect_channel name chan cb in
    fun () -> Toploop_unix.stop_channel_redirection redirection in
  Lwt.return
    (Grading.get_grade ?callback ?timeout ~divert exo solution)
