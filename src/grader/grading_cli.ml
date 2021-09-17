(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

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
  let rec get_dir () =
    let d =
      Filename.concat
        (Filename.get_temp_dir_name ())
        (Printf.sprintf "grader_%6X" (Random.int 0xFFFFFF))
    in
    Lwt.catch (fun () -> Lwt_unix.mkdir d 0o700 >>= fun () -> Lwt.return d)
    @@ function
    | Unix.Unix_error(Unix.EEXIST, _, _) -> get_dir ()
    | e -> raise e
  in
  get_dir () >>= fun dir ->
  Lwt.catch
    (fun () -> f dir >>= fun res -> remove_dir dir >>= fun () -> Lwt.return res)
    (fun e -> remove_dir dir >>= fun () -> Lwt.fail e)

let get_grade ?callback ?timeout ?dirname exo solution =
  with_temp_dir @@ fun cmis_dir ->
  let module ResDump =
    OCamlResFormats.Files (OCamlResSubFormats.Raw) in
  let dump_cmis =
    ResDump.output { OCamlResFormats.base_output_dir = cmis_dir } in
  dump_cmis Embedded_cmis.root ;
  dump_cmis Embedded_grading_cmis.root ;
  Load_path.init [ cmis_dir ] ;
  Toploop_unix.initialize () ;
  let divert name chan cb =
    let redirection = Toploop_unix.redirect_channel name chan cb in
    fun () -> Toploop_unix.stop_channel_redirection redirection in
  Lwt.wrap @@ fun () ->
  Grading.get_grade ?callback ?timeout ?dirname ~divert exo solution
