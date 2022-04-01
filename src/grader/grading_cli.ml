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
        (Printf.sprintf "grader_%06X" (Random.int 0xFFFFFF))
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

(* The answer of the grader will be returned marshalled through a pipe:
   type it explicitely and avoid any exceptions inside. *)
type grader_answer =
  (Learnocaml_report.t, Grading.error) Stdlib.result * string * string * string

let get_grade ?callback ?timeout ?dirname exo solution =
  with_temp_dir @@ fun cmis_dir ->
  Lwt_io.flush_all () >>= fun () ->
  flush_all ();
  let in_fd, out_fd = Unix.pipe ~cloexec:true () in
  match Lwt_unix.fork () with
  | 0 ->
      (* /!\ there must be strictly no Lwt calls in the child *)
      Unix.close in_fd;
      let oc = Unix.out_channel_of_descr out_fd in
      let (ret: grader_answer) =
        let module ResDump =
          OCamlResFormats.Files (OCamlResSubFormats.Raw) in
        let dump_cmis =
          ResDump.output { OCamlResFormats.base_output_dir = cmis_dir } in
        dump_cmis Embedded_cmis.root ;
        (* dump_cmis Embedded_grading_cmis.root ; *)
        Load_path.init [ cmis_dir ] ;
        Toploop_unix.initialize () ;
        let divert name chan cb =
          let redirection = Toploop_unix.redirect_channel name chan cb in
          fun () -> Toploop_unix.stop_channel_redirection redirection in
        let load_code compiled_code =
          try
            Toploop_unix.use_compiled_string compiled_code.Learnocaml_exercise.cma;
            Toploop_ext.Ok (true, [])
          with _ -> Toploop_ext.Ok (false, [])
        in
        Grading.get_grade ?callback ?timeout ?dirname ~divert ~load_code
          exo solution
      in
      output_value oc ret;
      flush_all ();
      Unix._exit 0
  | child_pid ->
      Unix.close out_fd;
      let ic = Lwt_io.of_unix_fd ~mode:Lwt_io.Input in_fd in
      Lwt.catch
        (fun () -> Lwt_io.read_value ic >|= Option.some)
        (function End_of_file -> Lwt.return_none | exn -> Lwt.fail exn)
      >>= fun (ans: grader_answer option) ->
      Lwt_unix.waitpid [] child_pid >>= fun (_pid, stat) ->
      match ans, stat with
      | _, Unix.WSIGNALED n ->
          Printf.ksprintf Lwt.fail_with "Grading sub-process was killed (%d)" n
      | Some ans, Unix.WEXITED 0 ->
          Lwt.return ans
      | _ ->
          Lwt.fail_with "Grading sub-process error"
