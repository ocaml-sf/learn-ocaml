(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

let display_std_outputs = ref false

let dump_outputs = ref None

let dump_reports = ref None

let display_callback = ref false

let display_outcomes = ref false

let grade_student = ref None

let individual_timeout = ref None

let display_reports = ref false

let dump_dot = ref None

open Lwt.Infix

let read_exercise exercise_dir =
  let read_field field =
    let fn = Filename.concat exercise_dir field in
    Lwt_unix.file_exists fn
    >>= fun exists ->
    if not exists then Lwt.return None
    else
      Lwt_io.with_file ~mode:Lwt_io.Input fn Lwt_io.read
      >>= fun content -> Lwt.return (Some content)
  in
  Learnocaml_exercise.read_lwt ~read_field
    ~id:(Filename.basename exercise_dir)
    ~decipher:false ()

let remove_trailing_slash s =
  let len = String.length s in
  if len <> 0 && s.[len - 1] = '/' then String.sub s 0 (len - 1) else s

let read_student_file exercise_dir path =
  let fn =
    if Filename.is_relative path then Filename.concat exercise_dir path
    else path
  in
  Lwt_unix.file_exists fn
  >>= fun exists ->
  if not exists then (
    Format.eprintf "Cannot find '%s': No such file@." fn;
    exit 1 )
  else Lwt_io.with_file ~mode:Lwt_io.Input fn Lwt_io.read

let grade ?(print_result = false) ?dirname meta exercise output_json =
  Lwt.catch
    (fun () ->
      let code_to_grade =
        match !grade_student with
        | Some path -> read_student_file (Sys.getcwd ()) path
        | None ->
            Lwt.return Learnocaml_exercise.(decipher File.solution exercise)
      in
      let callback =
        if !display_callback then Some (Printf.eprintf "[ %s ]%!\r\027[K")
        else None
      in
      let timeout = !individual_timeout in
      code_to_grade
      >>= fun code ->
      Grading_cli.get_grade ?callback ?timeout ?dirname exercise code
      >>= fun (result, stdout_contents, stderr_contents, outcomes) ->
      flush stderr;
      match result with
      | Error exn ->
          let dump_error ppf =
            ( match Grading.string_of_exn exn with
            | Some msg -> Format.fprintf ppf "%s@." msg
            | None -> Format.fprintf ppf "%a@." Location.report_exception exn
            );
            if stdout_contents <> "" then
              Format.fprintf ppf "grader stdout:@.%s@." stdout_contents;
            if stderr_contents <> "" then
              Format.fprintf ppf "grader stderr:@.%s@." stderr_contents;
            if outcomes <> "" then
              Format.fprintf ppf "grader outcomes:@.%s@." outcomes
          in
          ( match !dump_outputs with
          | None -> ()
          | Some prefix ->
              let oc = open_out (prefix ^ ".error") in
              dump_error (Format.formatter_of_out_channel oc);
              close_out oc );
          dump_error Format.err_formatter;
          Lwt.return (Error (-1))
      | Ok report ->
          let max, failure = Learnocaml_report.result report in
          if !display_reports then
            Learnocaml_report.print
              (Format.formatter_of_out_channel stderr)
              report;
          ( match !dump_reports with
          | None -> ()
          | Some prefix ->
              let oc = open_out (prefix ^ ".report.txt") in
              Learnocaml_report.print
                (Format.formatter_of_out_channel oc)
                report;
              close_out oc;
              let oc = open_out (prefix ^ ".report.html") in
              Learnocaml_report.output_html
                (Format.formatter_of_out_channel oc)
                report;
              close_out oc );
          if stderr_contents <> "" then (
            ( match !dump_outputs with
            | None -> ()
            | Some prefix ->
                let oc = open_out (prefix ^ ".stderr") in
                output_string oc stderr_contents;
                close_out oc );
            if !display_std_outputs then Format.eprintf "%s" stderr_contents );
          if stdout_contents <> "" then (
            ( match !dump_outputs with
            | None -> ()
            | Some prefix ->
                let oc = open_out (prefix ^ ".stdout") in
                output_string oc stdout_contents;
                close_out oc );
            if !display_std_outputs then Format.printf "%s" stdout_contents );
          if outcomes <> "" then (
            ( match !dump_outputs with
            | None -> ()
            | Some prefix ->
                let oc = open_out (prefix ^ ".outcomes") in
                output_string oc outcomes; close_out oc );
            if !display_outcomes then Format.printf "%s" outcomes );
          if failure then (
            if print_result then
              Printf.eprintf "%-30s - Failure - %d points\n%!"
                Learnocaml_exercise.(access File.id exercise)
                max;
            Lwt.return (Error max) )
          else (
            if print_result then
              Printf.eprintf "%-30s - Success - %d points\n%!"
                Learnocaml_exercise.(access File.id exercise)
                max;
            match output_json with
            | None -> Lwt.return (Ok ())
            | Some json_file ->
                let json =
                  Json_encoding.(
                    construct
                      (tup3 Learnocaml_data.Exercise.Meta.enc
                         Learnocaml_exercise.encoding (option float)))
                    ( meta
                    , Learnocaml_exercise.(update File.max_score max exercise)
                    , None )
                in
                let json =
                  match json with (`A _ | `O _) as d -> d | v -> `A [v]
                in
                let str =
                  Ezjsonm.to_string ~minify:false (json :> Ezjsonm.t)
                in
                Lwt_utils.mkdir_p (Filename.dirname json_file)
                >>= fun () ->
                Lwt_io.with_file ~mode:Lwt_io.Output json_file
                @@ fun chan ->
                Lwt_io.write chan str >>= fun () -> Lwt.return (Ok ()) ) )
    (fun exn ->
      ( match !dump_outputs with
      | None -> ()
      | Some prefix ->
          let oc = open_out (prefix ^ ".error") in
          Format.fprintf
            (Format.formatter_of_out_channel oc)
            "%a@!" Location.report_exception exn;
          close_out oc );
      Format.eprintf "%a" Location.report_exception exn;
      Lwt.return (Error (-1)) )

let grade_from_dir ?(print_result = false) exercise_dir output_json =
  let exercise_dir = remove_trailing_slash exercise_dir in
  read_exercise exercise_dir
  >>= fun exo ->
  Lwt_io.(
    with_file ~mode:Input
      (String.concat Filename.dir_sep [exercise_dir; "meta.json"])
      read)
  >>= fun content ->
  let meta =
    (match content with "" -> `O [] | s -> Ezjsonm.from_string s)
    |> Json_encoding.destruct Learnocaml_data.Exercise.Meta.enc
  in
  grade ~print_result ~dirname:exercise_dir meta exo output_json
