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

(* Should stdout / stderr of the grader be echoed *)
let display_std_outputs = ref false

(* Should outputs of the grader be saved and where *)
let dump_outputs = ref None

(* Should the reports be saved and where *)
let dump_reports = ref None

(* Should the message from 'test.ml' be displayed on stdout ? *)
let display_callback = ref false

(* Should compiler outcome be printed ? *)
let display_outcomes = ref false

(* Where to put the graded exercise *)
let output_json = ref None

(* Should the tool grade 'student.ml' instead of 'solution.ml' ? *)
let grade_student = ref false

let args = Arg.align @@
  [ "-output-json", Arg.String (fun s -> output_json := Some s),
    "PATH save the graded exercise in JSON format in the given file" ;
    "-grade-student", Arg.Set grade_student,
    " grade file 'student.ml' instead of 'solution.ml'";
    "-display-outcomes", Arg.Set display_outcomes,
    " display the toplevel's outcomes" ;
    "-display-progression", Arg.Set display_callback,
    " display grading progression messages" ;
    "-display-stdouts", Arg.Set display_std_outputs,
    " display the toplevel's standard outputs" ;
    "-dump-outputs", Arg.String (fun s -> dump_outputs := Some s),
    "PREFIX save the outputs in files with the given prefix" ;
    "-dump-reports", Arg.String (fun s -> dump_reports := Some s),
    "PREFIX save the reports in files with the given prefix" ]

open Lwt.Infix

let read_exercise exercise_dir =
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

let remove_trailing_slash s =
  let len = String.length s in
  if len <> 0 && s.[len-1] = '/' then String.sub s 0 (len-1) else s

let read_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s;;

let grade exercise_dir output_json =
  Lwt.catch
    (fun () ->
       let exercise_dir = remove_trailing_slash exercise_dir in
       read_exercise exercise_dir >>= fun exo ->
       let solution = if !grade_student then
                        read_file (Filename.concat exercise_dir "student.ml")
                      else Learnocaml_exercise.(get solution) exo in
       let callback =
         if !display_callback then Some (Printf.printf "[ %s ]\n%!") else None in
       Grading_cli.get_grade ?callback exo solution
       >>= fun (result, stdout_contents, stderr_contents, outcomes) ->
       match result with
       | Error exn ->
           let dump_error ppf =
             Format.fprintf ppf "%a@." Location.report_exception exn ;
             if stdout_contents <> "" then begin
               Format.fprintf ppf "grader stdout:@.%s@." stdout_contents
             end ;
             if stderr_contents <> "" then begin
               Format.fprintf ppf "grader stderr:@.%s@." stderr_contents
             end ;
             if outcomes <> "" then begin
               Format.fprintf ppf "grader outcomes:@.%s@." outcomes
             end in
           begin match !dump_outputs with
             | None -> ()
             | Some prefix ->
                 let oc = open_out (prefix ^ ".error") in
                 dump_error (Format.formatter_of_out_channel oc) ;
                 close_out oc
           end ;
           dump_error Format.err_formatter ;
           Lwt.return 1
       | Ok report ->
           let (max, failure) = Learnocaml_report.result_of_report report in
           begin match !dump_reports with
             | None -> ()
             | Some prefix ->
                 let oc = open_out (prefix ^ ".report.txt") in
                 Learnocaml_report.print_report (Format.formatter_of_out_channel oc) report ;
                 close_out oc ;
                 let oc = open_out (prefix ^ ".report.html") in
                 Learnocaml_report.output_html_of_report (Format.formatter_of_out_channel oc) report ;
                 close_out oc
           end ;
           if stderr_contents <> "" then begin
             begin match !dump_outputs with
               | None -> ()
               | Some prefix ->
                   let oc = open_out (prefix ^ ".stderr") in
                   output_string oc stderr_contents ;
                   close_out oc
             end ;
             if !display_std_outputs then
               Format.eprintf "%s" stderr_contents
           end ;
           if stdout_contents <> "" then begin
             begin match !dump_outputs with
               | None -> ()
               | Some prefix ->
                   let oc = open_out (prefix ^ ".stdout") in
                   output_string oc stdout_contents ;
                   close_out oc
             end ;
             if !display_std_outputs then
               Format.printf "%s" stdout_contents
           end ;
           if outcomes <> "" then begin
             begin match !dump_outputs with
               | None -> ()
               | Some prefix ->
                   let oc = open_out (prefix ^ ".outcomes") in
                   output_string oc outcomes ;
                   close_out oc
             end ;
             if !display_outcomes then
               Format.printf "%s" outcomes
           end ;
           if failure && !display_callback then begin
             Printf.eprintf "Failure!\n%!" ;
             Lwt.return 2
           end
           else begin
             if !display_callback then Printf.printf "Success: %d points.\n%!" max ;
             match output_json with
             | None ->
                 Lwt.return 0
             | Some json_file ->
                 let exo = Learnocaml_exercise.(set max_score) max exo in
                 Learnocaml_exercise.write_lwt
                   ~write_field: (fun f v acc -> Lwt.return ((f, `String v) :: acc))
                   exo ~cipher:true [ "learnocaml_version", `String "1" ] >>= fun fields ->
                 Lwt_io.with_file ~mode: Lwt_io.Output json_file @@ fun chan ->
                 Lwt_io.write chan (Ezjsonm.to_string (`O fields)) >>= fun () ->
                 Lwt.return 0
           end)
    (fun exn ->
       begin match !dump_outputs with
         | None -> ()
         | Some prefix ->
             let oc = open_out (prefix ^ ".error") in
             Format.fprintf
               (Format.formatter_of_out_channel oc)
               "%a@!" Location.report_exception exn ;
             close_out oc
       end ;
       Format.eprintf "%a" Location.report_exception exn ;
       Lwt.return 1)

let main () : unit =
  let anons = ref [] in
  Arg.parse args
    (fun anon -> anons := anon :: !anons)
    "Usage: ./learnocaml-grader [options] <problem directory>" ;
  match !anons with
  | [] ->
      Format.eprintf "A problem directory is expected@." ;
      exit 1
  | _ :: _ :: _ ->
      Format.eprintf "A single problem directory is expected@." ;
      exit 1
  | [ single ] ->
      exit (Lwt_main.run (grade single !output_json))
