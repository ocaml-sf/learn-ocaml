(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Learnocaml_process_common
open Learnocaml_index
open Learnocaml_data

open Lwt.Infix

let tutorials_dir = ref "./tutorials"

let tutorials_index = ref None

let args = Arg.align @@
  [ "-tutorials-dir", Arg.Set_string tutorials_dir,
    "PATH path to the tutorial repository (default: [./tutorials])" ;
    "-tutorials-index", Arg.String (fun fn -> tutorials_index := Some fn),
    "PATH path to the tutorials index (default: [<tutorials-dir>/index.json])" ]

let main dest_dir =
  let tutorials_index =
    match !tutorials_index with
    | Some tutorials_index -> tutorials_index
    | None -> !tutorials_dir / "index.json" in
  let tutorials_dest_dir =
    dest_dir / Learnocaml_index.tutorials_dir in
  Lwt_utils.mkdir_p tutorials_dest_dir >>= fun () ->
  Lwt.catch
    (fun () ->
       (if Sys.file_exists tutorials_index then
          from_file Tutorial.Index.enc tutorials_index >|=
          List.map Tutorial.Index.(fun (id, s) ->
              id, (s.series_title,
                   List.map (fun e -> e.name) s.series_tutorials))
        else
          match
            Array.to_list (Sys.readdir !tutorials_dir) |>
            List.filter (fun file ->
                not (Sys.is_directory file) &&
                (Filename.check_suffix file ".md" ||
                 Filename.check_suffix file ".html"))
          with
          | [] ->
              Format.eprintf "No index file, no .md or .html file.@." ;
              Format.eprintf "This does not look like a LearnOCaml tutorial repository.@." ;
              Lwt.fail_with  "cannot continue"
          | files ->
              Format.eprintf "Missing index file, using all .md and .html files.@." ;
              Lwt.return [ "tutorials", ("All tutorials", files) ]) >>= fun series ->
       let retrieve_tutorial tutorial_name =
         let base_name = !tutorials_dir / tutorial_name in
         let md_file = base_name ^ ".md" in
         if Sys.file_exists md_file then
           Learnocaml_tutorial_parser.parse_md_tutorial
             ~tutorial_name ~file_name: md_file
         else
           let html_file = base_name ^ ".html" in
           if Sys.file_exists html_file then
             Learnocaml_tutorial_parser.parse_html_tutorial
               ~tutorial_name ~file_name: html_file
           else
             Lwt.fail_with (Format.asprintf "missing file %s.{html|md}" base_name ) in
       List.fold_left
         (fun acc (name, (series_title, tutorials)) ->
            Lwt_list.map_p
              (fun name ->
                 retrieve_tutorial name >>= fun (server_index_handle, tutorial) ->
                 let json_path = dest_dir / tutorial_path name in
                 to_file Tutorial.enc json_path tutorial >>= fun () ->
                 Lwt.return server_index_handle)
              tutorials >>= fun series_tutorials ->
            acc >>= fun acc ->
            Lwt.return ((name, Tutorial.Index.{series_title; series_tutorials})
                        :: acc))
         (Lwt.return [])
         series >>= fun index ->
       to_file Tutorial.Index.enc (dest_dir / tutorial_index_path) index >>= fun () ->
       Lwt.return true)
    (fun exn ->
       let print_unknown ppf = function
         | Failure msg -> Format.fprintf ppf "Fatal: %s" msg
         | exn -> Format.fprintf ppf "Fatal: %s"  (Printexc.to_string exn) in
       Json_encoding.print_error ~print_unknown Format.err_formatter exn ;
       Format.eprintf "@." ;
       Lwt.return false)
