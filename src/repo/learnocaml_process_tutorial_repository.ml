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

let tutorials_dir = ref "./tutorials"

let tutorials_index = ref None

let args = Arg.align @@
  [ "-tutorials-dir", Arg.Set_string tutorials_dir,
    "PATH path to the tutorial repository (default: [./tutorials])" ;
    "-tutorials-index", Arg.String (fun fn -> tutorials_index := Some fn),
    "PATH path to the tutorials index (default: [<tutorials-dir>/index.json])" ]

let index_enc =
  let open Json_encoding in
  let series_enc =
    obj2
      (req "title" string)
      (req "tutorials" (list string)) in
  check_version_2 @@
  obj1 (req "series" (assoc series_enc))

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

let main dest_dir =
  let (/) dir f =
    String.concat Filename.dir_sep [ dir ; f ] in
  let tutorials_index =
    match !tutorials_index with
    | Some tutorials_index -> tutorials_index
    | None -> !tutorials_dir / "index.json" in
  let tutorials_dest_dir =
    dest_dir / Learnocaml_index.tutorials_dir in
  Lwt_unix.file_exists tutorials_dest_dir >>= fun exists ->
  (if exists then Lwt.return_unit
   else Lwt_unix.mkdir tutorials_dest_dir 0o755) >>= fun () ->
  Lwt.catch
    (fun () ->
       (if Sys.file_exists tutorials_index then
          from_file index_enc tutorials_index
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
              Format.eprintf "Missing index file, using all .dm and .html files.@." ;
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
                 to_file Learnocaml_tutorial.tutorial_enc json_path tutorial >>= fun () ->
                 Lwt.return server_index_handle)
              tutorials >>= fun series_tutorials ->
            acc >>= fun acc ->
            Lwt.return (StringMap.add name { series_title ; series_tutorials } acc))
         (Lwt.return StringMap.empty)
         series >>= fun index ->
       to_file tutorial_index_enc (dest_dir / tutorial_index_path) index >>= fun () ->
       Lwt.return true)
    (fun exn ->
       let print_unknown ppf = function
         | Failure msg -> Format.fprintf ppf "Fatal: %s" msg
         | exn -> Format.fprintf ppf "Fatal: %s"  (Printexc.to_string exn) in
       Json_encoding.print_error ~print_unknown Format.err_formatter exn ;
       Format.eprintf "@." ;
       Lwt.return false)
