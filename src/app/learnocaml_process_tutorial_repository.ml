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


open Server_index

open Lwt.Infix

let parse_html_tutorial tutorial_name filename =
  Lwt_io.(with_file ~mode: Input) filename @@ fun chan ->
  Lwt_io.read chan >>= fun contents ->
  let tree =
    let open Markup in
    string contents |> parse_html |> signals |> tree
      ~text: (fun text -> `Text text)
      ~element: (fun (_, name) _ children -> `Elt (name, children)) in
  let rec strip = function
    | `Elt (name, children) ->
        let rec skip_white_space acc = function
          | [] -> List.rev acc
          | `Text "" :: rest -> skip_white_space acc rest
          | oth :: rest -> skip_white_space (oth :: acc) rest in
        `Elt (name, skip_white_space [] @@ List.map strip children)
    | `Text text -> `Text (String.concat "" (List.map String.trim text)) in
  match tree with
  | None -> Lwt.fail_with ("cannot parse " ^ filename)
  | Some tree -> match strip tree with
    | `Elt ("html", [ `Elt ("head", _) ; `Elt ("body", contents) ])
    | `Elt ("html", [ `Elt ("body", contents) ]) ->
        begin match contents with
          | `Elt ("h1", title) :: rest ->
              let rec parse_code acc = function
                | [] -> Lwt.return (String.concat "" (List.rev acc))
                | `Text text :: rest ->
                    parse_code (text :: acc) rest
                | `Elt (tag, _) :: _ ->
                    let msg = "unsupported markup " ^ tag ^ " in code" in
                    Lwt.fail_with (Format.asprintf "in file %s, %s" filename msg) in
              let rec parse_text acc = function
                | [] -> Lwt.return (List.rev acc)
                | `Text t1 :: `Text t2 :: rest ->
                    parse_text acc (`Text (t1 ^ t2) :: rest)
                | `Elt ("br", _) :: rest ->
                    parse_text acc rest
                | `Text text :: rest ->
                    parse_text (Text text :: acc) rest
                | `Elt (("code" | "quote"), children) :: rest ->
                    parse_code [] children >>= fun code ->
                    parse_text (Code { code ; runnable = false } :: acc) rest
                | `Elt (("strong" | "em" | "b"), children) :: rest ->
                    parse_text [] children >>= fun contents ->
                    parse_text (Emph contents :: acc) rest
                | `Elt (tag, _) :: _ ->
                    let msg = "unsupported markup " ^ tag ^ " in text" in
                    Lwt.fail_with (Format.asprintf "in file %s, %s" filename msg) in
              let rec parse_contents acc = function
                | `Elt ("p", children) :: rest ->
                    parse_text [] children >>= fun contents ->
                    parse_contents (Tutorial.Paragraph contents :: acc) rest
                | `Elt ("pre", [ `Text code]) :: rest ->
                    let contents = [ Code { code ; runnable = false } ] in
                    parse_contents (Tutorial.Paragraph contents :: acc) rest
                | `Elt (tag, _) :: _ ->
                    let msg = "the only markups supported at toplevel are \
                               h2, p, ul and pre, " ^ tag ^ "is not allowed" in
                    Lwt.fail_with (Format.asprintf "in file %s, %s" filename msg)
                | `Text _ :: _ ->
                    let msg = "text is not allowed at the toplevel, \
                               use a p markup" in
                    Lwt.fail_with (Format.asprintf "in file %s, %s" filename msg)
                | [] -> Lwt.return (List.rev acc) in
              let rec parse_steps = function
                | acc, None, []  ->
                    Lwt.return (List.rev acc)
                | acc, Some (step_title, sacc), [] ->
                    parse_contents [] (List.rev sacc) >>= fun step_contents ->
                    let acc = Tutorial.{ step_title ; step_contents } :: acc in
                    Lwt.return (List.rev acc)
                | acc, None, `Elt ("h2", title) :: rest ->
                    parse_text [] title >>= fun step_title ->
                    parse_steps (acc, Some (step_title, []), rest)
                | acc, None, elt :: rest ->
                    let msg = "step title (h2 markup) expected \
                               after the tutorial title (h1 markup)" in
                    Lwt.fail_with (Format.asprintf "in file %s, %s" filename msg)
                | acc, Some (step_title, sacc), (`Elt ("h2", _) :: _ as rest) ->
                    parse_contents [] (List.rev sacc) >>= fun step_contents ->
                    let acc = Tutorial.{ step_title ; step_contents } :: acc in
                    parse_steps (acc, None, rest)
                | acc, Some (step_title, sacc), elt :: rest ->
                    parse_steps (acc, Some (step_title, elt :: sacc), rest)
              in
              parse_text [] title >>= fun tutorial_title ->
              parse_steps ([], None, rest) >>= fun tutorial_steps ->
              Lwt.return
                (Server_index.{ tutorial_title ; tutorial_name },
                 Tutorial.{ tutorial_title ; tutorial_steps })
          | _ ->
              let msg = "tutorial title (h1 markup) expected \
                         at the beginning of the body" in
              Lwt.fail_with (Format.asprintf "in file %s, %s" filename msg)
        end
    | _ -> Lwt.fail_with ("bad HTML structure for " ^ filename)

let parse_md_tutorial tutorial_name filename =
  Lwt_io.(with_file ~mode: Input) filename @@ fun chan ->
  Lwt_io.read chan >>= fun str ->
  let strip md =
    let rec strip acc = function
      | [] -> List.rev acc
      | Omd.NL :: rest -> strip acc rest
      | oth :: rest -> strip (oth :: acc) rest in
    strip [] md in
  let rec parse_title md =
    let rec parse acc = function
      | [] -> Lwt.return (List.rev acc)
      | Omd.NL :: rest -> parse acc rest
      | Omd.Code (_, text) :: rest ->
          let elt =
            if String.length text >= 2 &&
               String.get text 0 = '|' &&
               String.get text (String.length text - 1) = '|'  then
              Code { code = String.sub text 1 (String.length text - 2) ;
                     runnable = true }
            else
            if String.length text >= 2 &&
               String.get text 0 = '$' &&
               String.get text (String.length text - 1) = '$'  then
              Math (String.sub text 1 (String.length text - 2))
            else
              Code { code = text ; runnable = false } in
          parse (elt :: acc) rest
      | Omd.Text t1 :: Omd.Text t2 :: rest ->
          parse acc (Omd.Text (t1 ^ t2) :: rest)
      | Omd.Text text :: rest ->
          parse (Text text :: acc) rest
      | Omd.Emph t :: rest | Omd.Bold t :: rest ->
          parse_title t >>= fun text ->
          parse (Emph text :: acc) rest
      | _ ->
          let msg = "unexpected element in title" in
          Lwt.fail_with (Format.asprintf "in file %s, %s" filename msg) in
    parse [] md in
  match Omd.of_string str |> strip with
  | Omd.H1 title :: contents ->
      parse_title title >>= fun tutorial_title ->
      Lwt.return
        (Server_index.{ tutorial_title ; tutorial_name },
         Tutorial.{ tutorial_title ; tutorial_steps = [] })
  | _ ->
      let msg = "files must start with a level 1 title" in
      Lwt.fail_with (Format.asprintf "in file %s, %s" filename msg)

let tutorials_dir = ref "./tutorials"

let args = Arg.align @@
  [ "-tutorials-dir", Arg.Set_string tutorials_dir,
    "PATH path to the tutorial repository (default: [./tutorials])" ]

let index_enc =
  let open Json_encoding in
  let series_enc =
    obj2
      (req "title" string)
      (req "tutorials" (list string)) in
  check_version_1 @@
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
  Lwt.catch
    (fun () ->
       let (/) dir f =
         String.concat Filename.dir_sep [ dir ; f ] in
       (if Sys.file_exists (!tutorials_dir / "index.json") then
          from_file index_enc (!tutorials_dir / "index.json")
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
       let retrieve_tutorial name =
         let base_name = !tutorials_dir / name in
         let md_file = base_name ^ ".md" in
         if Sys.file_exists md_file then
           parse_md_tutorial name md_file
         else
           let html_file = base_name ^ ".html" in
           if Sys.file_exists html_file then
             parse_html_tutorial name html_file
           else
             Lwt.fail_with (Format.asprintf "missing file %s.{html|md}" base_name ) in
       List.fold_left
         (fun acc (name, (series_title, tutorials)) ->
            Lwt_list.map_p
              (fun name ->
                 retrieve_tutorial name >>= fun (server_index_handle, tutorial) ->
                 let json_path = dest_dir / "tutorial_" ^ name ^ ".json" in
                 to_file Tutorial.tutorial_enc json_path tutorial >>= fun () ->
                 Lwt.return server_index_handle)
              tutorials >>= fun series_tutorials ->
            acc >>= fun acc ->
            Lwt.return (StringMap.add name { series_title ; series_tutorials } acc))
         (Lwt.return StringMap.empty)
         series >>= fun index ->
       to_file tutorial_index_enc (dest_dir / "tutorials.json") index >>= fun () ->
       Lwt.return true)
    (fun exn ->
       let print_unknown ppf = function
         | Failure msg -> Format.fprintf ppf "Fatal: %s" msg
         | exn -> Format.fprintf ppf "Fatal: %s"  (Printexc.to_string exn) in
       Json_encoding.print_error ~print_unknown Format.err_formatter exn ;
       Format.eprintf "@." ;
       Lwt.return false)
