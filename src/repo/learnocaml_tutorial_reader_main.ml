(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Learnocaml_data

(* If and where to export the tutorial as JSON. *)
let output_json = ref None

(* If and where to export the tutorial as HTML. *)
let output_html = ref None

(* If and where to export the tutorial as Markdown. *)
let output_md = ref None

let args =
  Arg.align
  @@ [ ( "-output-json"
       , Arg.String (fun s -> output_json := Some s)
       , "PATH export the tutorial in JSON format in the given file" )
     ; ( "-output-html"
       , Arg.String (fun s -> output_html := Some s)
       , "PATH export the tutorial in HTML format in the given file" )
     ; ( "-output-md"
       , Arg.String (fun s -> output_md := Some s)
       , "PATH export the tutorial in Markdown format in the given file" ) ]

open Lwt.Infix

let main () =
  let tutos = ref [] in
  let anon_fun tuto = tutos := tuto :: !tutos in
  let usage_msg =
    "Usage: ./learnocaml-tutorial-reader [options] <tutorial.[md|html|json]>"
  in
  Arg.parse args anon_fun usage_msg;
  exit
    (Lwt_main.run
       (Lwt.catch
          (fun () ->
            match !tutos with
            | [] ->
                Format.eprintf "A tutorial file is expected.@.";
                Lwt.return 1
            | _ :: _ :: _ ->
                Format.eprintf "A single tutorial file is expected.@.";
                Lwt.return 1
            | [file_name] ->
                let tutorial_name =
                  Filename.basename (Filename.chop_extension file_name)
                in
                ( if Filename.check_suffix file_name ".html" then
                  Learnocaml_tutorial_parser.parse_html_tutorial ~tutorial_name
                    ~file_name
                else if Filename.check_suffix file_name ".md" then
                  Learnocaml_tutorial_parser.parse_md_tutorial ~tutorial_name
                    ~file_name
                else if Filename.check_suffix file_name ".json" then
                  Lwt_io.with_file ~mode:Lwt_io.Input file_name
                  @@ fun chan ->
                  Lwt_io.read chan
                  >>= fun text ->
                  let json = Ezjsonm.from_string text in
                  let tutorial = Json_encoding.destruct Tutorial.enc json in
                  let title = tutorial.Tutorial.title in
                  Lwt.return
                    (Tutorial.Index.{name = tutorial_name; title}, tutorial)
                else
                  Lwt.fail_with
                    "unrecognized file extension, expecting .md, .html or .json"
                )
                >>= fun (_, tutorial) ->
                Lwt.join
                  [ ( match !output_html with
                    | None -> Lwt.return ()
                    | Some file_name ->
                        let text =
                          Learnocaml_tutorial_parser.print_html_tutorial
                            ~tutorial_name tutorial
                        in
                        Lwt_io.with_file ~mode:Lwt_io.Output file_name
                        @@ fun chan -> Lwt_io.write chan text )
                  ; ( match !output_md with
                    | None -> Lwt.return ()
                    | Some file_name ->
                        let text =
                          Learnocaml_tutorial_parser.print_md_tutorial tutorial
                        in
                        Lwt_io.with_file ~mode:Lwt_io.Output file_name
                        @@ fun chan -> Lwt_io.write chan text )
                  ; ( match !output_json with
                    | None -> Lwt.return ()
                    | Some file_name -> (
                        let json =
                          Json_encoding.construct Tutorial.enc tutorial
                        in
                        match json with
                        | (`O _ | `A _) as json ->
                            Lwt_io.with_file ~mode:Lwt_io.Output file_name
                            @@ fun chan ->
                            let text = Ezjsonm.to_string json in
                            Lwt_io.write chan text
                        | _ -> assert false ) ) ]
                >>= fun () -> Lwt.return 0 )
          (fun exn ->
            let print_unknown ppf = function
              | Failure msg ->
                  Format.fprintf ppf "Cannot process tutorial: %s" msg
              | exn ->
                  Format.fprintf ppf "Cannot process tutorial: %s"
                    (Printexc.to_string exn)
            in
            Json_encoding.print_error ~print_unknown Format.err_formatter exn;
            Format.eprintf "@.";
            Lwt.return 1 )))

let () = main ()
