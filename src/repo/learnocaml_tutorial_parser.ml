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

let lines_margin lines =
  let rec line_margin start prev line =
    if start >= String.length line then start
    else if start >= prev then prev
    else match String.get line start with
      | ' ' | '\t' -> line_margin (start + 1) prev line
      | _ -> start in
  match lines with
  | [] -> 0
  | _ -> List.fold_left (line_margin 0) max_int lines

let trailing_whitespace margin line =
  let rec loop i =
    if i < margin then i
    else match String.get line i with
      | ' ' | '\t' -> loop (i - 1)
      | _ -> i in
  let last = max (margin - 1) (String.length line - 1) in
  last - loop last

let drop_padding_lines lines =
  let rec drop_start = function
    | [] -> []
    | line :: rest ->
        let len = String.length line in
        let rec empty i =
          if i >= len then true
          else match String.get line i with
            | ' ' | '\t' -> empty (i + 1)
            | _ -> false in
        if empty 0 then drop_start rest else line :: rest in
  lines |> drop_start |> List.rev |> drop_start |> List.rev

let cut_margin margin lines =
  let rec cut acc = function
    | [] -> List.rev acc
    | line :: lines ->
        let len = String.length line
                  - margin
                  - trailing_whitespace margin line in
        cut (String.sub line margin len :: acc) lines
  in cut [] lines

let reshape_code_block code =
  let lines = Stringext.split code ~on: '\n' in
  let lines = drop_padding_lines lines in
  let margin = lines_margin lines in
  let lines = cut_margin margin lines in
  let code = String.concat "\n" lines in
  code

let parse_md_code_notation code =
  let len = String.length code in
  if String.contains code '\n' then
    let lines = Stringext.split code ~on: '\n' in
    let lines = drop_padding_lines lines in
    let margin = lines_margin lines in
    let lines = cut_margin margin lines in
    let left_line lines =
      List.fold_left
        (fun acc line ->
           if String.length line = 0 then
             `None
           else
             match acc, String.get line 0 with
             | `Init, c -> `Some c
             | `Some c', c when c = c' -> `Some c
             | _ -> `None) `Init lines |> function
      | `Init | `None -> None
      | `Some char -> Some char in
    match left_line lines with
    | Some ('$' | '|' as c) ->
        let cut_first_char line =
          String.sub line 1 (String.length line - 1) in
        let lines = List.map cut_first_char lines in
        let lines = drop_padding_lines lines in
        let margin = lines_margin lines in
        let lines = cut_margin margin lines in
        let code = String.concat "\n" lines in
        begin match c with
          | '|' -> Code { code ; runnable = true }
          | '$' -> Math code
          | _ -> assert false end
    | None | Some _ ->
        let code = String.concat "\n" lines in
        Code { code ; runnable = false }
  else if len > 2
       && (String.get code 0 = String.get code (len - 1))
       && (String.get code 0 <> String.get code 1) then
    match String.get code 0 with
    | '|' ->
        let code = String.trim (String.sub code 1 (len - 2)) in
        Code { code ; runnable = true }
    | '$' ->
        let code = String.trim (String.sub code 1 (len - 2)) in
        Math code
    | _ -> Code { code ; runnable = false }
  else
    Code { code ; runnable = false }

let parse_html_tutorial ~tutorial_name ~file_name =
  let fail fmt =
    Format.kasprintf
      (fun res -> Lwt.fail_with (Format.sprintf "in file %s, %s" file_name res))
      fmt in
  Lwt_io.(with_file ~mode: Input) file_name @@ fun chan ->
  Lwt_io.read chan >>= fun contents ->
  let tree =
    let open Markup in
    string contents |> parse_html |> signals |> tree
      ~text: (fun text -> `Text (String.concat "" text))
      ~element: (fun (_, name) attribs children ->
          let attribs = List.map (fun ((_, n), v) -> (n, v)) attribs in
          `Elt (name, attribs, children)) in
  let rec strip = function
    | `Elt ("pre", _, children) as elt -> elt
    | `Elt (name, attribs, children) ->
        let rec skip_white_space acc = function
          | [] -> List.rev acc
          | `Text "" :: rest -> skip_white_space acc rest
          | oth :: rest -> skip_white_space (oth :: acc) rest in
        `Elt (name, attribs, skip_white_space [] @@ List.map strip children)
    | `Text text -> `Text (String.trim text) in
  let rec parse_code acc = function
    | [] -> Lwt.return (String.concat "" (List.rev acc))
    | `Text text :: rest ->
        parse_code (text :: acc) rest
    | `Elt (tag, _, _) :: _ ->
        fail "unsupported markup <%s> in code" tag in
  let rec parse_text acc = function
    | [] -> Lwt.return (List.rev acc)
    | `Text t1 :: `Text t2 :: rest ->
        parse_text acc (`Text (t1 ^ t2) :: rest)
    | `Elt ("br", _, _) :: rest ->
        parse_text acc rest
    | `Text text :: rest ->
        let text = String.trim (Str.(global_replace (regexp "[ \t\n]+")) " " text) in
        parse_text (Text text :: acc) rest
    | `Elt (("code" | "quote"), [], children) :: rest ->
        parse_code [] children >>= fun code ->
        let code = String.trim (Str.(global_replace (regexp "\\( *\n[ \t]*\\)+")) " " code) in
        parse_text (Code { code ; runnable = false } :: acc) rest
    | `Elt (("code" | "quote"), [ "data-math", _ ], children) :: rest ->
        parse_code [] children >>= fun code ->
        let code = String.trim (Str.(global_replace (regexp "\\( *\n[ \t]*\\)+")) " " code) in
        parse_text (Math code :: acc) rest
    | `Elt (("code" | "quote"), [ "data-run", _ ], children) :: rest ->
        parse_code [] children >>= fun code ->
        let code = String.trim (Str.(global_replace (regexp "\\( *\n[ \t]*\\)+")) " " code) in
        parse_text (Code { code ; runnable = true } :: acc) rest
    | `Elt (("code" | "quote"), _ , children) :: rest ->
        fail "the <code> markup expects either \
              one data-math, one data-run or zero attribute"
    | `Elt (("strong" | "em" | "b"), _, children) :: rest ->
        parse_text [] children >>= fun contents ->
        parse_text (Emph contents :: acc) rest
    | `Elt (tag, _, _) :: _ ->
        fail "unsupported markup <%s> in text" tag in
  let rec parse_contents acc = function
    | `Elt ("p", _, children) :: rest ->
        parse_text [] children >>= fun contents ->
        parse_contents (Learnocaml_tutorial.Paragraph contents :: acc) rest
    | `Elt ("pre", [], children) :: rest ->
        parse_code [] children >>= fun code ->
        let code = reshape_code_block code in
        let contents = [ Code { code ; runnable = false } ] in
        parse_contents (Learnocaml_tutorial.Paragraph contents :: acc) rest
    | `Elt ("pre", [ "data-run", _ ], children) :: rest ->
        parse_code [] children >>= fun code ->
        let code = reshape_code_block code in
        let contents = [ Code { code ; runnable = true } ] in
        parse_contents (Learnocaml_tutorial.Paragraph contents :: acc) rest
    | `Elt ("pre", [ "data-math", _ ], children) :: rest ->
        parse_code [] children >>= fun code ->
        let code = reshape_code_block code in
        let contents = [ Math code ] in
        parse_contents (Learnocaml_tutorial.Paragraph contents :: acc) rest
    | `Elt ("pre", _ , children) :: rest ->
        fail "the <pre> markup expects either \
              one data-math, one data-run or zero attribute"
    | `Elt (tag, _, _) :: _ ->
        fail "the only markups supported at toplevel are \
              <h2>, <p>, <ul> and <pre>, <%s> is not allowed" tag
    | `Text _ :: _ ->
        fail "text is not allowed at the toplevel, \
              wrap it in a <p> markup"
    | [] -> Lwt.return (List.rev acc) in
  let rec parse_steps = function
    | acc, None, []  ->
        Lwt.return (List.rev acc)
    | acc, Some (step_title, sacc), [] ->
        parse_contents [] (List.rev sacc) >>= fun step_contents ->
        let acc = Learnocaml_tutorial.{ step_title ; step_contents } :: acc in
        Lwt.return (List.rev acc)
    | acc, None, `Elt ("h2", _, title) :: rest ->
        parse_text [] title >>= fun step_title ->
        parse_steps (acc, Some (step_title, []), rest)
    | acc, None, elt :: rest ->
        fail "step title (<h2> markup) expected \
                after the tutorial title (<h1> markup)"
    | acc, Some (step_title, sacc), (`Elt ("h2", _, _) :: _ as rest) ->
        parse_contents [] (List.rev sacc) >>= fun step_contents ->
        let acc = Learnocaml_tutorial.{ step_title ; step_contents } :: acc in
        parse_steps (acc, None, rest)
    | acc, Some (step_title, sacc), elt :: rest ->
        parse_steps (acc, Some (step_title, elt :: sacc), rest) in
  match tree with
  | None -> fail "unparsable HTML file"
  | Some tree -> match strip tree with
    | `Elt ("html", _, [ `Elt ("head", _, _) ; `Elt ("body", _, contents) ])
    | `Elt ("html", _, [ `Elt ("body", _, contents) ]) ->
        begin match contents with
          | `Elt ("h1", _, title) :: rest ->
              parse_text [] title >>= fun tutorial_title ->
              parse_steps ([], None, rest) >>= fun tutorial_steps ->
              Lwt.return
                (Learnocaml_index.{ tutorial_title ; tutorial_name },
                 Learnocaml_tutorial.{ tutorial_title ; tutorial_steps })
          | _ ->
              fail "tutorial title (<h1> markup) expected \
                    at the beginning of the <body>"
        end
    | _ -> fail "wrong HTML structure, \
                 expecting a standard <html> with a <body>"

let parse_md_tutorial ~tutorial_name ~file_name =
  let fail fmt =
    Format.kasprintf
      (fun res -> Lwt.fail_with (Format.sprintf "in file %s, %s" file_name res))
      fmt in
  Lwt_io.(with_file ~mode: Input) file_name @@ fun chan ->
  Lwt_io.read chan >>= fun str ->
  let strip md =
    let rec strip acc = function
      | [] -> List.rev acc
      | Omd.NL :: rest -> strip acc rest
      | oth :: rest -> strip (oth :: acc) rest in
    strip [] md in
  let rec parse_text acc = function
    | [] -> Lwt.return (List.rev acc)
    | Omd.NL :: rest -> parse_text acc rest
    | Omd.Code (_, code) :: rest ->
        let elt = parse_md_code_notation code in
        parse_text (elt :: acc) rest
    | Omd.Text t1 :: Omd.Text t2 :: rest ->
        parse_text acc (Omd.Text (t1 ^ t2) :: rest)
    | Omd.Text text :: rest ->
        parse_text (Text text :: acc) rest
    | Omd.Emph t :: rest | Omd.Bold t :: rest ->
        parse_text [] t >>= fun text ->
        parse_text (Emph text :: acc) rest
    | elt :: _  ->
        fail "unexpected content in title (%s)"
          (Omd.to_markdown [ elt ]) in
  let rec parse_contents acc = function
    | Omd.Paragraph children :: rest ->
        parse_text [] children >>= fun contents ->
        parse_contents (Learnocaml_tutorial.Paragraph contents :: acc) rest
    | Omd.Code_block (_, code) :: rest ->
        let contents = [ parse_md_code_notation (code ^ "\n") ] in
        parse_contents (Learnocaml_tutorial.Paragraph contents :: acc) rest
    | elt :: _ ->
        fail "unexpected content at toplevel (%s)"
          (Omd.to_markdown [ elt ])
    | [] -> Lwt.return (List.rev acc) in
  let rec parse_steps = function
    | acc, None, []  ->
        Lwt.return (List.rev acc)
    | acc, Some (step_title, sacc), [] ->
        parse_contents [] (List.rev sacc) >>= fun step_contents ->
        let acc = Learnocaml_tutorial.{ step_title ; step_contents } :: acc in
        Lwt.return (List.rev acc)
    | acc, None, Omd.H2 title :: rest ->
        parse_text [] title >>= fun step_title ->
        parse_steps (acc, Some (step_title, []), rest)
    | acc, None, elt :: rest ->
        fail "step title (<h2> markup) expected \
              after the tutorial title (<h1> markup)"
    | acc, Some (step_title, sacc), (Omd.H2 _ :: _ as rest) ->
        parse_contents [] (List.rev sacc) >>= fun step_contents ->
        let acc = Learnocaml_tutorial.{ step_title ; step_contents } :: acc in
        parse_steps (acc, None, rest)
    | acc, Some (step_title, sacc), elt :: rest ->
        parse_steps (acc, Some (step_title, elt :: sacc), rest) in
  match Omd.of_string str |> strip with
  | Omd.H1 title :: rest ->
      parse_text [] title >>= fun tutorial_title ->
      parse_steps ([], None, rest) >>= fun tutorial_steps ->
      Lwt.return
        (Learnocaml_index.{ tutorial_title ; tutorial_name },
         Learnocaml_tutorial.{ tutorial_title ; tutorial_steps })
  | _ ->
      fail "expecting a level 1 title at file beginning"

let print_html_tutorial ~tutorial_name tutorial =
  let open Learnocaml_tutorial in
  let buffer = Buffer.create 10000 in
  let ppf = Format.formatter_of_buffer buffer in
  let { tutorial_title ; tutorial_steps } = tutorial in
  let utf8_of_cp =
    let tmp = Buffer.create 513 in
    fun cp ->
      Buffer.clear tmp ;
      Uutf.Buffer.add_utf_8 tmp cp ;
      Buffer.contents tmp in
  let pp_escaped ppf t =
    Uutf.String.fold_utf_8 (fun () _ cp ->
        match cp with
        | `Uchar 0x20 -> Format.fprintf ppf "@ "
        | `Uchar 0x26 -> Format.fprintf ppf "&amp;"
        | `Uchar 0x3C -> Format.fprintf ppf "&lt;"
        | `Uchar 0x3E -> Format.fprintf ppf "&gt;"
        | `Uchar 0xA0 -> Format.fprintf ppf "&nbsp;"
        | `Uchar cp -> Format.fprintf ppf "%s" (utf8_of_cp cp)
        | `Malformed _ -> ())
      () t in
  let rec pp_text ppf = function
    | [] -> ()
    |  Code { code ; runnable = false} :: rest ->
        Format.fprintf ppf "<code>%a</code>" pp_escaped code ;
        if rest <> [] then Format.fprintf ppf "@ " ;
        pp_text ppf rest
    |  Code { code ; runnable = true} :: rest ->
        Format.fprintf ppf "<code data-run>%a</code>" pp_escaped code ;
        if rest <> [] then Format.fprintf ppf "@ " ;
        pp_text ppf rest
    |  Math code :: rest ->
        Format.fprintf ppf "<code data-math>%a</code>" pp_escaped code ;
        if rest <> [] then Format.fprintf ppf "@ " ;
        pp_text ppf rest
    |  Emph text :: rest ->
        Format.fprintf ppf "<em>%a</em>@," pp_text text ;
        if rest <> [] then Format.fprintf ppf "@ " ;
        pp_text ppf rest
    | Text t :: rest ->
        pp_escaped ppf t ;
        if rest <> [] then Format.fprintf ppf "@ " ;
        pp_text ppf rest
    | _ -> assert false in
  let rec pp_content ppf = function
    | Paragraph [ Code { code ; runnable} ] ->
        Format.fprintf ppf "@[<v 2><pre%s>@," (if runnable then " data-run" else "") ;
        let code = reshape_code_block code in
        Uutf.String.fold_utf_8 (fun () _ cp ->
            match cp with
            | `Uchar 0x0A -> Format.fprintf ppf "@,"
            | `Uchar 0x26 -> Format.fprintf ppf "&amp;"
            | `Uchar 0x3C -> Format.fprintf ppf "&lt;"
            | `Uchar 0x3E -> Format.fprintf ppf "&gt;"
            | `Uchar 0xA0 -> Format.fprintf ppf "&nbsp;"
            | `Uchar cp -> Format.fprintf ppf "%s" (utf8_of_cp cp)
            | `Malformed _ -> ())
          () code ;
        Format.fprintf ppf "@]@,</pre>"
    | Paragraph text ->
        Format.fprintf ppf "@[<hov 2><p>%a@]</p>" pp_text text
    | Enum items ->
        let pp_item ppf text =
          Format.fprintf ppf "@[<hov 2><p>%a@]</p>" pp_text text in
        Format.fprintf ppf "@[<v 2><ul>%a@]</ul>"
          (Format.pp_print_list pp_item) items in
  let pp_step ppf { step_title ; step_contents } =
    Format.fprintf ppf "@[<hov 2><h2>%a</h2>@]@,%a"
      pp_text step_title
      (Format.pp_print_list pp_content) step_contents in
  Format.fprintf ppf "@[<v 2><html>@,\
                      @[<v 2><head>@]@,\
                      <meta charset='UTF-8'>@,\
                      <title>%s</title>@,\
                      </head>@]@,\
                      @[<v 2><body>@,\
                      @[<hov 2><h1>%a</h1>@]@,\
                      %a@]@,\
                      </body>@]@,\
                      </html>@."
    tutorial_name
    pp_text tutorial_title
    (Format.pp_print_list pp_step) tutorial_steps ;
  Buffer.contents buffer

let print_md_tutorial ~tutorial_name tutorial =
  let open Learnocaml_tutorial in
  let buffer = Buffer.create 10000 in
  let ppf = Format.formatter_of_buffer buffer in
  let { tutorial_title ; tutorial_steps } = tutorial in
  let pp_sep ppf () = Format.fprintf ppf "@,@," in
  let drop_newlines code =
    Stringext.split ~on:'\n' code
    |> List.map String.trim
    |> String.concat " " in
  let rec pp_text ppf = function
    | [] -> ()
    |  Code { code ; runnable = false} :: rest ->
        let code = drop_newlines code in
        let code = Omd_backend.markdown_of_md [ Omd.Code ("", code) ] in
        Format.fprintf ppf "%s" code ;
        if rest <> [] then Format.fprintf ppf "@ " ;
        pp_text ppf rest
    |  Code { code ; runnable = true} :: rest ->
        let code = drop_newlines code in
        let code = Omd_backend.markdown_of_md [ Omd.Code ("", "| " ^ code ^ " |") ] in
        Format.fprintf ppf "%s" code ;
        pp_text ppf rest
    |  Math code :: rest ->
        let code = drop_newlines code in
        let code = Omd_backend.markdown_of_md [ Omd.Code ("", "$ " ^ code ^ " $") ] in
        Format.fprintf ppf "%s" code ;
        if rest <> [] then Format.fprintf ppf "@ " ;
        pp_text ppf rest
    |  Emph text :: rest ->
        Format.fprintf ppf "*%a*@," pp_text text ;
        if rest <> [] then Format.fprintf ppf "@ " ;
        pp_text ppf rest
    | Text t :: rest ->
        Format.pp_print_text ppf
          (Omd_backend.escape_markdown_characters t) ;
        if rest <> [] then Format.fprintf ppf "@ " ;
        pp_text ppf rest
    | _ -> assert false in
  let rec pp_content ppf = function
    | Paragraph [ Code { code ; runnable} ] ->
        let prefix = if runnable then "| " else "" in
        let lines = Stringext.split ~on:'\n' code in
        Format.pp_print_list
          (fun ppf -> Format.fprintf ppf "    %s%s" prefix)
          ppf lines
    | Paragraph text ->
        Format.fprintf ppf "@[<hov 0>%a@]" pp_text text
    | Enum items ->
        Format.pp_print_list ~pp_sep
          (fun ppf item ->
             Format.fprintf ppf "@[<hov 4>  * %a@]" pp_text item)
          ppf items in
  let pp_step ppf { step_title ; step_contents } =
    let title = Format.asprintf "@[<h 0>%a@]" pp_text step_title in
    Format.fprintf ppf "%s@,%s@,@,%a"
      title (String.make (String.length title) '-')
      (Format.pp_print_list ~pp_sep pp_content) step_contents in
  let title = Format.asprintf "@[<h 0>%a@]" pp_text tutorial_title in
  Format.fprintf ppf "@[<v 0>\
                      %s@,%s@,@,\
                      %a@."
    title (String.make (String.length title) '=')
    (Format.pp_print_list ~pp_sep pp_step) tutorial_steps ;
  Buffer.contents buffer
