(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Learnocaml_data
open Tutorial
open Lwt.Infix

let lines_margin lines =
  let rec line_margin start prev line =
    if start >= String.length line then start
    else if start >= prev then prev
    else
      match line.[start] with
      | ' ' | '\t' -> line_margin (start + 1) prev line
      | _ -> start
  in
  match lines with
  | [] -> 0
  | _ -> List.fold_left (line_margin 0) max_int lines

let trailing_whitespace margin line =
  let rec loop i =
    if i < margin then i
    else match line.[i] with ' ' | '\t' -> loop (i - 1) | _ -> i
  in
  let last = max (margin - 1) (String.length line - 1) in
  last - loop last

let drop_padding_lines lines =
  let rec drop_start = function
    | [] -> []
    | line :: rest ->
        let len = String.length line in
        let rec empty i =
          if i >= len then true
          else match line.[i] with ' ' | '\t' -> empty (i + 1) | _ -> false
        in
        if empty 0 then drop_start rest else line :: rest
  in
  lines |> drop_start |> List.rev |> drop_start |> List.rev

let cut_margin margin lines =
  let rec cut acc = function
    | [] -> List.rev acc
    | line :: lines ->
        let len =
          String.length line - margin - trailing_whitespace margin line
        in
        cut (String.sub line margin len :: acc) lines
  in
  cut [] lines

let reshape_code_block code =
  let lines = Stringext.split code ~on:'\n' in
  let lines = drop_padding_lines lines in
  let margin = lines_margin lines in
  let lines = cut_margin margin lines in
  let code = String.concat "\n" lines in
  code

let parse_md_code_notation code =
  let len = String.length code in
  if String.contains code '\n' then
    let lines = Stringext.split code ~on:'\n' in
    let lines = drop_padding_lines lines in
    let margin = lines_margin lines in
    let lines = cut_margin margin lines in
    let left_line lines =
      List.fold_left
        (fun acc line ->
          if String.length line = 0 then `None
          else
            match (acc, line.[0]) with
            | `Init, c -> `Some c
            | `Some c', c when c = c' -> `Some c
            | _ -> `None )
        `Init lines
      |> function `Init | `None -> None | `Some char -> Some char
    in
    match left_line lines with
    | Some (('$' | '|') as c) -> (
        let cut_first_char line = String.sub line 1 (String.length line - 1) in
        let lines = List.map cut_first_char lines in
        let lines = drop_padding_lines lines in
        let margin = lines_margin lines in
        let lines = cut_margin margin lines in
        let code = String.concat "\n" lines in
        match c with
        | '|' -> Code {code; runnable = true}
        | '$' -> Math code
        | _ -> assert false )
    | None | Some _ ->
        let code = String.concat "\n" lines in
        Code {code; runnable = false}
  else if len > 2 && code.[0] = code.[len - 1] && code.[0] <> code.[1] then
    match code.[0] with
    | '|' ->
        let code = String.trim (String.sub code 1 (len - 2)) in
        Code {code; runnable = true}
    | '$' ->
        let code = String.trim (String.sub code 1 (len - 2)) in
        Math code
    | _ -> Code {code; runnable = false}
  else Code {code; runnable = false}

let parse_html_tutorial ~tutorial_name ~file_name =
  let fail fmt =
    Format.kasprintf
      (fun res -> Lwt.fail_with (Format.sprintf "in file %s, %s" file_name res))
      fmt
  in
  Lwt_io.(with_file ~mode:Input) file_name
  @@ fun chan ->
  Lwt_io.read chan
  >>= fun contents ->
  let tree =
    let open Markup in
    string contents |> parse_html |> signals
    |> tree
         ~text:(fun text -> `Text (String.concat "" text))
         ~element:(fun (_, name) attribs children ->
           let attribs = List.map (fun ((_, n), v) -> (n, v)) attribs in
           `Elt (name, attribs, children) )
  in
  let rec strip = function
    | `Elt ("pre", _, _) as elt -> elt
    | `Elt (name, attribs, children) ->
        let rec skip_white_space acc = function
          | [] -> List.rev acc
          | `Text "" :: rest -> skip_white_space acc rest
          | oth :: rest -> skip_white_space (oth :: acc) rest
        in
        `Elt (name, attribs, skip_white_space [] @@ List.map strip children)
    | `Text text -> `Text (String.trim text)
  in
  let rec parse_code acc = function
    | [] -> Lwt.return (String.concat "" (List.rev acc))
    | `Text text :: rest -> parse_code (text :: acc) rest
    | `Elt (tag, _, _) :: _ -> fail "unsupported markup <%s> in code" tag
  in
  let rec parse_text acc = function
    | [] -> Lwt.return (List.rev acc)
    | `Text t1 :: `Text t2 :: rest -> parse_text acc (`Text (t1 ^ t2) :: rest)
    | `Elt ("br", _, _) :: rest -> parse_text acc rest
    | `Text text :: rest ->
        let text =
          String.trim (Str.(global_replace (regexp "[ \t\n]+")) " " text)
        in
        parse_text (Text text :: acc) rest
    | `Elt (("code" | "quote"), [], children) :: rest ->
        parse_code [] children
        >>= fun code ->
        let code =
          String.trim
            (Str.(global_replace (regexp "\\( *\n[ \t]*\\)+")) " " code)
        in
        parse_text (Code {code; runnable = false} :: acc) rest
    | `Elt (("code" | "quote"), [("data-math", _)], children) :: rest ->
        parse_code [] children
        >>= fun code ->
        let code =
          String.trim
            (Str.(global_replace (regexp "\\( *\n[ \t]*\\)+")) " " code)
        in
        parse_text (Math code :: acc) rest
    | `Elt (("code" | "quote"), [("data-run", _)], children) :: rest ->
        parse_code [] children
        >>= fun code ->
        let code =
          String.trim
            (Str.(global_replace (regexp "\\( *\n[ \t]*\\)+")) " " code)
        in
        parse_text (Code {code; runnable = true} :: acc) rest
    | `Elt (("code" | "quote"), _, _) :: _ ->
        fail
          "the <code> markup expects either one data-math, one data-run or \
           zero attribute"
    | `Elt (("strong" | "em" | "b"), _, children) :: rest ->
        parse_text [] children
        >>= fun contents -> parse_text (Emph contents :: acc) rest
    | `Elt (tag, _, _) :: _ -> fail "unsupported markup <%s> in text" tag
  in
  let rec parse_contents ?(require_p = true) acc = function
    | `Elt ("p", _, children) :: rest ->
        parse_text [] children
        >>= fun contents ->
        parse_contents ~require_p (Paragraph contents :: acc) rest
    | `Elt ((("ul" | "ol") as tag), _, children) :: rest ->
        let rec parse_items tag acc = function
          | [] -> Lwt.return (List.rev acc)
          | `Elt ("li", _, children) :: rest ->
              parse_contents ~require_p:false [] children
              >>= fun contents -> parse_items tag (contents :: acc) rest
          | _ -> fail "unexpected non <li> element in <%s>" tag
        in
        parse_items tag [] children
        >>= fun items -> parse_contents ~require_p (Enum items :: acc) rest
    | `Elt ("pre", [], children) :: rest ->
        parse_code [] children
        >>= fun code ->
        let code = reshape_code_block code in
        let code_block = Code_block {code; runnable = false} in
        parse_contents ~require_p (code_block :: acc) rest
    | `Elt ("pre", [("data-run", _)], children) :: rest ->
        parse_code [] children
        >>= fun code ->
        let code = reshape_code_block code in
        let code_block = Code_block {code; runnable = true} in
        parse_contents ~require_p (code_block :: acc) rest
    | `Elt ("pre", [("data-math", _)], children) :: rest ->
        parse_code [] children
        >>= fun code ->
        let code = reshape_code_block code in
        let contents = [Math code] in
        parse_contents ~require_p (Paragraph contents :: acc) rest
    | `Elt ("pre", _, _) :: _ ->
        fail
          "the <pre> markup expects either one data-math, one data-run or \
           zero attribute"
    | `Elt (tag, _, _) :: _ as l ->
        if require_p || acc <> [] then
          fail
            "the only markups supported at toplevel are <h2>, <p>, <ul> and \
             <pre>, <%s> is not allowed"
            tag
        else parse_text [] l >>= fun text -> Lwt.return [Paragraph text]
    | `Text _ :: _ as l ->
        if require_p || acc <> [] then
          fail "text is not allowed at the toplevel, wrap it in a <p> markup"
        else parse_text [] l >>= fun text -> Lwt.return [Paragraph text]
    | [] -> Lwt.return (List.rev acc)
  in
  let rec parse_steps = function
    | acc, None, [] -> Lwt.return (List.rev acc)
    | acc, Some (step_title, sacc), [] ->
        parse_contents [] (List.rev sacc)
        >>= fun step_contents ->
        let acc = {step_title; step_contents} :: acc in
        Lwt.return (List.rev acc)
    | acc, None, `Elt ("h2", _, title) :: rest ->
        parse_text [] title
        >>= fun step_title -> parse_steps (acc, Some (step_title, []), rest)
    | _, None, _ :: _ ->
        fail
          "step title (<h2> markup) expected after the tutorial title (<h1> \
           markup)"
    | acc, Some (step_title, sacc), (`Elt ("h2", _, _) :: _ as rest) ->
        parse_contents [] (List.rev sacc)
        >>= fun step_contents ->
        let acc = {step_title; step_contents} :: acc in
        parse_steps (acc, None, rest)
    | acc, Some (step_title, sacc), elt :: rest ->
        parse_steps (acc, Some (step_title, elt :: sacc), rest)
  in
  match tree with
  | None -> fail "unparsable HTML file"
  | Some tree -> (
    match strip tree with
    | `Elt ("html", _, [`Elt ("head", _, _); `Elt ("body", _, contents)])
     |`Elt ("html", _, [`Elt ("body", _, contents)]) -> (
      match contents with
      | `Elt ("h1", _, title) :: rest ->
          parse_text [] title
          >>= fun title ->
          parse_steps ([], None, rest)
          >>= fun steps ->
          Lwt.return (Index.{title; name = tutorial_name}, {title; steps})
      | _ ->
          fail
            "tutorial title (<h1> markup) expected at the beginning of the \
             <body>" )
    | _ ->
        fail "wrong HTML structure, expecting a standard <html> with a <body>"
    )

let parse_md_tutorial ~tutorial_name ~file_name =
  let fail fmt =
    Format.kasprintf
      (fun res -> Lwt.fail_with (Format.sprintf "in file %s, %s" file_name res))
      fmt
  in
  Lwt_io.(with_file ~mode:Input) file_name
  @@ fun chan ->
  Lwt_io.read chan
  >>= fun str ->
  let strip md =
    let rec strip acc = function
      | [] -> List.rev acc
      | Omd.NL :: rest -> strip acc rest
      | oth :: rest -> strip (oth :: acc) rest
    in
    strip [] md
  in
  let rec parse_text acc = function
    | [] -> Lwt.return (List.rev acc)
    | Omd.NL :: rest -> parse_text acc rest
    | Omd.Code (_, code) :: rest ->
        let elt = parse_md_code_notation code in
        parse_text (elt :: acc) rest
    | Omd.Text t1 :: Omd.NL :: Omd.Text t2 :: rest ->
        parse_text acc (Omd.Text (t1 ^ " " ^ t2) :: rest)
    | Omd.Text t1 :: Omd.Text t2 :: rest ->
        parse_text acc (Omd.Text (t1 ^ t2) :: rest)
    | Omd.Text text :: rest -> parse_text (Text text :: acc) rest
    | Omd.Emph t :: rest | Omd.Bold t :: rest ->
        parse_text [] t >>= fun text -> parse_text (Emph text :: acc) rest
    | elt :: _ ->
        fail "unexpected content in text (%s)" (Omd.to_markdown [elt])
  in
  let rec parse_contents acc = function
    | (Omd.Ul l | Omd.Ol l | Omd.Ulp l | Omd.Olp l) :: rest ->
        Lwt_list.map_p (parse_contents []) l
        >>= fun items -> parse_contents (Enum items :: acc) rest
    | Omd.Paragraph children :: rest ->
        parse_text [] children
        >>= fun contents -> parse_contents (Paragraph contents :: acc) rest
    | Omd.Code_block (_, code) :: rest ->
        let blocks =
          List.map
            (fun code ->
              match parse_md_code_notation (code ^ "\n") with
              | Code code -> Code_block code
              | contents -> Paragraph [contents] )
            (Re.split (Re.compile (Re.str "\n\n")) code)
        in
        parse_contents (List.rev blocks @ acc) rest
    | elt :: _ ->
        fail "unexpected content at toplevel (%s)" (Omd.to_markdown [elt])
    | [] -> Lwt.return (List.rev acc)
  in
  let rec parse_steps = function
    | acc, None, [] -> Lwt.return (List.rev acc)
    | acc, Some (step_title, sacc), [] ->
        parse_contents [] (List.rev sacc)
        >>= fun step_contents ->
        let acc = {step_title; step_contents} :: acc in
        Lwt.return (List.rev acc)
    | acc, None, Omd.H2 title :: rest ->
        parse_text [] title
        >>= fun step_title -> parse_steps (acc, Some (step_title, []), rest)
    | _, None, _ :: _ ->
        fail
          "step title (<h2> markup) expected after the tutorial title (<h1> \
           markup)"
    | acc, Some (step_title, sacc), (Omd.H2 _ :: _ as rest) ->
        parse_contents [] (List.rev sacc)
        >>= fun step_contents ->
        let acc = {step_title; step_contents} :: acc in
        parse_steps (acc, None, rest)
    | acc, Some (step_title, sacc), elt :: rest ->
        parse_steps (acc, Some (step_title, elt :: sacc), rest)
  in
  match Omd.of_string str |> strip with
  | Omd.H1 title :: rest ->
      parse_text [] title
      >>= fun title ->
      parse_steps ([], None, rest)
      >>= fun steps ->
      Lwt.return (Index.{title; name = tutorial_name}, {title; steps})
  | _ -> fail "expecting a level 1 title at file beginning"

let print_html_tutorial ~tutorial_name tutorial =
  let buffer = Buffer.create 10000 in
  let ppf = Format.formatter_of_buffer buffer in
  let utf8_of_cp =
    let tmp = Buffer.create 513 in
    fun cp ->
      Buffer.clear tmp;
      Uutf.Buffer.add_utf_8 tmp cp;
      Buffer.contents tmp
  in
  let pp_escaped ppf t =
    Uutf.String.fold_utf_8
      (fun () _ cp ->
        match cp with
        | `Uchar c -> (
          match Uchar.to_int c with
          | 0x20 -> Format.fprintf ppf "@ "
          | 0x26 -> Format.fprintf ppf "&amp;"
          | 0x3C -> Format.fprintf ppf "&lt;"
          | 0x3E -> Format.fprintf ppf "&gt;"
          | 0xA0 -> Format.fprintf ppf "&nbsp;"
          | _ -> Format.fprintf ppf "%s" (utf8_of_cp c) )
        | `Malformed _ -> () )
      () t
  in
  let rec pp_text ppf = function
    | [] -> ()
    | Code {code; runnable = false} :: rest ->
        Format.fprintf ppf "<code>%a</code>" pp_escaped code;
        if rest <> [] then Format.fprintf ppf "@ ";
        pp_text ppf rest
    | Code {code; runnable = true} :: rest ->
        Format.fprintf ppf "<code data-run>%a</code>" pp_escaped code;
        if rest <> [] then Format.fprintf ppf "@ ";
        pp_text ppf rest
    | Math code :: rest ->
        Format.fprintf ppf "<script type=\"math/asciimath\">%a</script>"
          pp_escaped code;
        if rest <> [] then Format.fprintf ppf "@ ";
        pp_text ppf rest
    | Emph text :: rest ->
        Format.fprintf ppf "<em>%a</em>" pp_text text;
        if rest <> [] then Format.fprintf ppf "@ ";
        pp_text ppf rest
    | Text t :: rest ->
        pp_escaped ppf t;
        if rest <> [] then Format.fprintf ppf "@ ";
        pp_text ppf rest
    | _ -> assert false
  in
  let rec pp_content ppf = function
    | Code_block {code; runnable} ->
        Format.fprintf ppf "@[<v 2><pre%s>@,"
          (if runnable then " data-run" else "");
        let code = reshape_code_block code in
        Uutf.String.fold_utf_8
          (fun () _ cp ->
            match cp with
            | `Uchar c -> (
              match Uchar.to_int c with
              | 0x26 -> Format.fprintf ppf "&amp;"
              | 0x3C -> Format.fprintf ppf "&lt;"
              | 0x3E -> Format.fprintf ppf "&gt;"
              | 0xA0 -> Format.fprintf ppf "&nbsp;"
              | _ -> Format.fprintf ppf "%s" (utf8_of_cp c) )
            | `Malformed _ -> () )
          () code;
        Format.fprintf ppf "@]@,</pre>"
    | Paragraph text -> Format.fprintf ppf "@[<hov 2><p>%a@]</p>" pp_text text
    | Enum items ->
        let pp_item ppf contents =
          Format.fprintf ppf "@[<hov 2><li>%a@]</li>"
            (Format.pp_print_list pp_content)
            contents
        in
        Format.fprintf ppf "@[<v 2><ul>%a@]</ul>"
          (Format.pp_print_list pp_item)
          items
  in
  let pp_step ppf {step_title; step_contents} =
    Format.fprintf ppf "@[<hov 2><h2>%a</h2>@]@,%a" pp_text step_title
      (Format.pp_print_list pp_content)
      step_contents
  in
  Format.fprintf ppf
    "@[<v 2><html>@,@[<v 2><head>@,<meta \
     charset='UTF-8'>@,<title>%s</title>@]@,</head>@,@[<v 2><body>@,@[<hov \
     2><h1>%a</h1>@]@,%a@]@,</body>@]@,</html>@."
    tutorial_name pp_text tutorial.title
    (Format.pp_print_list pp_step)
    tutorial.steps;
  Buffer.contents buffer

let print_md_tutorial tutorial =
  let buffer = Buffer.create 10000 in
  let ppf = Format.formatter_of_buffer buffer in
  let pp_sep ppf () = Format.fprintf ppf "@,@," in
  let drop_newlines code =
    Stringext.split ~on:'\n' code |> List.map String.trim |> String.concat " "
  in
  let rec pp_text ppf = function
    | [] -> ()
    | Code {code; runnable = false} :: rest ->
        let code = drop_newlines code in
        let code = Omd_backend.markdown_of_md [Omd.Code ("", code)] in
        Format.fprintf ppf "%s" code;
        if rest <> [] then Format.fprintf ppf "@ ";
        pp_text ppf rest
    | Code {code; runnable = true} :: rest ->
        let code = drop_newlines code in
        let code =
          Omd_backend.markdown_of_md [Omd.Code ("", "| " ^ code ^ " |")]
        in
        Format.fprintf ppf "%s" code;
        pp_text ppf rest
    | Math code :: rest ->
        let code = drop_newlines code in
        let code =
          Omd_backend.markdown_of_md [Omd.Code ("", "$ " ^ code ^ " $")]
        in
        Format.fprintf ppf "%s" code;
        if rest <> [] then Format.fprintf ppf "@ ";
        pp_text ppf rest
    | Emph text :: rest ->
        Format.fprintf ppf "*%a*@," pp_text text;
        if rest <> [] then Format.fprintf ppf "@ ";
        pp_text ppf rest
    | Text t :: rest ->
        Format.pp_print_text ppf (Omd_backend.escape_markdown_characters t);
        if rest <> [] then Format.fprintf ppf "@ ";
        pp_text ppf rest
    | _ -> assert false
  in
  let rec pp_content ppf = function
    | Code_block {code; runnable} ->
        let prefix = if runnable then "| " else "" in
        let lines = Stringext.split ~on:'\n' code in
        Format.pp_print_list
          (fun ppf -> Format.fprintf ppf "    %s%s" prefix)
          ppf lines
    | Paragraph text -> Format.fprintf ppf "@[<hov 0>%a@]" pp_text text
    | Enum items ->
        Format.pp_print_list ~pp_sep
          (fun ppf item ->
            Format.fprintf ppf "@[<hov 4>  * %a@]"
              (Format.pp_print_list ~pp_sep pp_content)
              item )
          ppf items
  in
  let pp_step ppf {step_title; step_contents} =
    let title = Format.asprintf "@[<h 0>%a@]" pp_text step_title in
    Format.fprintf ppf "%s@,%s@,@,%a" title
      (String.make (String.length title) '-')
      (Format.pp_print_list ~pp_sep pp_content)
      step_contents
  in
  let title = Format.asprintf "@[<h 0>%a@]" pp_text tutorial.title in
  Format.fprintf ppf "@[<v 0>%s@,%s@,@,%a@." title
    (String.make (String.length title) '=')
    (Format.pp_print_list ~pp_sep pp_step)
    tutorial.steps;
  Buffer.contents buffer
