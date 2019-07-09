(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(* -- minimal HTML producer ------------------------------------------------- *)

type html = elt list
and elt =
  | C of string
  | T of string
  | I of string
  | E of string * attr list * html
  | S of string * attr list
and attr = string * string

let rec output_html ppf = function
  | elt :: elts ->
    output_elt ppf elt ;
    begin match elt, elts with
      | (E _ | S _ | C _), (T _ | I _) :: _
      | (T _ | I _), (E _ | S _ | C _) :: _
      | (T _ | I _), (T _ | I _) :: _ ->
        Format.fprintf ppf " "
      | _ -> () end ;
    output_html ppf elts
  | [] -> ()

and output_text ppf text =
  for i = 0 to String.length text - 1 do
    match String.get text i with
    | '"' -> Format.fprintf ppf "&quot;"
    | '<' -> Format.fprintf ppf "&lt;"
    | '>' -> Format.fprintf ppf "&gt;"
    | '&' -> Format.fprintf ppf "&amp;"
    | c -> Format.fprintf ppf "%c" c
  done

and output_elt ppf = function
  | T text ->
    Format.fprintf ppf "%a" output_text text
  | I text ->
    Format.fprintf ppf "&%s;" text
  | C text ->
    Format.fprintf ppf "<!--%s-->" text
  | S (name, attrs) ->
    Format.fprintf ppf "<%s%a/>"
      name output_attrs attrs
  | E ("script", attrs, [ C text ]) ->
    Format.fprintf ppf "<script%a>//<!--\n%s\n//--></script>"
      output_attrs attrs text
  | E ("style", attrs, [ C text ]) ->
    Format.fprintf ppf "<style%a>/*<!--*/\n.workaroundfunbug {}\n%s/*-->*/</style>"
      output_attrs attrs text
  | E (name, attrs, html) ->
    Format.fprintf ppf "<%s%a>%a</%s>"
      name output_attrs attrs output_html html name

and output_attrs ppf attrs =
  List.iter (fun (n, v) ->
      if String.contains v '"' then
        Printf.eprintf "Error: double quote in attribute value.\n%!"
      else
        Format.fprintf ppf " %s=\"%a\"" n output_text v)
    attrs

(* -- report format --------------------------------------------------------- *)

type t = item list

and item =
  | Section of text * t
  | SectionMin of text * t * int
  | Message of text * status

and status =
  | Success of int | Penalty of int | Failure
  | Warning | Informative | Important

and text = inline list

and inline =
  | Text of string
  | Break
  | Code of string
  | Output of string

let result items =
  let rec do_report items =
    List.fold_left (fun (successes, failures) item ->
        let (isuccesses, ifailures) = do_item item in
        (successes + isuccesses, failures || ifailures))
      (0, false) items
  and do_item = function
    | Message (_text, status) ->
        begin match status with
          | Success n -> (n, false)
          | Penalty n -> (-n, true)
          | Failure -> (0, true)
          | Warning | Informative | Important -> (0, false) end
    | Section (_title, contents) ->
        do_report contents
    | SectionMin (_title, contents, min) ->
        let (n, b) = do_report contents in
        (max n min, b) in
  let (n, b) = do_report items in
  (max n 0, b)

let enc =
  let open Json_encoding in
  let text_enc =
    list @@ union
      [ case
          (obj2
             (req "text" string)
             (dft "display"
                (string_enum [ "normal", `Normal ;
                               "code", `Code ;
                               "output", `Output ])
                `Normal))
          (function
            | Text text -> Some (text, `Normal)
            | Code text -> Some (text, `Code)
            | Output text -> Some (text, `Output)
            | _ -> None)
          (function
            | (text, `Normal) -> Text text
            | (text, `Code) -> Code text
            | (text, `Output) -> Output text) ;
        case
          empty
          (function Break -> Some () | _ -> None)
          (function () -> Break) ] in
  let status_enc =
    union
      [ case
          int
          (function Success n -> Some n | Penalty n -> Some (-n) | _ -> None)
          (fun n -> if n > 0 then Success n else if n < 0 then Penalty (-n)
                                            else Failure) ;
        case
          (string_enum [ "failure", Failure ;
                         "warning", Warning ;
                         "informative", Informative ;
                         "important", Important ])
          (function Success _ | Penalty _ -> None | v -> Some v)
          (function v -> v)
      ]
  in
  let item_enc = mu "reportItem" @@ fun item_enc ->
    union
      [ case
          (obj3
             (req "section" text_enc)
             (req "contents" (list item_enc))
             (opt "minscore" int))
          (function
            | Section (text, report) -> Some (text, report, None)
            | SectionMin (text, report, min) -> Some (text, report, Some min)
            | Message _ -> None)
          (function
            | (text, report, None) -> Section (text, report)
            | (text, report, Some min) -> SectionMin (text, report, min)) ;
        case
          (obj2
             (req "message" text_enc)
             (req "result" status_enc))
          (function
            | Message (text, status) -> Some (text, status)
            | Section _ | SectionMin _ -> None)
          (fun (text, status) -> Message (text, status)) ]
  in
  list item_enc

(* -- report HTML output ---------------------------------------------------- *)

let folder, unfolder =
  let js = "var div = this.parentElement.parentElement;\
            if (div.classList.contains ('folded')) {\
            div.classList.remove ('folded') ;\
            this.innerHTML = 'v'\
            } else {\
            div.classList.add ('folded') ;\
            this.innerHTML = '>'\
            }" in
  E ("span", [ "onclick", js ; "class", "folder-icon clickable" ], [ T "v" ]),
  E ("span", [ "onclick", js ; "class", "folder-icon clickable" ], [ T ">" ])

let format items =
  let rec format_report items =
    List.fold_left (fun ((successes, failures), items) item ->
        let (isuccesses, ifailures), item = format_item item in
        (successes + isuccesses, failures || ifailures), item :: items)
      ((0, false), []) items |> fun (result, items) ->
    (result, List.rev items)
  and format_item = function
    | Message (text, status) ->
        let result, result_class, score = match status with
          | Success 1 -> (1, false), "success", Some "1 pt"
          | Success n -> (n, false), "success", Some (string_of_int n ^ " pts")
          | Penalty 1 -> (-1, true), "failure", Some "-1 pt"
          | Penalty n ->
              (-n, true), "warning", Some ("-" ^ string_of_int n ^ " pts")
          | Failure -> (0, true), "failure", Some "0 pt"
          | Warning -> (0, false), "warning", None
          | Informative -> (0, false), "informative", None
          | Important -> (0, false), "important", None in
        result,
        E ("p", [ "class", "message " ^ result_class ],
           [ E ("span", [ "class", "text" ],
                match score with
                | None -> format_text text
                | Some score -> E ("span", [ "class", "score" ], [ T score ]) :: format_text text) ])
    | Section (title, contents) ->
        format_section title (format_report contents)
    | SectionMin (title, contents, min) ->
        let (n, b), formatted_report = format_report contents in
        format_section ~min title ((max n min, b), formatted_report)
  and format_section ?min title (result, formatted_report) =
    let result_class, score, folder =
      let min_str = match min with
        | Some m when m = fst result -> " " ^ [%i "(minimum mark)"]
        | _ -> "" in
      let format_section_html result_str =
        [ E ("span", [ "class", "score" ],
             [ T (result_str ^ min_str)])] in
      match result with
        | (0, false) ->
            "informative folded", [], unfolder
        | (n, false) ->
            "success folded",
            format_section_html @@ Format.asprintf [%if"Completed, %d pts"] n,
            unfolder
        | (0, true) ->
            "failure",
            format_section_html @@ [%i"Failed"],
            folder
        | (s, true) when s < 0 || min_str <> "" ->
            "failure",
            format_section_html @@ Format.asprintf "%s, %d pts" [%i"Failed"] s,
            folder
        | (s, true) ->
            "warning",
            format_section_html @@ Format.asprintf [%if"Incomplete, %d pts"] s,
            folder in
    result,
    E ("div", [ "class", "section " ^ result_class ],
       [ E ("span", [ "class", "title" ],
            folder :: format_text title @ score) ;
         E ("div", [ "class", "report" ], formatted_report) ])
  and format_text text =
    let format = function
      | Text w ->
          T w
      | Break ->
          S ("br", [])
      | Code s when String.contains s '\n' ->
          E ("code", ["class", "code-block" ], [ T s ])
      | Output s ->
          E ("code", ["class", "output-block" ], [ T s ])
      | Code s ->
          E ("code", [ "class", "code" ], [ T s ]) in
    List.map format text in
  let (n, b), report = format_report items in
  let result = (max n 0, b) in
  let result_class, score = match result with
    | (0, false) -> "informative", []
    | (0, true) ->
        "failure", [ T [%i"Exercise failed"] ;
                     E ("span", [ "class", "score" ],
                        [ T [%i"0 pt"] ]) ]
    | (n, false) ->
        "success", [ T [%i"Exercise complete"] ;
                     E ("span", [ "class", "score" ],
                        [ T (Format.asprintf [%if"%d pts"] n) ]) ]
    | (s, true) ->
        "warning", [ T [%i"Exercise incomplete"] ;
                     E ("span", [ "class", "score" ],
                        [ T (Format.asprintf [%if"%d pts"] s) ]) ] in

  let js = "var div = this.parentElement.parentElement;\
            if (div.classList.contains ('folded')) {\
            div.classList.remove ('folded') ;\
            } else {\
            div.classList.add ('folded') ;\
            }" in
  E ("div", [ "id", "learnocaml-report" ],
     [E ("div", [ "class", " section " ^ result_class ],
         [ E ("span", [ "class", "title clickable" ; "onclick", js], score) ]) ;
      E ("div", [ "class", "main" ], report) ])

let css = {|
#ocaml_fun_report > div {
  animation: fade_in_report 1s ease-in;
  opacity: 1;
}
@keyframes fade_in_report {
  from { opacity: 0 }
  to   { opacity: 1 }
}
#ocaml_fun_report
.hilight {
  font-weight: bold
}
#ocaml_fun_report
.large {
  text-size: 120%
}
#ocaml_fun_report
.code {
  font-family: 'monospace';
  display: inline;
  font-size: 14px;
  line-height: 16px;
  margin: 0;
  padding: 0;
  color: unset;
  background: unset;
  border: unset;
}
#ocaml_fun_report
.code-block {
  vertical-align: top;
  font-family: 'monospace';
  white-space: pre;
  display: block;
  font-size: 14px;
  line-height: 16px;
  margin: 0 0 0 14px;
  padding: 0;
  color: unset;
  background: unset;
  border: unset;
}
#ocaml_fun_report
.output-block {
  vertical-align: top;
  font-family: 'monospace';
  white-space: pre;
  display: block;
  font-size: 14px;
  line-height: 16px;
  margin: 0 0 0 14px;
  padding: 0;
  color: unset;
  background: rgba(255,255,255,0.5);
  border: 2px rgba(255,255,255,0.5) solid;
}
#ocaml_fun_report
.section {
  display:block;
  padding: 0;
  margin: 2px 0 2px 0
}
#ocaml_fun_report
.message {
  display:block;
  margin: 2px 0 2px 0;
}
#ocaml_fun_report
.section .section,
#ocaml_fun_report
.section .message { margin: 0 }
#ocaml_fun_report
.text,
#ocaml_fun_report
.report {
  display: block;
  border-width: 0px 0px 0px 5px;
  border-style: solid;
  margin: 0;
  font-size: 14px;
  line-height: 16px;
  padding: 0;
  min-height: 22px;
}
#ocaml_fun_report
.text {
  padding: 0 0 0 4px
}
#ocaml_fun_report
.title {
  display: block;
  border-width: 0px 0px 0px 5px;
  border-style: solid;
  margin: 0;
  padding: 6px 4px 6px 4px ;
  font-size: 18px;
}
#ocaml_fun_report
.main .title {
  padding: 2px 0px 2px 0px ;
  line-height: 22px;
  font-size: 14px;
}
#ocaml_fun_report
.score {
  float: right;
  padding: 0 4px 0 0;
  height: 0;
}
#ocaml_fun_report
.success > .title span {
  background-color: green;
  color: white
}
#ocaml_fun_report
.failure > .title span {
  background-color: red;
  color: white
}
#ocaml_fun_report
.warning > .title span {
  background-color: orange;
  color: white
}
#ocaml_fun_report
.informative > .title span {
  background-color: lightgrey;
}
#ocaml_fun_report
.success > .title {
  background-color: green;
  color: white
}
#ocaml_fun_report
.failure > .title {
  background-color: red;
  color: white
}
#ocaml_fun_report
.informative > .title {
  background-color: lightgrey;
}
#ocaml_fun_report
.warning > .title {
  background-color: orange;
  color: white
}
#ocaml_fun_report
.warning > .text {
  background-color: #FA8
}
#ocaml_fun_report
.success > .text {
  background-color: lightgreen
}
#ocaml_fun_report
.failure > .text {
  background-color: pink
}
#ocaml_fun_report
.informative > .text {
  background-color: white
}
#ocaml_fun_report
.important > .text {
  background-color: lightblue
}
#ocaml_fun_report
.warning > .text,
#ocaml_fun_report
.warning > .report,
#ocaml_fun_report
.warning > .title {
  border-color: orange
}
#ocaml_fun_report
.success > .text,
#ocaml_fun_report
.success > .report,
#ocaml_fun_report
.success > .title {
  border-color: green
}
#ocaml_fun_report
.failure > .text,
#ocaml_fun_report
.failure > .report,
#ocaml_fun_report
.failure > .title {
  border-color: red
}
#ocaml_fun_report
.informative > .text,
#ocaml_fun_report
.informative > .report,
#ocaml_fun_report
.informative > .title {
  border-color: lightgrey;
}
#ocaml_fun_report
.important > .text,
#ocaml_fun_report
.important > .report,
#ocaml_fun_report
.important > .title {
  border-color: blue
}
#ocaml_fun_report
.success .text .score {
  color: green
}
#ocaml_fun_report
.failure .text .score {
  color: red
}
#ocaml_fun_report .folded .report {
  display: none;
}
#ocaml_fun_report .folded .main {
  display: none
}
#ocaml_fun_report .clickable {
  cursor: pointer ;
}
#ocaml_fun_report .folder-icon {
  font-family: sans-serif ;
  font-weight: bold;
  width: 1em;
  float: left;
}
|}


let output_html ?(bare = false) ppf report =
  let html_report =
    if bare then
      format report
    else
      E ("div", ["id", "ocaml_fun_report" ],
         [ E ("style", [], [ C css ]) ;
           format report ]) in
  output_html ppf [html_report]

let to_html ?bare report =
  Format.asprintf "%a" (output_html ?bare) report

let print ppf items =
  let rec print_report ppf items =
    Format.pp_print_list format_item ppf items
  and format_item ppf = function
    | Section (text, contents) -> Format.fprintf ppf "@[<v 2>@[<hv>%a@]@,%a@]" print_text text print_report contents
    | SectionMin (text, contents, min) -> Format.fprintf ppf "@[<v 2>@[<hv>%a@ %a@]@,%a@]" print_text text print_min min print_report contents
    | Message (text, Failure) -> Format.fprintf ppf [%if"@[<v 2>Failure: %a@]"] print_text text
    | Message (text, Warning) -> Format.fprintf ppf [%if"@[<v 2>Warning: %a@]"] print_text text
    | Message (text, Informative) -> Format.fprintf ppf "@[<v 2>%a@]" print_text text
    | Message (text, Important) -> Format.fprintf ppf [%if"@[<v 2>Important: %a@]"] print_text text
    | Message (text, Success n) -> Format.fprintf ppf [%if"@[<v 2>Success %d: %a@]"] n print_text text
    | Message (text, Penalty n) -> Format.fprintf ppf [%if"@[<v 2>Penalty %d: %a@]"] (-n) print_text text
  and print_text ppf = function
    | (Code wa | Output wa) :: Text wb :: rest when not (String.contains (String.trim wa) '\n') ->
        print_text ppf (Text ("[" ^ String.trim wa ^ "] " ^ wb) :: rest)
    | Text wa :: (Code wb | Output wb) :: rest when not (String.contains (String.trim wb) '\n') ->
        print_text ppf (Text (wa ^ " [" ^ String.trim wb ^ "]") :: rest)
    | Text wa :: Text wb :: rest ->
        print_text ppf (Text (wa ^ " " ^ wb) :: rest)
    | Text w :: rest ->
        Format.fprintf ppf "@[<hov>%a@]%a" Format.pp_print_text w print_text rest
    | Break :: rest ->
        Format.fprintf ppf "@\n%a" print_text rest
    | Code s :: rest ->
        Format.fprintf ppf "@,%a%a" print_code s print_text rest
    | Output s :: rest ->
        Format.fprintf ppf "@,%a%a" print_code s print_text rest
    | [] -> ()
  and print_code ppf s =
    let s = String.trim s in
    Format.fprintf ppf "@[<v 0> | " ;
    for i = 0 to String.length s - 1 do
      match String.get s i with
      | '\n' -> Format.fprintf ppf "@, | "
      | c -> Format.fprintf ppf "%c" c
    done ;
    Format.fprintf ppf "@]"
  and print_min ppf min =
    Format.fprintf ppf "%a@ %a" Format.pp_print_string [%i "(minimum mark)"]
                                Format.pp_print_int min in
  Format.fprintf ppf "@[<v>%a@]@." print_report items

(* -- report building combinators ------------------------------------------- *)

let split_text str =
  let rec spaces acc i =
    if i < String.length str then
      match String.get str i, acc with
      | (' ' | '\t'), acc -> spaces acc (succ i)
      | ('\r' | '\n'), ([] | Break :: _) -> spaces acc (succ i)
      | ('\r' | '\n'), acc -> spaces (Break :: acc) (succ i)
      | _, acc -> word acc i i
    else acc
  and word acc st i =
    if i < String.length str then
      match String.get str i, acc with
      | (' ' | '\t' | '\r' | '\n'), acc -> spaces (cut acc st i) (succ i)
      | _, acc -> word acc st (succ i)
    else cut acc st i
  and cut acc i j =
    if i = j then acc else Text (String.sub str i (j - i)) :: acc in
  List.rev (spaces [] 0)

let success ~points ~message =
  Message (split_text message, Success points)
let failure ~message =
  Message (split_text message, Failure)
let message ~message =
  Message (split_text message, Informative)
let info ~message =
  Message (split_text message, Important)
let warning ~message =
  Message (split_text message, Warning)
let section ~title contents =
  Section (split_text title, contents)
