(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2016 OCamlPro.
 *
 * Learn-OCaml is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your op1tion) any later version.
 *
 * Learn-OCaml is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

open Js_utils
open Lwt.Infix
open Learnocaml_common
open Learnocaml_exercise_state
open Js_of_ocaml
open Editor_lib
open Dom_html


let quality_function = {|
let avoid_thentrue = let already = ref false in fun _ ->
  if !already then [] else begin
    already := true ;
    Learnocaml_report.[ Message ([ Text "* N'écrivez pas les motifs de code suivants:";
                                   Code "[if ... then true else ...;
 if ... then false else ...;
 if ... then ... else true;
 if ... then ... else false]"; Text "
Utilisez de préférence les opérateurs booléens (&&), (||), not."], Success ~-4) ]
  end

let check_thentrue e =
    Parsetree.(
      match e with
      | {pexp_desc = Pexp_ifthenelse (_, e1, (Some e2))} ->
         begin
           match e1 with
           | {pexp_desc = Pexp_construct ({Asttypes.txt = (Longident.Lident "false")}, None)}
           | {pexp_desc = Pexp_construct ({Asttypes.txt = (Longident.Lident "true")}, None)} ->
              avoid_thentrue e1
           | _ -> []
         end @ begin
           match e2 with
           | {pexp_desc = Pexp_construct ({Asttypes.txt = (Longident.Lident "false")}, None)}
           | {pexp_desc = Pexp_construct ({Asttypes.txt = (Longident.Lident "true")}, None)} ->
             avoid_thentrue e2
           | _ -> []
          end
      | _ -> [])

let avoid_list1app = let already = ref false in fun _ ->
  if !already then [] else begin
    already := true ;
    Learnocaml_report.[ Message ([ Text "* N'écrivez pas:";
                                   Code "[x] @ l";
                                   Text ". Écrivez de préférence:";
                                   Code "x :: l";
                                   Text "."], Success ~-4) ]
  end

let check_list1app e =
  Parsetree.(
    match e.pexp_desc with
    | Pexp_apply (app0, [(_, lst1); _]) ->
       (match app0.pexp_desc, lst1.pexp_desc with
        | Pexp_ident {Asttypes.txt = app0'},
          Pexp_construct ({Asttypes.txt = (Longident.Lident "::")}, Some lst1')
             when List.mem (Longident.flatten app0') [["List"; "append"]; ["@"]] ->
           (match lst1'.pexp_desc with
            | Pexp_tuple [_; nil0] ->
               (match nil0.pexp_desc with
                | Pexp_construct ({Asttypes.txt = (Longident.Lident "[]")}, None) ->
                   avoid_list1app e
                | _ -> [])
            | _ -> [])
        | _ -> [])
    | _ -> [])

let avoid_eqphy = let already = ref false in fun _ ->
  if !already then [] else begin
    already := true ;
    Learnocaml_report.[ Message ([ Text "* Pour PFITA, n'utilisez pas l'égalité physique";
                                   Code "(==)";
                                   Text ". Utilisez de préférence l'égalité structurelle";
                                   Code "(=)";
                                   Text "."], Success ~-1) ]
  end

let avoid_neqphy = let already = ref false in fun _ ->
  if !already then [] else begin
    already := true ;
    Learnocaml_report.[ Message ([ Text "* Pour PFITA, n'utilisez pas l'inégalité physique";
                                   Code "(!=)";
                                   Text ". Utilisez de préférence l'inégalité structurelle";
                                   Code "(<>)";
                                   Text "."], Success ~-1) ]
  end

let check_eqphy e =
  Parsetree.(
    match e.pexp_desc with
    | Pexp_ident {Asttypes.txt = Longident.Lident "=="} -> avoid_eqphy e
    | _ -> [])

let check_neqphy e =
  Parsetree.(
    match e.pexp_desc with
    | Pexp_ident {Asttypes.txt = Longident.Lident "!="} -> avoid_neqphy e
    | _ -> [])
|}

let imperative_function = {|let ast_imperative_check ast =
  let chk_expr e =
    Parsetree.(
      match e with
      | {pexp_desc = Pexp_sequence _} -> forbid_syntax ";" e
      | {pexp_desc = Pexp_while _} -> forbid_syntax "while" e
      | {pexp_desc = Pexp_for _} -> forbid_syntax "for" e
      | {pexp_desc = Pexp_array _} -> forbid_syntax "array" e
      | _ -> [] ) in
  let imperative_report =
    ast_check_structure
      ~on_expression:chk_expr
      ast |> List.sort_uniq compare in
  if snd (Learnocaml_report.result imperative_report) then
    imperative_report
  else
    []|}


let id = arg "id"


(*_____________________Functions for the Generate button_____________________*)

(* Every couple is saved into the local storage *)

let rec save_questions listeQuestions id = match listeQuestions with
  | [] -> ()
  | (name, list_mono) :: suite ->
     let () = List.iter (fun (gen, ty) ->
                  let question = TestAgainstSol
                                   {name; ty; suite = "[]"; gen;
                                    tester = ""; sampler = ""} in
                  let testhaut =  get_testhaut id in (* FIXME: do this once *)
                  let question_id = compute_question_id testhaut in
                  let new_testhaut = IntMap.add question_id question testhaut in
                  save_testhaut new_testhaut id)
                list_mono in
    save_questions suite id

(*----------------------------------------------------------------------*)

let grade_black = ref (fun () -> ())
let grade_red = ref (fun () -> ())
let init_tabs, select_tab =
  let names = [ "toplevel" ; "report" ; "editor" ; "template" ; "test" ;
                "question" ; "prelude" ; "prepare" ; "testhaut" ] in
  let current = ref "question" in
  let select_tab name =
    set_arg "tab" name ;
    if name = "testhaut" then
      !grade_red ()
    else
      !grade_black ();
    Manip.removeClass
      (find_component ("learnocaml-exo-button-" ^ !current))
      "front-tab" ;
    Manip.removeClass
      (find_component ("learnocaml-exo-tab-" ^ !current))
      "front-tab" ;
    Manip.enable
      (find_component ("learnocaml-exo-button-" ^ !current)) ;
    Manip.addClass
      (find_component ("learnocaml-exo-button-" ^ name))
      "front-tab" ;
    Manip.addClass
      (find_component ("learnocaml-exo-tab-" ^ name))
      "front-tab" ;
    Manip.disable
      (find_component ("learnocaml-exo-button-" ^ name)) ;
    current := name in
  let init_tabs () =
    current := begin try
        let requested = arg "tab" in
        if List.mem requested names then requested else "question"
      with Not_found -> "question"
               end ;
    List.iter
      (fun name ->
         Manip.removeClass
           (find_component ("learnocaml-exo-button-" ^ name))
           "front-tab" ;
         Manip.removeClass
           (find_component ("learnocaml-exo-tab-" ^ name))
           "front-tab" ;
         Manip.Ev.onclick
           (find_component ("learnocaml-exo-button-" ^ name))
           (fun _ -> select_tab name ; true))
      names ;
    select_tab !current in
  init_tabs, select_tab


let display_report exo report =
  (* let score, failed = Learnocaml_report.result report in *)
  let report_button = find_component "learnocaml-exo-button-report" in
  Manip.removeClass report_button "success" ;
  Manip.removeClass report_button "failure" ;
  Manip.removeClass report_button "partial" ;
  let grade = 100 in
  if grade >= 100 then begin
    Manip.addClass report_button "success" ;
    Manip.replaceChildren report_button
      Tyxml_js.Html5.[ pcdata [%i"Report"] ]
  end else if grade = 0 then begin
    Manip.addClass report_button "failure" ;
    Manip.replaceChildren report_button
      Tyxml_js.Html5.[ pcdata [%i"Report"] ]
  end else begin
    Manip.addClass report_button "partial" ;
    let pct = Format.asprintf "%2d%%" grade in
    Manip.replaceChildren report_button
      Tyxml_js.Html5.[ pcdata [%i"Report"] ;
                       span ~a: [ a_class [ "score" ] ] [ pcdata pct ]]
  end ;
  let report_container = find_component "learnocaml-exo-tab-report" in
  Manip.setInnerHtml report_container
    (Format.asprintf "%a"
       Learnocaml_report.(output_html_of_report ~bare: true) report) ;
  grade

let () =
  Lwt.async_exception_hook := begin function
    | Failure message -> fatal message
    | Server_caller.Cannot_fetch message -> fatal message
    | exn -> fatal (Printexc.to_string exn)
  end ;
  Lwt.async @@ fun () ->
  Translate.set_lang ();
  let translations = [
  "txt_preparing", [%i"Preparing the environment"];
  "learnocaml-exo-button-editor", [%i"Solution"];
  "learnocaml-exo-button-template", [%i"Template"];
  "learnocaml-exo-button-prelude", [%i"Prelude"];
  "learnocaml-exo-button-prepare", [%i"Prepare"];
  "learnocaml-exo-button-toplevel", [%i"Toplevel"];
  "learnocaml-exo-button-question", [%i"Question"];
  "learnocaml-exo-button-test", [%i"Test.ml"];
  "learnocaml-exo-button-testhaut", [%i"Test"];
  "learnocaml-exo-button-report", [%i"Report"];
  "learnocaml-exo-editor-pane", [%i"Editor"];
  "txt_grade_report", [%i"Click the Grade! button to test your solution"];
  "learnocaml-exo-test-pane", [%i"Editor"];
  ] in
  Translate.set_string_translations translations;
  let translations = [
      "learnocaml-exo-button-editor",
      [%i"Type here the solution of the exercise"];
      "learnocaml-exo-button-template",
      [%i"Type here or generate the template \
          the student will complete or correct"];
      "learnocaml-exo-button-prelude",
      [%i"Type here the definitions of types and \
          functions given to the student"];
      "learnocaml-exo-button-prepare",
      [%i"Type here hidden definitions given to the student"];
      "learnocaml-exo-button-question",
      [%i"Type here the wording of the exercise in Markdown"];
      "learnocaml-exo-button-test",
      [%i"Type here the tests code"];
      "learnocaml-exo-button-testhaut",
      [%i"Generate here the tests code"];
  ] in
  Translate.set_title_translations translations;
  Learnocaml_local_storage.init () ;

  (* ---- launch everything --------------------------------------------- *)

  let toplevel_buttons_group = button_group () in
  disable_button_group toplevel_buttons_group (* enabled after init *) ;
  let toplevel_toolbar = find_component "learnocaml-exo-toplevel-toolbar" in
  let editor_toolbar = find_component "learnocaml-exo-editor-toolbar" in
  let template_toolbar = find_component "learnocaml-exo-template-toolbar" in
  let prelude_toolbar = find_component "learnocaml-exo-prelude-toolbar" in
  let prepare_toolbar = find_component "learnocaml-exo-prepare-toolbar" in
  let test_toolbar = find_component "learnocaml-exo-test-toolbar" in
  let testhaut_toolbar = find_component "learnocaml-exo-testhaut-toolbar" in
  let toplevel_button = button ~container: toplevel_toolbar ~theme: "dark" in
  let editor_button = button ~container: editor_toolbar ~theme: "light" in
  let test_button = button ~container: test_toolbar ~theme: "light" in
  let testhaut_button = button ~container: testhaut_toolbar ~theme: "light" in
  let template_button = button ~container: template_toolbar ~theme: "light" in
  let prelude_button = button ~container: prelude_toolbar ~theme: "light" in
  let prepare_button = button ~container: prepare_toolbar ~theme: "light" in

  let after_init top =
    begin
       Lwt.return true
    end >>= fun r1 ->
    Learnocaml_toplevel.load ~print_outcome:false top
     "" >>= fun r2 ->
    if not r1 || not r2 then failwith [%i"unexpected error"];
    Learnocaml_toplevel.set_checking_environment top >>= fun () ->
    Lwt.return () in
  let timeout_prompt =
    Learnocaml_toplevel.make_timeout_popup
      ~on_show: (fun () -> select_tab "toplevel")
      () in
  let flood_prompt =
    Learnocaml_toplevel.make_flood_popup
      ~on_show: (fun () -> select_tab "toplevel")
      () in
  let history =
    let storage_key =
      Learnocaml_local_storage.exercise_toplevel_history id in
    let on_update self =
      Learnocaml_local_storage.store storage_key
        (Learnocaml_toplevel_history.snapshot self) in
    let snapshot =
      Learnocaml_local_storage.retrieve storage_key in
    Learnocaml_toplevel_history.create
      ~gettimeofday
      ~on_update
      ~max_size: 99
      ~snapshot () in

  let toplevel_launch =
    Learnocaml_toplevel.create
      ~after_init ~timeout_prompt ~flood_prompt
      ~on_disable_input: (fun _ -> disable_button_group toplevel_buttons_group)
      ~on_enable_input: (fun _ -> enable_button_group toplevel_buttons_group)
      ~container:(find_component "learnocaml-exo-toplevel-pane")
      ~history () in
  init_tabs () ;
  toplevel_launch >>= fun top ->

  (* ---- toplevel pane ------------------------------------------------- *)

  begin toplevel_button
      ~group: toplevel_buttons_group
      ~icon: "cleanup" [%i"Clear"] @@ fun () ->
    Learnocaml_toplevel.clear top ;
    Lwt.return ()
  end ;
  begin toplevel_button
      ~icon: "reload" [%i"Reset"] @@ fun () ->
    (* toplevel_launch >>= fun top -> SHOULD BE UNNECESSARY *)
     disabling_button_group toplevel_buttons_group
         (fun () -> Learnocaml_toplevel.reset top)
  end ;
  begin toplevel_button
      ~group: toplevel_buttons_group
      ~icon: "run" [%i"Eval phrase"] @@ fun () ->
    Learnocaml_toplevel.execute top ;
    Lwt.return ()
  end ;

  (* ---- prelude pane --------------------------------------------------- *)

  let editor_prelude = find_component "learnocaml-exo-prelude-pane" in
  let editor_prel = Ocaml_mode.create_ocaml_editor
                      (Tyxml_js.To_dom.of_div editor_prelude) in
  let ace_prel = Ocaml_mode.get_editor editor_prel in
  let contents=
    let a = get_prelude id in
    if a = "" then
      [%i"(* Local definitions the student\n\
          will be able to see *)\n"]
    else
      a in
  Ace.set_contents ace_prel contents ;
  Ace.set_font_size ace_prel 18;

  let typecheck_prelude () =
    Editor_lib.typecheck true ace_prel editor_prel top "" (Ace.get_contents ace_prel) in
  begin prelude_button
      ~group: toplevel_buttons_group
      ~icon: "typecheck" [%i"Check"] @@ typecheck_prelude
  end;

  (* ---- prepare pane --------------------------------------------------- *)

  let editor_prepare = find_component "learnocaml-exo-prepare-pane" in
  let editor_prep = Ocaml_mode.create_ocaml_editor
                      (Tyxml_js.To_dom.of_div editor_prepare) in
  let ace_prep = Ocaml_mode.get_editor editor_prep in
  let contents=
    let a= get_prepare id in
    if a = "" then
      [%i"(* Local definitions the student\n\
          won't be able to see *)\n"]
    else
      a in
  Ace.set_contents ace_prep contents ;
  Ace.set_font_size ace_prep 18;

  let typecheck_prepare () =
    let prel = Ace.get_contents ace_prel ^ "\n" in
    Editor_lib.typecheck true ace_prep editor_prep top prel
      ~onpasterr:(fun () -> select_tab "prelude"; typecheck_prelude ())
      (Ace.get_contents ace_prep) in
  begin prepare_button
      ~group: toplevel_buttons_group
      ~icon: "typecheck" [%i"Check"] @@ typecheck_prepare
  end;

  (* ---- test pane --------------------------------------------------- *)

  let editor_test = find_component "learnocaml-exo-test-pane" in
  let editor_t = Ocaml_mode.create_ocaml_editor
                   (Tyxml_js.To_dom.of_div editor_test) in
  let ace_t = Ocaml_mode.get_editor editor_t in
  let contents=
    let a = get_testml id in
    if a = "" then
      [%i"(* Grader and tests code *)\n"]
    else
      a in

  Ace.set_contents ace_t contents;
  Ace.set_font_size ace_t 18;


  let typecheck_testml () =
    let prelprep = (Ace.get_contents ace_prel ^ "\n"
                    ^ Ace.get_contents ace_prep ^ "\n") in
    Editor_lib.typecheck true ace_t editor_t top prelprep ~mock:true
      ~onpasterr:(fun () -> select_tab "prepare"; typecheck_prepare ())
      (Ace.get_contents ace_t) in
  begin test_button
      ~group: toplevel_buttons_group
      ~icon: "typecheck" [%i"Check"] @@ typecheck_testml
  end;

  (*-------question pane  -------------------------------------------------*)
  let editor_question = find_component "learnocaml-exo-question-mark" in
  let ace_quest = Ace.create_editor (Tyxml_js.To_dom.of_div editor_question ) in
   let question =
    let a = get_question id in
    if a = "" then [%i"# Questions\n\n\
    You can write here your questions using\n\
    the **Markdown** markup language\n"]
    else a in

  Ace.set_contents ace_quest question ;
  Ace.set_font_size ace_quest 18;

  let question = get_question id in
  let question =Omd.to_html (Omd.of_string question) in

  let text_container = find_component "learnocaml-exo-question-html" in
  let text_iframe = Dom_html.createIframe Dom_html.document in
  Manip.replaceChildren text_container
    Tyxml_js.Html5.[ Tyxml_js.Of_dom.of_iFrame text_iframe ] ;
  Js.Opt.case
    (text_iframe##.contentDocument)
    (fun () -> failwith "cannot edit iframe document")
    (fun d ->
       let html = Format.asprintf
           "<!DOCTYPE html>\
            <html><head>\
            <title>%s - exercise text</title>\
            <meta charset='UTF-8'>\
            <link rel='stylesheet'\
            href='css/learnocaml_standalone_description.css'>\
            </head>\
            <body>\
            %s\
            </body>\
            </html>"
           (get_titre id)
           question in
       d##open_;
       d##write (Js.string html);
       d##close);

  let old_text = ref "" in

  let onload () =
   let rec dyn_preview =
    let text = Ace.get_contents ace_quest in
      if text <> !old_text then begin
       Js.Opt.case
    (text_iframe##.contentDocument)
    (fun () -> failwith "cannot edit iframe document")
    (fun d ->
       let html = Format.asprintf
           "<!DOCTYPE html>\
            <html><head>\
            <title>%s - exercise text</title>\
            <meta charset='UTF-8'>\
            <link rel='stylesheet'\
            href='css/learnocaml_standalone_description.css'>\
            </head>\
            <body>\
            %s\
            </body>\
            </html>"
           (get_titre id)
           (Omd.to_html (Omd.of_string text)) in
       d##open_;
       d##write (Js.string html);
       d##close);
       old_text := text
        end in
   dyn_preview; () in

  (* ---- editor pane --------------------------------------------------- *)

  let editor_pane = find_component "learnocaml-exo-editor-pane" in
  let editor = Ocaml_mode.create_ocaml_editor
                 (Tyxml_js.To_dom.of_div editor_pane) in
  let ace = Ocaml_mode.get_editor editor in

  let contents =
    let a= get_solution id in
  if a = "" then
    [%i"(* Your solution *)\n"]
  else
    a in
  Ace.set_contents ace contents;
  Ace.set_font_size ace 18;

  (* ---- template pane --------------------------------------------------- *)

  let editor_template = find_component "learnocaml-exo-template-pane" in
  let editor_temp = Ocaml_mode.create_ocaml_editor
                      (Tyxml_js.To_dom.of_div editor_template) in
  let ace_temp = Ocaml_mode.get_editor editor_temp in
  let contents=
    let a = get_template id in
    if a = "" then
      [%i"(* Code the student will have\n\
          when he will start the exercise *)\n"]
    else
      a in
  Ace.set_contents ace_temp contents ;
  Ace.set_font_size ace_temp 18;

  let messages = Tyxml_js.Html5.ul [] in
  begin template_button
      ~icon: "sync" [%i"Gen. template"] @@ fun () ->
    if (Ace.get_contents ace_temp) = "" then
        Ace.set_contents ace_temp (genTemplate (Ace.get_contents ace) )
    else
      begin
       let aborted, abort_message =
         let t, u = Lwt.task () in
         let btn_cancel = Tyxml_js.Html5.(button [ pcdata [%i"Cancel"] ]) in
         Manip.Ev.onclick btn_cancel
           (fun _ -> hide_loading ~id:"learnocaml-exo-loading" () ; true) ;
         let btn_yes = Tyxml_js.Html5.(button [ pcdata [%i"Yes"] ]) in
         Manip.Ev.onclick btn_yes
           (fun _ -> Ace.set_contents ace_temp
                       (genTemplate (Ace.get_contents ace));
                     hide_loading ~id:"learnocaml-exo-loading" ();
                     true);
         let div =
           Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                             [pcdata [%i"Do you want to crush the template?\n"];
                              btn_yes ;
                              pcdata " " ;
                              btn_cancel ]) in
         Manip.SetCss.opacity div (Some "0") ;
         t, div in
       Manip.replaceChildren messages
         Tyxml_js.Html5.[ li [ pcdata "" ] ] ;
       show_loading ~id:"learnocaml-exo-loading" [ abort_message ] ;
       Manip.SetCss.opacity abort_message (Some "1")
      end;
    Lwt.return ()
  end ;

  let typecheck_template () =
    let prelprep = (Ace.get_contents ace_prel ^ "\n"
                    ^ Ace.get_contents ace_prep ^ "\n") in
    Editor_lib.typecheck true ace_temp editor_temp top prelprep
      ~onpasterr:(fun () -> select_tab "prepare"; typecheck_prepare ())
      (Ace.get_contents ace_temp) in
  begin template_button
      ~group: toplevel_buttons_group
      ~icon: "typecheck" [%i"Check"] @@ typecheck_template
  end;

  (* ---- testhaut pane --------------------------------------------------- *)

  let editor_testhaut = find_component "learnocaml-exo-testhaut-edit" in
  let editor_th =Ocaml_mode.create_ocaml_editor
                   (Tyxml_js.To_dom.of_div editor_testhaut ) in
  let ace_testhaut = Ocaml_mode.get_editor editor_th in
  let buffer = match get_buffer id with
    | buff -> if buff="" then
                [%i"(* Incipit: contains local definitions\n\
                    that will be reachable when you will create\n\
                    a new question *)\n"]
            else buff in
  Ace.set_contents ace_testhaut buffer ;
  Ace.set_font_size ace_testhaut 18;

  let _ = testhaut_init
            (find_component "learnocaml-exo-testhaut-pane") id in ();

  begin testhaut_button
      ~group: toplevel_buttons_group
      ~icon: "sync" [%i"Generate"] @@ fun () ->
    let sol = genTemplate (Ace.get_contents ace) in
    if sol<>"" then
      begin
        disabling_button_group toplevel_buttons_group
          (fun () -> Learnocaml_toplevel.reset top) >>= fun () ->
        Learnocaml_toplevel.execute_phrase top (Ace.get_contents ace) >>=
          fun ok ->
          if ok then
            let res = monomorph_generator (extract_functions (get_answer top)) in
            save_questions res id;
            Manip.removeChildren
              (find_component "learnocaml-exo-testhaut-pane");
            (testhaut_init (find_component "learnocaml-exo-testhaut-pane") id)
          else (select_tab "toplevel" ; Lwt.return ())
      end
    else Lwt.return ();
  end;
  let quality =
    match getElementById_coerce "quality_box" CoerceTo.input with
    | None -> failwith "unknown element quality_box"
    | Some s -> s in
  let imperative =
    match getElementById_coerce "imperative_box" CoerceTo.input with
    | None -> failwith "unknown element imperative_box"
    | Some s -> s in

  let ast_fonction () =
    let quality =
      match getElementById_coerce "quality_box" CoerceTo.input with
      | None -> failwith "unknown element quality_box"
      | Some s -> s in
    let imperative =
      match getElementById_coerce "imperative_box" CoerceTo.input with
      | None -> failwith "unknown element imperative_box"
      | Some s -> s in
    let fonction = if Js.to_bool(quality##.checked) then
                     quality_function
                   else
                     "" in
    let fonction = if Js.to_bool(imperative##.checked) then
                      fonction ^ imperative_function
                    else
                      fonction ^ "" in
    let fonction = fonction ^ "\n\nlet ast_quality ast =" in
    let fonction =
      if Js.to_bool(imperative##.checked) then
        fonction ^ {|
let imperative_report =
  let tempReport = ast_imperative_check ast in
  if tempReport = [] then []
  else (Message ([ Text "Des traits impératifs ont été détectés:" ],
                 Success ~-4)) :: tempReport
|}
      else
        fonction ^ {|
let imperative_report = []
|} in
    let fonction =
      if Js.to_bool(quality##.checked) then
        fonction ^ {|
and report =
  let tempReport = ast_check_structure
    ~on_expression:(check_thentrue @@@ check_list1app @@@
      check_eqphy @@@ check_neqphy)
    ast |> List.sort_uniq compare in
  if tempReport = [] then []
  else (Message ([Text "Des motifs de code indésirables ont été détectés:"],
                 Failure)) :: tempReport
|}
      else fonction ^ " and report = []" in
    let fonction = fonction ^ {|
in if imperative_report = [] && report = []
   then [ Message ([ Text "OK (pas de construction interdite détectée)"], Success 0) ]
   else imperative_report @ report;;
|} in
    fonction in
  let ast_code () =
    let quality =
      match getElementById_coerce "quality_box" CoerceTo.input with
      | None -> failwith "unknown element quality_box"
      | Some s -> s in
    let imperative =
      match getElementById_coerce "imperative_box" CoerceTo.input with
      | None -> failwith "unknown element imperative_box"
      | Some s -> s in
    let fonction =
      if Js.to_bool(quality##.checked) || Js.to_bool(imperative##.checked) then
        {|Section ([ Text "Qualité de code:" ], ast_quality code_ast);
|}
      else
        "" in
    fonction in
  let compile_aux () =
    let tests=test_prel ^ (ast_fonction ()) in
    let tests=tests ^ "\n" ^ (get_buffer id) ^ "\n" in
    let tests=
      IntMap.fold (fun qid -> fun quest -> fun str ->
                      str ^ (Test_spec.question_typed quest qid)^" \n")
        (get_testhaut id) tests in
    let tests=tests ^ init ^ "[\n" ^ ast_code () in
    let tests=
      IntMap.fold (fun qid->fun quest-> fun str ->
          let name=match quest with
            | TestAgainstSol a->a.name
            | TestAgainstSpec a ->a.name
            | TestSuite a -> a.name in
          (* refactor what it's up in editor_lib *)
          str ^ (section name ("question" ^ string_of_int qid ) ))
        (get_testhaut id) tests in
    tests ^ " ]"
  in
  begin testhaut_button
      ~group: toplevel_buttons_group
      ~icon: "typecheck" [%i"Check"] @@ fun () ->
    show_loading ~id:"learnocaml-exo-loading"
      Tyxml_js.Html5.[ ul [ li [ pcdata [%i"Checking"] ] ] ] ;
    let str = with_test_lib_prepare (compile_aux () )
    in
    Learnocaml_toplevel.check ~ppx_meta:true top str >>= fun res->
    typecheck_dialog_box "learnocaml-exo-loading" res
  end ;

  let recovering () =
    let solution = Ace.get_contents ace in
    let titre = get_titre id in
    let incipit= Ace.get_contents ace_testhaut in
    let question = Ace.get_contents ace_quest in
    let template = Ace.get_contents ace_temp in
    let testml = Ace.get_contents ace_t in
    let testhaut= get_testhaut id in
    let prepare= Ace.get_contents ace_prep in
    let prelude =Ace.get_contents ace_prel in
    let test ={testml;testhaut} in
    let diff = get_diff id in
    let description=get_description id in
    let metadata ={id;titre;description;diff} in
    let checkbox = {imperative= Js.to_bool imperative##.checked;
                    undesirable=Js.to_bool quality##.checked} in
    Learnocaml_local_storage.(store (editor_state id))
      { Learnocaml_exercise_state.metadata; incipit; solution; question;
        template; test; prepare; prelude; checkbox; mtime = gettimeofday () } in
  recovering_callback:=recovering ;


  let compile () =
    let tests = compile_aux () in
    match Learnocaml_local_storage.(retrieve (editor_state id) ) with
    | {metadata; prepare; incipit; solution; question;
       template; test; prelude; checkbox; mtime}->
        let mtime=gettimeofday () in
        let test ={testml=tests; testhaut=test.testhaut} in
        let nvexo= {metadata; incipit; prepare; solution; question;
                    template; test; prelude; checkbox; mtime} in
        Learnocaml_local_storage.(store (editor_state id)) nvexo;
        Ace.set_contents ace_t (get_testml id);
        select_tab "test"
  in
  begin testhaut_button
      ~group: toplevel_buttons_group
      ~icon: "run" [%i"Compile"] @@ fun () ->
         let aborted, abort_message =
           let t, u = Lwt.task () in
           let btn_no = Tyxml_js.Html5.(button [ pcdata [%i"No"] ]) in
           Manip.Ev.onclick btn_no ( fun _ ->
              hide_loading ~id:"learnocaml-exo-loading" () ; true) ;
           let btn_yes = Tyxml_js.Html5.(button [ pcdata [%i"Yes"] ]) in
           Manip.Ev.onclick btn_yes (fun _ ->
               hide_loading ~id:"learnocaml-exo-loading" ();
               compile () ; true) ;
           let div =
           Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
           [ pcdata
            [%i"Are you sure you want to overwrite the contents of Test.ml?\n"];
             btn_yes ;
             pcdata " " ;
             btn_no; ]) in
           Manip.SetCss.opacity div (Some "0") ;
           t, div in
         Manip.replaceChildren messages
           Tyxml_js.Html5.[ li [ pcdata "" ] ] ;
         show_loading ~id:"learnocaml-exo-loading" [ abort_message ] ;
         Manip.SetCss.opacity abort_message (Some "1") ;
         Lwt.return ()
  end ;
  begin testhaut_button
          ~group: toplevel_buttons_group
          ~icon: "cleanup" [%i "Delete all"] @@ fun () ->
    let delete_all_questions () =
      save_testhaut IntMap.empty id;
      Manip.removeChildren (find_component "learnocaml-exo-testhaut-pane");
      let _ = testhaut_init
                (find_component "learnocaml-exo-testhaut-pane") id in () in
         let aborted, abort_message =
           let t, u = Lwt.task () in
           let btn_no = Tyxml_js.Html5.(button [ pcdata [%i"No"] ]) in
           Manip.Ev.onclick btn_no ( fun _ ->
              hide_loading ~id:"learnocaml-exo-loading" () ; true) ;
           let btn_yes = Tyxml_js.Html5.(button [ pcdata [%i"Yes"] ]) in
           Manip.Ev.onclick btn_yes (fun _ ->
               hide_loading ~id:"learnocaml-exo-loading" ();
               delete_all_questions () ; true) ;
           let div =
             Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
             [pcdata [%i"Are you sure you want to delete all the questions?\n"];
              btn_yes;
              pcdata " " ;
              btn_no; ]) in
           Manip.SetCss.opacity div (Some "0");
           t, div in
         Manip.replaceChildren messages
           Tyxml_js.Html5.[ li [ pcdata "" ] ];
         show_loading ~id:"learnocaml-exo-loading" [ abort_message ];
         Manip.SetCss.opacity abort_message (Some "1");
         Lwt.return ()
  end;

  begin editor_button
      ~icon: "save" [%i"Save"] @@ fun () ->
    recovering () ;
    Lwt.return ()
  end ;

  begin editor_button
      ~icon: "download" [%i"Download"] @@ fun () ->
    recovering () ;
    let name = id ^ ".json" in
    let content =Learnocaml_local_storage.(retrieve (editor_state id)) in
  let json =
    Json_repr_browser.Json_encoding.construct
      Learnocaml_exercise_state.editor_state_enc
      content in
  let contents =
      (Js._JSON##stringify json) in
    Learnocaml_common.fake_download ~name ~contents ;
    Lwt.return ()
  end ;

  let typecheck_editor () =
    let prelprep = (Ace.get_contents ace_prel ^ "\n"
                    ^ Ace.get_contents ace_prep ^ "\n") in
    Editor_lib.typecheck true ace editor top prelprep
      ~onpasterr:(fun () -> select_tab "prepare"; typecheck_prepare ())
      (Ace.get_contents ace) in
  begin editor_button
      ~group: toplevel_buttons_group
      ~icon: "typecheck" [%i"Check"] @@ typecheck_editor
  end;
  begin toplevel_button
      ~group: toplevel_buttons_group
      ~icon: "run" [%i"Eval code"] @@ fun () ->
      let prelprep = (Ace.get_contents ace_prel ^ "\n"
                      ^ Ace.get_contents ace_prep ^ "\n") in
      Learnocaml_toplevel.execute_phrase top (prelprep ^ Ace.get_contents ace)
      >>= fun _ -> Lwt.return ()
  end;

  (* ---- main toolbar -------------------------------------------------- *)

  let exo_toolbar = find_component "learnocaml-exo-toolbar" in
  let toolbar_button = button ~container: exo_toolbar ~theme: "light" in
  let toolbar_button2 = button2 ~container: exo_toolbar ~theme: "light" in
  begin toolbar_button
      ~icon: "left" [%i"Metadata"] @@ fun () ->
      recovering () ;
      Dom_html.window##.location##assign
        (Js.string ("new_exercise.html#id=" ^ id ^ "&action=open"));
    Lwt.return ()
  end;

  begin toolbar_button
      ~icon: "upload" [%i"Experiment"] @@ fun ()->
    recovering ();

  let aborted, abort_message =
     let t, u = Lwt.task () in
     let btn = Tyxml_js.Html5.(button [ pcdata [%i"abort"] ]) in
     Manip.Ev.onclick btn (fun _ -> Lwt.wakeup u () ; true) ;
     let div =
        Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                          [ pcdata [%i"Grading is taking a lot of time, "] ;
                            btn ;
                            pcdata "?" ]) in
     Manip.SetCss.opacity div (Some "0") ;
     t, div in
  let worker = ref (Grading_jsoo.get_grade (exo_creator id)) in
  let correction =
    Learnocaml_exercise.get Learnocaml_exercise.solution (exo_creator id) in
    let grading =
      !worker correction >>= fun (report, _, _, _) ->
      Lwt.return report in
    let abortion =
      Lwt_js.sleep 5. >>= fun () ->
      Manip.SetCss.opacity abort_message (Some "1") ;
      aborted >>= fun () ->
      Lwt.return Learnocaml_report.[ Message
           ([ Text [%i"Grading aborted by user."] ], Failure) ] in
    Lwt.pick [ grading ; abortion ] >>= fun report_correction ->
    let score_maxi, failed2 =
      Learnocaml_report.result report_correction in
    Dom_html.window##.location##assign
      (Js.string ("exercise.html#id=." ^ id ^ "&score=" ^
                    (string_of_int score_maxi) ^ "&action=open"));
    Lwt.return_unit
  end;

  let messages = Tyxml_js.Html5.ul [] in
  begin toolbar_button
      ~icon: "list" [%i"Exercises"] @@ fun () ->
    let aborted, abort_message =
      let t, u = Lwt.task () in
      let btn_cancel = Tyxml_js.Html5.(button [ pcdata [%i"Cancel"] ]) in
      Manip.Ev.onclick btn_cancel ( fun _ ->
        hide_loading ~id:"learnocaml-exo-loading" () ; true) ;
      let btn_yes = Tyxml_js.Html5.(button [ pcdata [%i"Yes"] ]) in
      Manip.Ev.onclick btn_yes (fun _ ->
      recovering () ;
      Dom_html.window##.location##assign
        (Js.string "index.html#activity=editor") ; true) ;
      let btn_no = Tyxml_js.Html5.(button [ pcdata [%i"No"] ]) in
      Manip.Ev.onclick btn_no (fun _ ->
      Dom_html.window##.location##assign
        (Js.string "index.html#activity=editor") ; true);
      let div =
        Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                          [ pcdata [%i"Do you want to save before closing?\n"] ;
                            btn_yes ;
                            pcdata " " ;
                            btn_no ;
                            pcdata " " ;
                            btn_cancel ]) in
      Manip.SetCss.opacity div (Some "0") ;
      t, div in
    Manip.replaceChildren messages
      Tyxml_js.Html5.[ li [ pcdata "" ] ] ;
    show_loading ~id:"learnocaml-exo-loading" [ abort_message ] ;
    Manip.SetCss.opacity abort_message (Some "1") ;
    Lwt.return ()
  end ;
  let messages = Tyxml_js.Html5.ul [] in
  let callback text =
    Manip.appendChild messages Tyxml_js.Html5.(li [ pcdata text ]) in

  let worker () = ref (Grading_jsoo.get_grade ~callback (exo_creator id)  ) in
  let grade () =
    let aborted, abort_message =
      let t, u = Lwt.task () in
      let btn = Tyxml_js.Html5.(button [ pcdata [%i "abort" ]]) in
      Manip.Ev.onclick btn (fun _ -> Lwt.wakeup u () ; true) ;
      let div =
        Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                          [ pcdata [%i"Grading is taking a lot of time, "] ;
                            btn ;
                            pcdata "?" ]) in
      Manip.SetCss.opacity div (Some "0") ;
      t, div in
    Manip.replaceChildren messages
      Tyxml_js.Html5.[ li [ pcdata [%i"Launching the grader"] ] ] ;
    show_loading ~id:"learnocaml-exo-loading" [ messages ; abort_message ];
    Lwt_js.sleep 1. >>= fun () ->
    let prelprep = (Ace.get_contents ace_prel ^ "\n" ^ Ace.get_contents ace_prep ^ "\n") in
    let solution = Ace.get_contents ace in
    Learnocaml_toplevel.check top (prelprep ^ solution) >>= fun res ->
    match res with
    | Toploop_results.Ok ((), _) ->
       let grading =
         !(worker ()) solution >>= fun (report, _, _, _) ->
         Lwt.return report in
       let abortion =
         Lwt_js.sleep 5. >>= fun () ->
         Manip.SetCss.opacity abort_message (Some "1") ;
         aborted >>= fun () ->
         Lwt.return Learnocaml_report.[ Message
             ([ Text [%i"Grading aborted by user."] ], Failure) ] in
       Lwt.pick [ grading ; abortion ] >>= fun report ->
       let grade = display_report (exo_creator id) report in
       (worker() ) := Grading_jsoo.get_grade ~callback (exo_creator id) ;
       Learnocaml_local_storage.(store (exercise_state id))
         { Learnocaml_exercise_state.grade = Some grade;
           solution; report = Some report ; mtime = gettimeofday () } ;
       select_tab "report" ;
       Lwt_js.yield () >>= fun () ->
       hide_loading ~id:"learnocaml-exo-loading" () ;
       Lwt.return ()
    | Toploop_results.Error _ ->
       select_tab "report" ;
       Lwt_js.yield () >>= fun () ->
       hide_loading ~id:"learnocaml-exo-loading" () ;
       typecheck_editor () in
  begin toolbar_button2
     ~icon: "reload" [%i"Grade!"] @@ fun () ->
     recovering ();
     if arg "tab" = "testhaut" then
       begin
         let aborted, abort_message =
           let t, u = Lwt.task () in
           let btn_cancel = Tyxml_js.Html5.(button [ pcdata [%i"Cancel"] ]) in
           Manip.Ev.onclick btn_cancel ( fun _ ->
               hide_loading ~id:"learnocaml-exo-loading" () ; true) ;
           let btn_compile = Tyxml_js.Html5.(button [ pcdata [%i"Compile"] ]) in
           Manip.Ev.onclick btn_compile (fun _ ->
               recovering () ;
               compile ();
               let _ = grade () in (); true) ;
           let btn_no = Tyxml_js.Html5.(button
                         [ pcdata [%i"Grade without compiling Test"] ]) in
           Manip.Ev.onclick btn_no (fun _ -> let _ = grade () in () ; true);
           let div =
             Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
              [ pcdata [%i"The Grade feature relies on the contents of Test.ml. \
                           Do you want to compile the high-level tests \
                           and overwrite Test.ml?\n"] ;
                btn_compile ;
                pcdata " " ;
                btn_no ;
                pcdata " " ;
                btn_cancel ]) in
           Manip.SetCss.opacity div (Some "0") ;
           t, div in
         Manip.replaceChildren messages
           Tyxml_js.Html5.[ li [ pcdata "" ] ] ;
         show_loading ~id:"learnocaml-exo-loading" [ abort_message ] ;
         Manip.SetCss.opacity abort_message (Some "1") ;
         Lwt.return ()
       end
     else
       grade ()
  end ;
  grade_black:= (fun ()->
    Manip.removeClass (find_component "grade_id") "special_grade");
  grade_red:= (fun ()->
    Manip.addClass (find_component "grade_id") "special_grade" );
  if arg "tab" = "testhaut" then
    !grade_red ();
  
  (* ---- return -------------------------------------------------------- *)
  (* toplevel_launch >>= fun _ -> FIXME? SHOULD BE UNNECESSARY *)
  (* typecheck false >>= fun () ->  ? *)
  hide_loading ~id:"learnocaml-exo-loading" () ;
  let () = Lwt.async @@ fun () ->
       let _ = Dom_html.window##setInterval
                 (Js.wrap_callback (fun () -> onload ())) 200. in
       Lwt.return () in
  Lwt.return ();;

(* Automatic save *)
let () = Lwt.async @@ fun ()->
    let _ =
      let auto_save_interval = 120. (* in seconds *) in
      Dom_html.window##setInterval
          (Js.wrap_callback (fun () -> !recovering_callback ()))
    (auto_save_interval *. 1000.) in
    Lwt.return_unit ;;
