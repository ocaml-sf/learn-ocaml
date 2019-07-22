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
open Grade_exercise
open Learnocaml_data
open Js_of_ocaml
open Editor_lib
open Dom_html
open Test_spec

module H = Tyxml_js.Html

(*----------------------------------------------------------------------*)

let init_tabs, select_tab =
  mk_tab_handlers "question" [ "toplevel" ; "report" ; "editor" ; "template" ; "test" ;
                 "prelude" ; "prepare" ] 
    
              
let set_string_translations () =
  let translations = [
      "txt_preparing", [%i"Preparing the environment"];
      "learnocaml-exo-button-editor", [%i"Solution"];
      "learnocaml-exo-button-template", [%i"Template"];
      "learnocaml-exo-button-prelude", [%i"Prelude"];
      "learnocaml-exo-button-prepare", [%i"Prepare"];
      "learnocaml-exo-button-toplevel", [%i"Toplevel"];
      "learnocaml-exo-button-question", [%i"Question"];
      "learnocaml-exo-button-test", [%i"Test"];
      "learnocaml-exo-button-report", [%i"Report"];
      "learnocaml-exo-editor-pane", [%i"Editor"];
      "txt_grade_report", [%i"Click the Grade! button to test your solution"];
      "learnocaml-exo-test-pane", [%i"Editor"];
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
    ] in
  List.iter
    (fun (id, text) ->
      Manip.setInnerHtml (find_component id) text)
    translations

let activate_before_unload () :unit =
  Js.Unsafe.js_expr
    "window.onbeforeunload = function() {return 'You have unsaved changes!';}"

let unable_before_unload () :unit =
  Js.Unsafe.js_expr "window.onbeforeunload = null"
  
let onchange ace_list =
  let add_change_listener ace =
    Ace.on
      ace
      "change"
      (fun _ -> activate_before_unload ();) in
  List.iter (fun ace -> add_change_listener ace) ace_list 

          
let () =
  run_async_with_log @@ fun () ->
               (*set_string_translations ();*)
  Learnocaml_local_storage.init () ;

  (* ---- launch everything --------------------------------------------- *)
  let id =  arg "id"
  in
  let toplevel_buttons_group = button_group () in
  disable_button_group toplevel_buttons_group (* enabled after init *) ;
  let toplevel_toolbar = find_component "learnocaml-exo-toplevel-toolbar" in
  let editor_toolbar = find_component "learnocaml-exo-editor-toolbar" in
  let template_toolbar = find_component "learnocaml-exo-template-toolbar" in
  let prelude_toolbar = find_component "learnocaml-exo-prelude-toolbar" in
  let prepare_toolbar = find_component "learnocaml-exo-prepare-toolbar" in
  let test_toolbar = find_component "learnocaml-exo-test-toolbar" in
  let toplevel_button = button ~container: toplevel_toolbar ~theme: "dark" in
  let editor_button = button ~container: editor_toolbar ~theme: "light" in
  let test_button = button ~container: test_toolbar ~theme: "light" in
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
  let contents= get_prelude id in
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
  let contents= get_prepare id in
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

  begin test_button
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
            begin
              let questions = monomorph_generator (extract_functions (get_answer top)) in
              let indexed_list= List.mapi (fun i elt -> (i,elt)) questions in
              let string = compile indexed_list in
              Ace.set_contents ace_t string;
              Lwt.return_unit
            end
          else (select_tab "toplevel" ; Lwt.return ())
      end
    else Lwt.return ();
  end;

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

  (* ---- template pane --------------------------------------------------- *)

  let editor_template = find_component "learnocaml-exo-template-pane" in
  let editor_temp = Ocaml_mode.create_ocaml_editor
                      (Tyxml_js.To_dom.of_div editor_template) in
  let ace_temp = Ocaml_mode.get_editor editor_temp in
  let contents= get_template id  in
  Ace.set_contents ace_temp contents ;
  Ace.set_font_size ace_temp 18;

  begin template_button
      ~icon: "sync" [%i"Gen. template"] @@ fun () ->
    if (Ace.get_contents ace_temp) = "" then
        Ace.set_contents ace_temp (genTemplate (Ace.get_contents ace) )
    else
      begin
        Learnocaml_common.confirm
          ~title:"Confirmation"
          ~ok_label:"Yes"
          [ H.p [H.pcdata "Do you want to crush the template?\n"] ]
          (fun () ->
            Ace.set_contents ace_temp
              (genTemplate (Ace.get_contents ace));
            hide_loading ~id:"learnocaml-exo-loading" ());
      end ;
    Lwt.return();
  end;
  
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

  let recovering () =
    unable_before_unload ();
    let solution = Ace.get_contents ace in
    let descr = Ace.get_contents ace_quest in
    let template = Ace.get_contents ace_temp in
    let test = Ace.get_contents ace_t in
    let prepare= Ace.get_contents ace_prep in
    let prelude = Ace.get_contents ace_prel in
    let open Learnocaml_data.Editor in
    let exercise =
      {id;prelude;template;descr;prepare;test;solution;max_score=0}
    in
    let old_state = get_editor_state id in
    let new_state = {metadata=old_state.metadata;exercise} in
    update_index new_state in
  begin editor_button
      ~icon: "save" [%i"Save"] @@ fun () ->
    recovering ();
    Lwt.return ()
  end ;
  begin editor_button
      ~icon: "download" [%i"Download"] @@ fun () ->
     recovering () ;
    Editor_io.download id;
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
  (*let toolbar_button2 = button2 ~container: exo_toolbar ~theme: "light" in*)
  begin toolbar_button
      ~icon: "left" [%i"Metadata"] @@ fun () ->
      Dom_html.window##.location##assign
        (Js.string ("new_exercise.html#id=" ^ id ^ "&action=open"));
    Lwt.return ()
  end;
  begin toolbar_button
      ~icon: "list" [%i"Exercises"] @@ fun () ->
      Dom_html.window##.location##assign
        (Js.string "index.html#activity=editor"); 
    Lwt.return ()
  end ;

  begin toolbar_button
          ~icon: "upload" [%i"Experiment"] @@
          fun ()->
          recovering ();
          Dom_html.window##.location##assign
            (Js.string ("exercise.html#id=." ^ id));
          Lwt.return_unit
  end;
 

  (* TODO : factorize somehow this with 
 src/app/learnocaml_exercise_main grade to learnocaml_common *)
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
    show_load "learnocaml-exo-loading" [ messages ; abort_message ];
    Lwt_js.sleep 1. >>= fun () ->
    let prelprep = (Ace.get_contents ace_prel ^ "\n" ^ Ace.get_contents ace_prep ^ "\n") in
    let solution = Ace.get_contents ace in
    Learnocaml_toplevel.check top (prelprep ^ solution) >>= fun res ->
    match res with
    | Toploop_results.Ok ((), _) ->
        let grading =
          Lwt.finalize
            (fun () ->
               !(worker ()) >>= fun w ->
               w solution >>= fun (report, _, _, _) ->
               Lwt.return report)
            (fun () ->
               (worker ()) := get_grade ~callback (exo_creator id);
               Lwt.return_unit)
        in
       let abortion =
         Lwt_js.sleep 5. >>= fun () ->
         Manip.SetCss.opacity abort_message (Some "1") ;
         aborted >>= fun () ->
         Lwt.return Learnocaml_report.[ Message
             ([ Text [%i"Grading aborted by user."] ], Failure) ] in
       Lwt.pick [ grading ; abortion ] >>= fun report ->
       let _grade =  display_report (exo_creator id) report in
       (worker() ) := Grading_jsoo.get_grade ~callback (exo_creator id) ;
       select_tab "report" ;
       Lwt_js.yield () >>= fun () ->
       hide_loading ~id:"learnocaml-exo-loading" () ;
       Lwt.return ()
    | Toploop_results.Error _ ->
       select_tab "report" ;
       Lwt_js.yield () >>= fun () ->
       hide_loading ~id:"learnocaml-exo-loading" () ;
       typecheck_editor () in
  begin toolbar_button
     ~icon: "reload" [%i"Grade!"] @@ fun () ->
                                     recovering ();
                                     grade ()
  end ;
  onchange [ace_temp; ace_t; ace_prep; ace_prel; ace_quest; ace ];
  (* ---- return -------------------------------------------------------- *)
  (* toplevel_launch >>= fun _ -> should be unnecessary? *)
  (* typecheck false >>= fun () -> *)
  hide_loading ~id:"learnocaml-exo-loading" () ;
  let () = Lwt.async @@ fun () ->
       let _ = Dom_html.window##setInterval
                 (Js.wrap_callback (fun () -> onload ())) 200. in
       Lwt.return () in

  Lwt.return ();;
