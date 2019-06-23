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
       
(*
module Report = Learnocaml_report

let test_lib ?callback ?(timeout: int option)
 (module Introspection : Introspection_intf.INTROSPECTION) =
 let set_progress =
   match callback with
   | None -> (fun _ -> ())
   | Some set_progress -> set_progress in

 let results : Learnocaml_report.report option ref = ref None in

 let module M (* : Params *) = struct
   let results = results
   let set_progress = set_progress
   let timeout = timeout
   module Introspection = Introspection
 end in
 let module TL = Test_lib.Make(M) in
(module TL : Test_lib.S)
*)

(*
module Dummy_Functor (Introspection :
                        Introspection_intf.INTROSPECTION) = struct
  module Dummy_Params = struct
    let results = ref None
    let set_progress _ = ()
    module Introspection = Introspection            
  end       
  module Test_lib = Test_lib.Make(Dummy_Params)
  module Report = Learnocaml_report
*)


module StringMap = Map.Make (String)

let get_titre id = Learnocaml_local_storage.(retrieve (editor_state id)).titre

let get_diff id = Learnocaml_local_storage.(retrieve (editor_state id)).diff
let get_solution id = Learnocaml_local_storage.(retrieve (editor_state id)).solution
let get_question id = Learnocaml_local_storage.(retrieve (editor_state id)).question
let get_template id = Learnocaml_local_storage.(retrieve (editor_state id)).template
let get_test id = Learnocaml_local_storage.(retrieve (editor_state id)).test
let get_prelude id = Learnocaml_local_storage.(retrieve (editor_state id)).prelude
let get_prepare id = Learnocaml_local_storage.(retrieve (editor_state id)).prepare


let string_of_char ch = String.make 1 ch ;;

let failchar = [' ';'f';'a';'i';'l';'w';'i';'t';'h';' ';'"';'T';'O';'D';'O';'"';'\n'] ;;

let tail l = match l with
|[]->[]
|e::l->l ;;

let rec concatenation listech = match listech with
|[]->""
|c::l -> (string_of_char c)^(concatenation l);;

let rec decompositionSol str n = 
if (n+1= String.length str) then [(str.[n])]
else ( (str.[n])::(decompositionSol str (n+1)) );;

let rec commentaire listech cpt = match listech with
|[]->[]
|'*'::')'::l -> if cpt = 0 then l else commentaire l (cpt-1)
|'('::'*'::l -> commentaire l (cpt+1) 
|c::l->commentaire l cpt;;

let rec premierLet listech = match listech with 
|[]->[]
|'('::'*'::l -> premierLet (commentaire l 0)
|c::'l'::'e'::'t'::' '::l -> if (c='\n'||c=' ') then ('l'::'e'::'t'::' '::l) else premierLet l
|'l'::'e'::'t'::' '::l -> 'l'::'e'::'t'::' '::l 
|' '::l-> premierLet l
|'\n'::l-> premierLet l
|_->[];;

let rec validationLet listech = match listech with
|[]->false
|' '::l->validationLet l
|'\n'::l->validationLet l
|'('::l->validationLet l
|'l'::'e'::'t'::l->false
|_-> true
;;

let rec rechercheEgal listech = match listech with
|[]->0
|'='::l->1
|' '::'l'::'e'::'t'::' '::l->2
|'\n'::'l'::'e'::'t'::' '::l->2
|c::l->rechercheEgal l ;;

let rec rechercheLet listech b = match listech with
|[] -> []
|'('::'*'::l -> rechercheLet (commentaire l 0) b
|';'::';'::l -> rechercheLet l true
|'='::l -> rechercheLet l (validationLet l)
|_::'t'::'h'::'e'::'n'::_::l -> rechercheLet l (validationLet l)
|_::'e'::'l'::'s'::'e'::_::l -> rechercheLet l (validationLet l)
|_::'i'::'n'::_::l -> rechercheLet l (validationLet l)
|'-'::'>'::l->rechercheLet l (validationLet l)
|'l'::'e'::'t'::' '::l ->if b && ((rechercheEgal l)=1) then 'l'::'e'::'t'::' '::l else (if ((rechercheEgal l)=0) then rechercheLet l false else rechercheLet l true)
|c::suite -> rechercheLet suite b
;;

let rec decomposition2 listech = match listech with
     |[] -> []
     |'='::l -> ['=']
     |c::l-> c :: (decomposition2 l) ;;

let decompoFirst listech = match listech with
|[]-> []
|_->(decomposition2 listech)@failchar ;;

let rec genLet listech =
	let liste = rechercheLet listech true in
	match liste with
	|[]->[]
	|_-> (decomposition2 liste)@failchar@(genLet (tail liste)) ;;

let rec genTemplate chaine = if chaine="" then "" else
	concatenation (genLet (decompositionSol chaine 0));;

let init_tabs, select_tab =
  let names = [ "toplevel" ; "report" ; "editor" ; "template" ; "test" ; "question" ; "prelude" ; "prepare" ] in
  let current = ref "toplevel" in
  let select_tab name =
    set_arg "tab" name ;
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
        if List.mem requested names then requested else "toplevel"
      with Not_found -> "toplevel"
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
  let score, failed = Learnocaml_report.result_of_report report in
  let report_button = find_component "learnocaml-exo-button-report" in
  Manip.removeClass report_button "success" ;
  Manip.removeClass report_button "failure" ;
  Manip.removeClass report_button "partial" ;
  let grade = score * 100 / 100 (*(Learnocaml_exercise.(get max_score) exo)*) in
  if grade >= 100 then begin
    Manip.addClass report_button "success" ;
    Manip.replaceChildren report_button
      Tyxml_js.Html5.[ pcdata "Report" ]
  end else if grade = 0 then begin
    Manip.addClass report_button "failure" ;
    Manip.replaceChildren report_button
      Tyxml_js.Html5.[ pcdata "Report" ]
  end else begin
    Manip.addClass report_button "partial" ;
    let pct = Format.asprintf "%2d%%" grade in
    Manip.replaceChildren report_button
      Tyxml_js.Html5.[ pcdata "Report" ;
                       span ~a: [ a_class [ "score" ] ] [ pcdata pct ]]
  end ;
  let report_container = find_component "learnocaml-exo-tab-report" in
  Manip.setInnerHtml report_container
    (Format.asprintf "%a" Learnocaml_report.(output_html_of_report ~bare: true) report) ;
  grade

let () =
  Lwt.async_exception_hook := begin function
    | Failure message -> fatal message
    | Server_caller.Cannot_fetch message -> fatal message
    | exn -> fatal (Printexc.to_string exn)
  end ;
  Lwt.async @@ fun () ->
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
  let toplevel_button = button ~container: toplevel_toolbar ~theme: "dark" in
  let editor_button = button ~container: editor_toolbar ~theme: "light" in
  let test_button = button ~container: test_toolbar ~theme: "light" in
  let template_button = button ~container: template_toolbar ~theme: "light" in
  let prelude_button = button ~container: prelude_toolbar ~theme: "light" in
  let prepare_button = button ~container: prepare_toolbar ~theme: "light" in
  let id = arg "id" in

  let after_init top =
    begin 
       Lwt.return true
    end >>= fun r1 ->
    Learnocaml_toplevel.load ~print_outcome:false top
     "" >>= fun r2 ->
    if not r1 || not r2 then failwith "error in prelude" ;
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
      ~icon: "cleanup" "Clear" @@ fun () ->
    Learnocaml_toplevel.clear top ;
    Lwt.return ()
  end ;
  begin toplevel_button
      ~icon: "reload" "Reset" @@ fun () ->
    toplevel_launch >>= fun top ->
    disabling_button_group toplevel_buttons_group (fun () -> Learnocaml_toplevel.reset top)
  end ;
  begin toplevel_button
      ~group: toplevel_buttons_group
      ~icon: "run" "Eval phrase" @@ fun () ->
    Learnocaml_toplevel.execute top ;
    Lwt.return ()
  end ;


  (* ---- test pane --------------------------------------------------- *)
  let editor_test = find_component "learnocaml-exo-test-pane" in
  let editor_t = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div editor_test) in
  let ace_t = Ocaml_mode.get_editor editor_t in
  Ace.set_contents ace_t  (get_test id); 
  Ace.set_font_size ace_t 18;
  
   (* let typecheck set_class =
    Learnocaml_toplevel.check top (Ace.get_contents ace_t) >>= fun res ->
    let error, warnings =
      match res with
      | Toploop_results.Ok ((), warnings) -> None, warnings
      | Toploop_results.Error (err, warnings) -> Some err, warnings in
    let transl_loc { Toploop_results.loc_start ; loc_end } =
      { Ocaml_mode.loc_start ; loc_end } in
    let error = match error with
      | None -> None
      | Some { Toploop_results.locs ; msg ; if_highlight } ->
          Some { Ocaml_mode.locs = List.map transl_loc locs ;
                 msg = (if if_highlight <> "" then if_highlight else msg) } in
    let warnings =
      List.map
        (fun { Toploop_results.locs ; msg ; if_highlight } ->
           { Ocaml_mode.loc = transl_loc (List.hd locs) ;
             msg = (if if_highlight <> "" then if_highlight else msg) })
        warnings in
    Ocaml_mode.report_error ~set_class editor_t error warnings  >>= fun () ->
    Ace.focus ace_t ;
    Lwt.return () in *)
  begin test_button
      ~group: toplevel_buttons_group
      ~icon: "typecheck" "Check" @@ fun () ->
    Lwt.return ()
  end ;

  (* ---- template pane --------------------------------------------------- *)
  let editor_template = find_component "learnocaml-exo-template-pane" in
  let editor_temp = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div editor_template) in
  let ace_temp = Ocaml_mode.get_editor editor_temp in
  Ace.set_contents ace_temp
    ( get_template id ) ;
  Ace.set_font_size ace_temp 18;

  
  let typecheck set_class =
    Learnocaml_toplevel.check top (Ace.get_contents ace_temp) >>= fun res ->
    let error, warnings =
      match res with
      | Toploop_results.Ok ((), warnings) -> None, warnings
      | Toploop_results.Error (err, warnings) -> Some err, warnings in
    let transl_loc { Toploop_results.loc_start ; loc_end } =
      { Ocaml_mode.loc_start ; loc_end } in
    let error = match error with
      | None -> None
      | Some { Toploop_results.locs ; msg ; if_highlight } ->
          Some { Ocaml_mode.locs = List.map transl_loc locs ;
                 msg = (if if_highlight <> "" then if_highlight else msg) } in
    let warnings =
      List.map
        (fun { Toploop_results.locs ; msg ; if_highlight } ->
           { Ocaml_mode.loc = transl_loc (List.hd locs) ;
             msg = (if if_highlight <> "" then if_highlight else msg) })
        warnings in
    Ocaml_mode.report_error ~set_class editor_temp error warnings  >>= fun () ->
    Ace.focus ace_temp ;
    Lwt.return () in
  begin template_button
      ~group: toplevel_buttons_group
      ~icon: "typecheck" "Check" @@ fun () ->
    typecheck true
  end ;

  (*-------question pane  -------------------------------------------------*)
  let editor_question = find_component "learnocaml-exo-tab-question" in
  let ace_quest = Ace.create_editor (Tyxml_js.To_dom.of_div editor_question ) in
  Ace.set_contents ace_quest (get_question id) ;
  Ace.set_font_size ace_quest 18;

  (* ---- prelude pane --------------------------------------------------- *)
  let editor_prelude = find_component "learnocaml-exo-prelude-pane" in
  let editor_prel = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div editor_prelude) in
  let ace_prel = Ocaml_mode.get_editor editor_prel in
  Ace.set_contents ace_prel
    ( get_prelude id ) ;
  Ace.set_font_size ace_prel 18;
 

    (* ---- prepare pane --------------------------------------------------- *)
  let editor_prepare = find_component "learnocaml-exo-prepare-pane" in
  let editor_prep = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div editor_prepare) in
  let ace_prep = Ocaml_mode.get_editor editor_prep in
  Ace.set_contents ace_prep
    ( get_prepare id ) ;
  Ace.set_font_size ace_prep 18;

  (* ---- editor pane --------------------------------------------------- *)
  let editor_pane = find_component "learnocaml-exo-editor-pane" in
  let editor = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div editor_pane) in
  let ace = Ocaml_mode.get_editor editor in

  let recovering () =
    let solution = Ace.get_contents ace in
    let titre = get_titre id  in
    let question = Ace.get_contents ace_quest in
    let template = Ace.get_contents ace_temp in
    let test = Ace.get_contents ace_t in
    let prepare= Ace.get_contents ace_prep in
    let prelude =Ace.get_contents ace_prel in 
    let  diff =
      match Learnocaml_local_storage.(retrieve (editor_state id)) with
      | { Learnocaml_exercise_state.diff } -> diff
      | exception Not_found -> None in
    Learnocaml_local_storage.(store (editor_state id))
      { Learnocaml_exercise_state.id ; solution ; titre ; question ; template ; diff ; test ;prepare;prelude;
        mtime = gettimeofday () } in

  Ace.set_contents ace (get_solution id);
  Ace.set_font_size ace 18;
  let messages = Tyxml_js.Html5.ul [] in
  begin editor_button
      ~icon: "sync" "Gen.  template" @@ fun () ->
    select_tab "template";
    if (Ace.get_contents ace_temp) = "" then        
        Ace.set_contents ace_temp (genTemplate (Ace.get_contents ace) )
    else
      begin
       let aborted, abort_message =
         let t, u = Lwt.task () in
         let btn_cancel = Tyxml_js.Html5.(button [ pcdata "Cancel" ]) in
         Manip.Ev.onclick btn_cancel ( fun _ ->
                                       hide_loading ~id:"learnocaml-exo-loading" () ; true) ;
         let btn_yes = Tyxml_js.Html5.(button [ pcdata "Yes" ]) in
         Manip.Ev.onclick btn_yes (fun _ -> Ace.set_contents ace_temp (genTemplate (Ace.get_contents ace) );
                                            hide_loading ~id:"learnocaml-exo-loading" ();
                                            true) ;
         let div =
           Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                             [ pcdata "Do you want to crush template ?\n" ;
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

  begin editor_button
      ~icon: "save" "Save" @@ fun () ->
    recovering () ;
    Lwt.return ()
  end ;
  
  begin editor_button
      ~icon: "download" "Download" @@ fun () ->
    recovering () ;
    let name = id ^ ".json" in
    let content =Learnocaml_local_storage.(retrieve (editor_state id)) in  
  let json =
    Json_repr_browser.Json_encoding.construct
      Learnocaml_exercise_state.editor_state_enc
      content in
  let contents =
      (Js._JSON##stringify (json)) in
    Learnocaml_common.fake_download ~name ~contents ;
    Lwt.return ()
  end ;

 (* let lib = " module Test_lib = Test_lib.Make(struct\n\
        \  let results = results\n\
        \  let set_progress = set_progress\n\
        \  module Introspection = Introspection\n\
              end)" in *)
  let typecheck set_class =
    Learnocaml_toplevel.check top (Ace.get_contents ace) >>= fun res ->
    let error, warnings =
      match res with
      | Toploop_results.Ok ((), warnings) -> None, warnings
      | Toploop_results.Error (err, warnings) -> Some err, warnings in
    let transl_loc { Toploop_results.loc_start ; loc_end } =
      { Ocaml_mode.loc_start ; loc_end } in
    let error = match error with
      | None -> None
      | Some { Toploop_results.locs ; msg ; if_highlight } ->
          Some { Ocaml_mode.locs = List.map transl_loc locs ;
                 msg = (if if_highlight <> "" then if_highlight else msg) } in
    let warnings =
      List.map
        (fun { Toploop_results.locs ; msg ; if_highlight } ->
           { Ocaml_mode.loc = transl_loc (List.hd locs) ;
             msg = (if if_highlight <> "" then if_highlight else msg) })
        warnings in
    Ocaml_mode.report_error ~set_class editor error warnings  >>= fun () ->
    Ace.focus ace ;
    Lwt.return () in
  begin editor_button
      ~group: toplevel_buttons_group
      ~icon: "typecheck" "Check" @@ fun () ->
    typecheck true
  end ;
  begin toplevel_button
      ~group: toplevel_buttons_group
      ~icon: "run" "Eval code" @@ fun () ->
    Learnocaml_toplevel.execute_phrase top (Ace.get_contents ace) >>= fun _ ->
    Lwt.return ()
  end ;
  (* ---- main toolbar -------------------------------------------------- *)
  let exo_toolbar = find_component "learnocaml-exo-toolbar" in
  let toolbar_button = button ~container: exo_toolbar ~theme: "light" in
  begin toolbar_button
      ~icon: "left" "Metadata" @@ fun () ->
      recovering () ;
      Dom_html.window##.location##assign
        (Js.string ("new_exercise.html#id=" ^ id ^ "&action=open"));
    Lwt.return ()
  end;
  
  begin toolbar_button
      ~icon: "upload" "Export" @@ fun ()->
    recovering () ;
     Dom_html.window##.location##assign
        (Js.string ("exercise.html#id=." ^ id ^ "&action=open"));
    Lwt.return_unit
  end; 
    
  let messages = Tyxml_js.Html5.ul [] in
  begin toolbar_button
      ~icon: "list" "Exercises" @@ fun () ->
    let aborted, abort_message =
      let t, u = Lwt.task () in
      let btn_cancel = Tyxml_js.Html5.(button [ pcdata "Cancel" ]) in
      Manip.Ev.onclick btn_cancel ( fun _ ->
        hide_loading ~id:"learnocaml-exo-loading" () ; true) ;
      let btn_yes = Tyxml_js.Html5.(button [ pcdata "Yes" ]) in
      Manip.Ev.onclick btn_yes (fun _ ->
      recovering () ;
      Dom_html.window##.location##assign
        (Js.string "index.html#activity=editor") ; true) ;
      let btn_no = Tyxml_js.Html5.(button [ pcdata "No" ]) in
      Manip.Ev.onclick btn_no (fun _ -> 
      Dom_html.window##.location##assign
        (Js.string "index.html#activity=editor") ; true);
      let div =
        Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                          [ pcdata "Do you want to save before closing?\n" ;
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
  let exo ()=
  let titre =  get_titre id in
  let description="" in
  
  let exo1= Learnocaml_exercise.set  Learnocaml_exercise.id id Learnocaml_exercise.empty in
  let exo2= Learnocaml_exercise.set Learnocaml_exercise.title titre exo1 in
  let exo3 =Learnocaml_exercise.set Learnocaml_exercise.max_score 1 exo2 in
  let exo4 =Learnocaml_exercise.set Learnocaml_exercise.prepare (get_prepare id) exo3 in
  let exo5 =Learnocaml_exercise.set Learnocaml_exercise.prelude (get_prelude id) exo4 in
  let exo6 =Learnocaml_exercise.set Learnocaml_exercise.solution (get_solution id) exo5 in
  let exo7 =Learnocaml_exercise.set Learnocaml_exercise.test (get_test id) exo6 in
  let exo8 =Learnocaml_exercise.set Learnocaml_exercise.template (get_template id) exo7 in
  Learnocaml_exercise.set Learnocaml_exercise.descr description exo8
  in
  
  let worker () = ref (Grading_jsoo.get_grade ~callback (exo () )  ) in
  begin toolbar_button
      ~icon: "reload" "Grade!" @@ fun () ->


    let aborted, abort_message =
      let t, u = Lwt.task () in
      let btn = Tyxml_js.Html5.(button [ pcdata "abort" ]) in
      Manip.Ev.onclick btn (fun _ -> Lwt.wakeup u () ; true) ;
      let div =
        Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                          [ pcdata "Grading is taking a lot of time, " ;
                            btn ;
                            pcdata " ?" ]) in
      Manip.SetCss.opacity div (Some "0") ;
      t, div in
    Manip.replaceChildren messages
      Tyxml_js.Html5.[ li [ pcdata "Launching the grader" ] ] ;
    show_loading ~id:"learnocaml-exo-loading" [ messages ; abort_message ] ;
    Lwt_js.sleep 1. >>= fun () ->
    let solution = Ace.get_contents ace in
    Learnocaml_toplevel.check top solution >>= fun res ->
    match res with
    | Toploop_results.Ok ((), _) ->
        let grading =
          !(worker ()) solution >>= fun (report, _, _, _) ->
          Lwt.return report in
        let abortion =
          Lwt_js.sleep 5. >>= fun () ->
          Manip.SetCss.opacity abort_message (Some "1") ;
          aborted >>= fun () ->
          Lwt.return Learnocaml_report.[ Message ([ Text "Grading aborted by user." ], Failure) ] in
        Lwt.pick [ grading ; abortion ] >>= fun report ->
        let grade = display_report (exo () ) report in
        (worker() ) := Grading_jsoo.get_grade ~callback ( exo () ) ;
        Learnocaml_local_storage.(store (exercise_state id))
          { Learnocaml_exercise_state.grade = Some grade ; solution ; report = Some report ;
            mtime = gettimeofday () } ;
        select_tab "report" ;
        Lwt_js.yield () >>= fun () ->
        hide_loading ~id:"learnocaml-exo-loading" () ;
        Lwt.return ()
    | Toploop_results.Error _ ->
        let msg =
          Learnocaml_report.[ Text "Error in your code." ; Break ;
                   Text "Cannot start the grader if your code does not typecheck." ] in
        let report = Learnocaml_report.[ Message (msg, Failure) ] in
        let grade = display_report (exo () ) report in
        Learnocaml_local_storage.(store (exercise_state id))
          { Learnocaml_exercise_state.grade = Some grade ; solution ; report = Some report ;
            mtime = gettimeofday () } ;
        select_tab "report" ;
        Lwt_js.yield () >>= fun () ->
        hide_loading ~id:"learnocaml-exo-loading" () ;
        typecheck true
  end ;

  (* ---- return -------------------------------------------------------- *)
  toplevel_launch >>= fun _ ->
  typecheck false >>= fun () ->
  hide_loading ~id:"learnocaml-exo-loading" () ;

  Lwt.return () ;;
