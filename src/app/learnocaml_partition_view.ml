(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(* NB: exercises-tab -> where is displayed the partition *)

open Js_utils
open Lwt
open Learnocaml_data
open Learnocaml_common

module H = Tyxml_js.Html5
module React = Lwt_react

module El = struct
  (** Defines the static elements that should be present from index.html *)

  let id s = s, find_component s

  let loading_id, loading = id "learnocaml-exo-loading"

  let toolbar_id, toolbar = id "learnocaml-exo-toolbar"

  let buttons_id, buttons = id "learnocaml-exo-tab-buttons"

  let tabs_id, tabs = id "learnocaml-exo-tabs"

  module Tabs = struct
    type t = {
      name: string;
      btn: Html_types.div H.elt;
      tab: Html_types.div H.elt;
    }
    let tid name = {
      name;
      btn = snd (id ("learnocaml-exo-button-" ^ name));
      tab = snd (id ("learnocaml-exo-tab-" ^ name));
    }
    let stats = tid "stats"
    let list = tid "list"
    let editor = tid "editor"
    let text = tid "text"

    let all = [stats; list; editor; text]
  end

  let nickname_id, nickname = id "learnocaml-student-nickname"

  let token_id, token = id "learnocaml-token"
end

let selected_class_signal, set_selected_class = React.S.create None
let selected_repr_signal, set_selected_repr   = React.S.create None
        
let update_editor_tab, clear_editor_tab =
  let ace = lazy (
    let editor =
      Ocaml_mode.create_ocaml_editor
        (Tyxml_js.To_dom.of_div El.Tabs.(editor.tab))
    in
    let ace = Ocaml_mode.get_editor editor in
    Ace.set_font_size ace 16;
    Ace.set_readonly ace true;
    ace
  ) in
  (fun ans ->
     Ace.set_contents (Lazy.force ace) ~reset_undo:true ans),
  (fun () ->
    Ace.set_contents (Lazy.force ace) ~reset_undo:true "")

let tab_select_signal, select_tab =
  let open El.Tabs in
  let current = ref stats in
  let cls = "front-tab" in
  let tab_select_signal, tab_select_signal_set =
    React.S.create !current
  in
  let select_tab el =
    let prev = !current in
    current := el;
    Manip.removeClass prev.btn cls;
    Manip.removeClass prev.tab cls;
    Manip.enable prev.btn;
    Manip.addClass el.btn cls;
    Manip.addClass el.tab cls;
    Manip.disable el.btn;
    tab_select_signal_set el
  in
  List.iter (fun tab ->
      Manip.Ev.onclick tab.btn (fun _ -> select_tab tab; true))
    all;
  tab_select_signal, select_tab

let mouseover_toggle_signal elt sigvalue setter =
  let rec hdl _ =
    Manip.Ev.onmouseout elt (fun _ ->
        setter None;
        Manip.Ev.onmouseover elt hdl;
        true
      );
    setter (Some sigvalue);
    true
  in
  Manip.Ev.onmouseover elt hdl

let string_of_token_list lst = String.concat ", " (List.map Token.to_string lst)

let rec render_tree =
  let open Partition in
  function
  | Leaf xs ->
     [H.p ~a:[ H.a_onclick (fun _ ->
                   set_selected_class (Some xs); true)]
        [H.pcdata ("Leaf with " ^ string_of_int (List.length xs) ^ " student(s)")]]
  | Node (f,l,r) ->
     [
       H.p [ H.pcdata ("Node " ^ string_of_float f) ]
     ;  H.ul [
            H.li (render_tree l)
          ; H.li (render_tree r)
          ]
     ]

let render_trees xs =
  let aux (i,acc) t =
    let str =
      "Class n°" ^ string_of_int i
      ^ " (" ^ string_of_int (Partition.weight_of_tree List.length t) ^ " students)" in
    i+1,
    H.li
      ( H.pcdata str
      :: render_tree t)
    :: acc
  in
  List.rev @@
    snd @@
      List.fold_left aux (1,[]) xs

let render_classes xs =
  let aux (grade,values) acc =
    let str = string_of_int grade ^ "pts :" in
    H.p [H.pcdata str] ::
      H.ul (render_trees values) ::
        acc
  in List.fold_right aux xs []

let exercises_tab part =
  let open Partition in
  let not_graded =
    string_of_int (List.length part.not_graded)
    ^ " codes were not graded: "
    ^ string_of_token_list part.not_graded in
  let bad_type =
       string_of_int (List.length part.bad_type)
    ^ " codes had the wrong type: "
    ^ string_of_token_list part.bad_type in
  H.p [H.pcdata not_graded]
  :: H.p [H.pcdata bad_type]
  :: render_classes part.patition_by_grade

let _class_selection_updater =
  let previous = ref None in
  let of_repr repr = [H.code [H.pcdata repr]] in
  let onclick p tok repr =
     H.a_onclick @@
       fun _ ->
       (match !previous with
        | None -> ()
        | Some prev -> Manip.replaceChildren prev []);
       previous := Some p;
       Manip.replaceChildren p (of_repr repr);
       set_selected_repr (Some (tok,repr));
       true in
  let to_li tok repr p =
    H.li
      ~a:[ onclick p tok repr ]
      [H.pcdata (Token.to_string tok); p] in
  let mkfirst (tok,repr) =
    let p =  H.p (of_repr repr) in
    previous := Some p;
    to_li tok repr p in
  let mkelem (tok,repr) =
    to_li tok repr @@ H.p []
  in
  selected_class_signal |> React.S.map @@ fun id ->
  match id with
  | None -> ()
  | Some xs ->
     set_selected_repr (Some (List.hd xs));
     Manip.replaceChildren El.Tabs.(stats.tab)
       [H.ul @@ mkfirst (List.hd xs) :: List.map mkelem (List.tl xs)]

let clear_tabs () =
  Manip.replaceChildren El.Tabs.(text.tab) [];
  clear_editor_tab ()

let update_text_tab meta exo =
  let text_iframe = Dom_html.createIframe Dom_html.document in
  Manip.replaceChildren El.Tabs.(text.tab) [
    H.h1 [H.pcdata meta.Exercise.Meta.title];
    Tyxml_js.Of_dom.of_iFrame text_iframe
  ];
  Js.Opt.case
    (text_iframe##.contentDocument)
    (fun () -> failwith "cannot edit iframe document")
    (fun d ->
       d##open_;
       d##write (Js.string (exercise_text meta exo));
       d##close)

let set_string_translations () =
  let translations = [
    "txt_loading", [%i"Loading student data"];
    "learnocaml-exo-button-stats", [%i"Stats"];
    "learnocaml-exo-button-list", [%i"Exercises"];
    "learnocaml-exo-button-text", [%i"Subject"];
    "learnocaml-exo-button-editor", [%i"Answer"];
  ] in
  List.iter
    (fun (id, text) ->
       Manip.setInnerHtml (find_component id) text)
    translations

let () =
  Lwt.async_exception_hook := begin fun e ->
    Firebug.console##log (Js.string
                            (Printexc.to_string e ^
                             if Printexc.backtrace_status () then
                               Printexc.get_backtrace ()
                             else ""));
    match e with
    | Failure message -> fatal message
    | Server_caller.Cannot_fetch message -> fatal message
    | exn -> fatal (Printexc.to_string exn)
  end ;
  (match Js_utils.get_lang() with Some l -> Ocplib_i18n.set_lang l | None -> ());
  Lwt.async @@ fun () ->
  Learnocaml_local_storage.init ();
  (match Js_utils.get_lang() with Some l -> Ocplib_i18n.set_lang l | None -> ());
  set_string_translations ();
  let teacher_token = Learnocaml_local_storage.(retrieve sync_token) in
  if not (Token.is_teacher teacher_token) then
    (* No security here: it's client-side, and we don't check that the token is
       registered server-side *)
    failwith "The page you are trying to access is for teachers only";
  let exercise_id =
    try (List.assoc "id" Url.Current.arguments)
    with Not_found -> failwith "Exercise id is missing" in
  let fun_id =
    try (List.assoc "function" Url.Current.arguments)
    with Not_found -> failwith "function name is missing" in

  hide_loading ~id:El.loading_id ();

  Manip.Ev.onclick El.Tabs.editor.El.Tabs.btn
    (fun _ ->
      match React.S.value selected_repr_signal with
      | None -> true
      | Some (tok,_) ->
         select_tab El.Tabs.editor;
         Lwt.async (fun () ->
           retrieve (Learnocaml_api.Fetch_save tok)
           >|= fun save ->
           match SMap.find_opt exercise_id save.Save.all_exercise_states with
           | None -> ()
           | Some x ->
              update_editor_tab x.Answer.solution);
         true );

  retrieve (Learnocaml_api.Partition (teacher_token, exercise_id, fun_id))
  >>= fun part ->
  Manip.replaceChildren El.Tabs.(list.tab) (exercises_tab part);
  Lwt.return_unit
