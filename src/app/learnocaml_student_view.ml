(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2016-2018 OCamlPro.
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
    let report = tid "report"
    let editor = tid "editor"
    let text = tid "text"

    let all = [stats; list; report; editor; text]
  end

  let nickname_id, nickname = id "learnocaml-student-nickname"

  let token_id, token = id "learnocaml-token"

  module Dyn = struct
    let exercise_line_id id = "learnocaml-exercise-line-"^id
  end

end

let hide_loading () = hide_loading ~id:El.loading_id ()

let show_loading msg =
  show_loading ~id:El.loading_id H.[ul [li [pcdata msg]]]

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

let selected_exercise_signal, set_selected_exercise = React.S.create None

let gather_assignments student_token index status =
  let status_map =
    List.fold_left (fun m ex -> SMap.add ex.Exercise.Status.id ex m)
      SMap.empty status
  in
  let assignments =
    get_assignments (Token.Set.singleton student_token) status_map
    |> List.filter (fun (_, toks, _, _) ->
        Token.Set.mem student_token toks)
  in
  let open_exercises =
    let open Exercise.Status in
    SMap.fold (fun id ex acc ->
        match Token.Map.find_opt student_token ex.assignments.token_map with
        | Some Open -> id::acc
        | None when ex.assignments.default = Open -> id::acc
        | _ -> acc)
      status_map []
  in
  let assgs =
    List.map (fun ((start, stop), _tok, _dft, ids) -> Some (start, stop), ids)
      assignments @
    match open_exercises with [] -> [] | l -> [None, SSet.of_list l]
  in
  List.map (fun (dates, ids) ->
      (* Reorder the exercises in the index order *)
      let index =
        Exercise.Index.filter (fun id _ -> SSet.mem id ids) index
      in
      dates,
      List.rev @@ Exercise.Index.fold_exercises (fun l id meta ->
          if SSet.mem id ids then
            let st = SMap.find_opt id status_map in
            (id, meta,
             (match st with None -> [] | Some st ->
                 Exercise.Status.skills_prereq meta st),
             (match st with None -> [] | Some st ->
                 Exercise.Status.skills_focus meta st))
            :: l
          else l)
        [] index)
    assgs

let exercises_tab assignments answers =
  let grade_sty grade =
    H.a_style ("background-color:"^grade_color grade)
  in
  let exercise_line (id, meta, _, _) ans =
    let grade, mtime = match ans with
      | None -> None, None
      | Some Answer.{grade; mtime; _} -> grade, Some mtime
    in
    H.tr ~a:[ H.a_id (El.Dyn.exercise_line_id id);
              H.a_class ["learnocaml-exercise-line"];
              H.a_onclick (fun _ ->
                  set_selected_exercise (Some id);
                  true) ] [
      H.td ~a:[ H.a_class ["exercise-id"] ] [ H.pcdata id ];
      H.td ~a:[ H.a_class ["exercise-title"] ]
        [ H.pcdata meta.Exercise.Meta.title ];
      H.td ~a:[ H.a_class ["exercise-kind"] ] [
        H.pcdata (string_of_exercise_kind meta.Exercise.Meta.kind);
      ];
      H.td ~a:[ H.a_class ["exercise-stars"] ]
        [ stars_div meta.Exercise.Meta.stars ];
      H.td ~a:[ H.a_class ["grade"]; grade_sty grade ] [
        match grade with
        | None -> H.pcdata ""
        | Some g -> H.pcdata (Printf.sprintf "%d%%" g)
      ];
      H.td ~a:[ H.a_class ["last-updated"] ] [
        match mtime with
        | None -> H.pcdata ""
        | Some t -> date ~time:true t
      ];
    ]
  in
  let[@warning "-3"] assg_lines =
    (* tyxml_js marks a_scope as deprecated in HTML5, which is wrong: it's
       deprecated for <td> but not for <th>. *)
    let now = gettimeofday () in
    List.map (fun (assg, ids) ->
        let states =
          List.map (fun (id, _, _, _) -> SMap.find_opt id answers) ids
        in
        let avg_grade, mtime =
          let tot, n, mtime =
            List.fold_left (fun (tot, n, tm) -> function
                | Some Answer.{grade = Some g; mtime; _} ->
                    tot + g, n + 1, max mtime tm
                | Some Answer.{mtime; _} -> tot, n+1, max mtime tm
                | _ -> tot, n + 1, tm)
              (0, 0, 0.) states
          in
          (if n = 0 then 0. else float_of_int tot /. float_of_int n),
          (if mtime = 0. then None else Some mtime)
        in
        let text =
          match assg with
          | Some (start, _) when start > now ->
              [H.pcdata [%i"Future assignment (starting "];
               date start;
               H.pcdata ")"]
          | Some (_, stop) when stop < now ->
              [H.pcdata [%i"Terminated assignment ("];
               date stop;
               H.pcdata ")"]
          | Some (_, stop) ->
              [H.pcdata [%i"Ongoing assignment (due "];
               date stop;
               H.pcdata ")"]
          | None ->
              [H.pcdata [%i"Open exercises"]];
        in
        H.tr ~a:[ H.a_class ["learnocaml-assignment-line"];
                  grade_sty (Some (int_of_float avg_grade)) ] [
          H.th ~a:[ H.a_scope `Rowgroup; H.a_colspan 4 ] text;
          H.th ~a:[ H.a_scope `Rowgroup;
                    H.a_class ["grade"] ] [
            H.pcdata (Printf.sprintf "%01.1f%%" avg_grade)
          ];
          H.th ~a:[ H.a_scope `Rowgroup;
                    H.a_class ["last-updated"] ] [
            match mtime with Some t -> date ~time:true t | None -> H.pcdata "";
          ];
        ] ::
        List.map2 exercise_line ids states
      )
      assignments
  in
  match assg_lines with
  | [] -> Lwt.return (H.pcdata "No assigned or open exercises found")
  | lines -> Lwt.return (H.table (List.concat lines))

let stats_tab assignments answers =
  let smap_add n m key =
    try
      let tot, count = SMap.find key m in
      SMap.add key (n + tot, count + 1) m
    with Not_found ->
      SMap.add key (n, 1) m
  in
  let total_grade, n_attempted, n_total, by_prereq, by_focus =
    List.fold_left
      (fun acc (_dates, exercises) ->
         List.fold_left
           (fun
             (total_grade, n_attempted, n_total, by_prereq, by_focus)
             (id, _meta, prereq, focus) ->
             match SMap.find_opt id answers with
             | None ->
                 (total_grade, n_attempted, n_total + 1,
                  List.fold_left (smap_add 0) by_prereq prereq,
                  List.fold_left (smap_add 0) by_focus focus)
             | Some a ->
                 let g = match a.Answer.grade with None -> 0 | Some g -> g in
                 total_grade + g,
                 n_attempted + 1,
                 n_total + 1,
                 List.fold_left (smap_add g) by_prereq prereq,
                 List.fold_left (smap_add g) by_focus focus)
           acc exercises)
      (0, 0, 0, SMap.empty, SMap.empty)
      assignments
  in
  let item ?(indent=0) ?(fmt = H.pcdata) lbl title v =
    H.tr ~a:[H.a_title title] [
      H.td ~a:[H.a_class ["stats-label"];
               H.a_style ("margin-left:"^string_of_int (indent * 8)^"px")]
        [fmt lbl];
      H.td v
    ]
  in
  let pct x y =
    let cls = H.a_class ["grade"; "stats-pct"] in
    if y = 0 then
      H.div ~a:[cls; H.a_style ("background-color:"^grade_color None)]
        [H.pcdata "--%"]
    else
    let r = 100. *. float_of_int x /. float_of_int y in
    let color = grade_color (Some (int_of_float r)) in
    let background =
      Printf.sprintf "background:linear-gradient(to right,\
                      %s 0%%,%s %.0f%%,transparent %.0f%%)"
        color color r r
    in
    H.div ~a:[H.a_class ["grade"; "stats-pct"];
               H.a_style background]
      [H.pcdata (Printf.sprintf "%02.1f%%" r)]
  in [
    H.h3 [H.pcdata [%i"Student stats"]];
    H.table ~a:[H.a_class ["student-stats"]] begin
      [
        item [%i"completion"]
          [%i"The average grade over all accessible exercises"]
          [pct total_grade (100 * n_total)];
        item [%i"attempted"]
          [%i"The amount of accessible exercises that have been attempted"]
          [pct n_attempted n_total];
        item [%i"success"]
          [%i"The average grade over attempted exercises"]
          [pct total_grade (100 * n_attempted)];
      ]
      @
      (if SMap.is_empty by_focus then [] else
       H.tr [H.th ~a:[H.a_colspan 2]
               [H.pcdata [%i"success over exercises training skills"]]] ::
       List.map (fun (sk, (tot, count)) ->
           item ~indent:1 ~fmt:tag_span sk
             ([%i"Success over exercises training skill "]^sk)
             [pct tot (100 * count)];
         )
         (SMap.bindings by_focus))
      @
      (if SMap.is_empty by_prereq then [] else
       H.tr [H.th ~a:[H.a_colspan 2]
               [H.pcdata [%i"success over exercises requiring skills"]]] ::
       List.map (fun (sk, (tot, count)) ->
           item ~indent:1 ~fmt:tag_span sk
             ([%i"Success over exercises requiring skill "]^sk)
             [pct tot (100 * count)];
         )
         (SMap.bindings by_prereq))
    end
  ]

let init_exercises_and_stats_tabs teacher_token student_token answers =
  Server_caller.request_exn (Learnocaml_api.Exercise_index teacher_token)
  >>= fun (index, _) ->
  Server_caller.request_exn (Learnocaml_api.Exercise_status_index teacher_token)
  >>= fun status ->
  let assignments = gather_assignments student_token index status in
  Manip.replaceChildren El.Tabs.(stats.tab) (stats_tab assignments answers);
  exercises_tab assignments answers >|= fun tbl ->
  Manip.replaceChildren El.Tabs.(list.tab) [tbl]

let _exercise_selection_updater =
  let previously_selected = ref None in
  selected_exercise_signal |> React.S.map @@ fun id ->
  let line id = find_component (El.Dyn.exercise_line_id id) in
  (match !previously_selected with
   | None -> ()
   | Some id -> Manip.removeClass (line id) "selected");
  previously_selected := id;
  match id with
  | None -> ()
  | Some id ->
      Manip.addClass (line id) "selected";
      if React.S.value tab_select_signal = El.Tabs.list then
        select_tab El.Tabs.report

let restore_report_button () =
  let report_button = El.Tabs.(report.btn) in
  Manip.removeClass report_button "success";
  Manip.removeClass report_button "failure";
  Manip.removeClass report_button "partial";
  Manip.replaceChildren report_button
    Tyxml_js.Html5.[ pcdata [%i"Report"] ]

let display_report exo report =
  let score, _failed = Report.result report in
  let report_button = El.Tabs.(report.btn) in
  restore_report_button ();
  let grade =
    let max = Learnocaml_exercise.(access File.max_score exo) in
    if max = 0 then 999 else score * 100 / max
  in
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
  Manip.setInnerHtml El.Tabs.(report.tab)
    (Format.asprintf "%a" Report.(output_html ~bare: true) report) ;
  grade

let update_answer_tab, clear_answer_tab =
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
     Ace.set_contents (Lazy.force ace) ~reset_undo:true ans.Answer.solution),
  (fun () ->
     Ace.set_contents (Lazy.force ace) ~reset_undo:true "")

let clear_tabs () =
  restore_report_button ();
  List.iter (fun t ->
      Manip.replaceChildren El.Tabs.(t.tab) [])
    El.Tabs.([report; text]);
  clear_answer_tab ()

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

let update_report_tab exo ans =
  match ans.Answer.report with
  | Some report ->
      let grade = display_report exo report in
      (match ans.Answer.grade with
       | Some g when g <> grade ->
           Manip.appendChildFirst El.Tabs.(report.tab)
             (H.div ~a:[H.a_class ["warning"]]
                [H.pcdata [%i"GRADE DOESN'T MATCH: cheating suspected"]])
       | _ -> ())
  | None ->
      Manip.replaceChildren El.Tabs.(report.tab)
        [H.div [H.pcdata [%i"No report available"]]]

let update_tabs meta exo ans =
  update_text_tab meta exo;
  match ans with
  | None -> ()
  | Some ans ->
      update_report_tab exo ans;
      update_answer_tab ans

let set_string_translations () =
  let translations = [
    "txt_preparing", [%i"Preparing the environment"];
    "learnocaml-exo-button-stats", [%i"Stats"];
    "learnocaml-exo-button-list", [%i"Exercises"];
    "learnocaml-exo-button-report", [%i"Report"];
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
  (* set_string_translations (); *)
  (* Manip.setInnerText El.version ("v."^Learnocaml_api.version); *)
  Learnocaml_local_storage.init ();
  set_string_translations ();
  let teacher_token = Learnocaml_local_storage.(retrieve sync_token) in
  if not (Token.is_teacher teacher_token) then
    (* No security here: it's client-side, and we don't check that the token is
       registered server-side *)
    failwith "The page you are trying to access is for teachers only";
  let student_token =
    try Token.parse (List.assoc "token" Url.Current.arguments)
    with Not_found | Failure _ -> failwith "Student token missing or invalid"
  in
  Manip.setInnerText El.token
    ([%i"Status of student: "] ^ Token.to_string student_token);
  Server_caller.request_exn (Learnocaml_api.Fetch_save student_token)
  >>= fun save ->
  Manip.setInnerText El.nickname save.Save.nickname;
  init_exercises_and_stats_tabs
    teacher_token student_token save.Save.all_exercise_states
  >>= fun () ->
  hide_loading ();
  let _sig =
    selected_exercise_signal |> React.S.map @@ function
    | None -> ()
    | Some ex_id ->
        Lwt.async @@ fun () ->
        Server_caller.request_exn
          (Learnocaml_api.Exercise (teacher_token, ex_id))
        >>= fun (meta, exo, _) ->
        clear_tabs ();
        let ans = SMap.find_opt ex_id save.Save.all_exercise_states in
        update_tabs meta exo ans;
        Lwt.return_unit
  in




  Lwt.return_unit (*
  let init_tabs token =
    let get_opt o = Js.Optdef.get o (fun () -> false) in
    let tabs =
      (if get_opt config##.enableTryocaml
       then [ "tryocaml", ([%i"Try OCaml"], tryocaml_tab) ] else []) @
      (if get_opt config##.enableLessons
       then [ "lessons", ([%i"Lessons"], lessons_tab) ] else []) @
      (match token, get_opt config##.enableExercises with
       | Some token, true -> [ "exercises", ([%i"Exercises"], exercises_tab token) ]
       | _ -> []) @
      (if get_opt config##.enableToplevel
       then [ "toplevel", ([%i"Toplevel"], toplevel_tab) ] else []) @
      (match token with
       | Some t when Token.is_teacher t ->
           [ "teacher", ([%i"Teach"], teacher_tab t) ]
       | _ -> [])
    in
    let container = El.tab_buttons_container in
    let current_btn = ref None in
    let current_args = ref (ref []) in
    let mutex = Lwt_mutex.create () in
    Manip.removeChildren container ;
    List.map
      (fun (id, (name, callback)) ->
         let btn = Tyxml_js.Html5.(button [ pcdata name]) in
         let div = ref None in
         let args = ref [] in
         let rec select () =
           Lwt_mutex.lock mutex >>= fun () ->
           begin match !current_btn with
             | None -> ()
             | Some btn -> Manip.removeClass btn "active"
           end ;
           Manip.removeChildren El.content ;
           List.iter (fun (n, _) -> delete_arg n) !(!current_args) ;
           begin match !div with
             | Some div ->
                 List.iter (fun (n, v) -> set_arg n v) !args ;
                 Manip.appendChild El.content div ;
                 Lwt.return_unit
             | None ->
                 let arg name =
                   arg name in
                 let set_arg name value =
                   args := set_assoc name value !args ;
                   set_arg name value in
                 let delete_arg name =
                   args := delete_assoc name !args ;
                   delete_arg name in
                 callback select (arg, set_arg, delete_arg) () >>= fun fresh ->
                 div := Some fresh ;
                 Lwt.return_unit
           end >>= fun () ->
           set_arg "activity" id ;
           Manip.addClass btn "active" ;
           menu_hidden := true ;
           Manip.addClass El.menu "hidden" ;
           current_btn := Some btn ;
           current_args := args ;
           Lwt_mutex.unlock mutex ;
           Lwt.return () in
         Manip.Ev.onclick btn
           (fun _ -> Lwt.async select ; true) ;
         Manip.appendChild container btn ;
         id, (name, select))
      tabs
  in
  let download_save () =
    let name = "learnocaml-main.json" in
    let contents =
      let json =
        Json_repr_browser.Json_encoding.construct
          Save.enc
          (get_state_as_save_file ~include_reports:true ()) in
      Js._JSON##(stringify json) in
    Learnocaml_common.fake_download ~name ~contents ;
    Lwt.return ()
  in
  let import_save () =
    Learnocaml_common.fake_upload () >>= fun (_, contents) ->
    let save_file =
      Json_repr_browser.Json_encoding.destruct
        Save.enc
        (Js._JSON##(parse contents)) in
    let token = try Some (get_stored_token ()) with Not_found -> None in
    set_state_from_save_file ?token save_file ;
    (Tyxml_js.To_dom.of_input El.nickname_field)##.value :=
      Js.string save_file.Save.nickname;
    let _tabs = init_tabs token in
    no_tab_selected ();
    Lwt.return ()
  in
  let logout_dialog () =
    Lwt.catch
      (fun () ->
         sync () >|= fun _ ->
         [%i"Be sure to write down your token before logging out:"])
      (fun _ ->
         Lwt.return
           [%i"WARNING: the data could not be synchronised with the server. \
               Logging out will lose your local changes, be sure you exported \
               a backup."])
    >|= fun s ->
    confirm ~title:[%i"Logout"] ~ok_label:[%i"Logout"]
      [H.p [H.pcdata s];
       H.div ~a:[H.a_style "text-align: center;"]
         [token_disp_div (get_stored_token ())]]
      (fun () ->
         Lwt.async @@ fun () ->
         Learnocaml_local_storage.clear ();
         reload ();
         Lwt.return_unit)
  in
  List.iter (fun (text, icon, f) ->
      button ~container:El.sync_buttons ~theme:"white" ~icon text f)
    [
      [%i"Show token"], "token", (fun () ->
          show_token_dialog (get_stored_token ());
          Lwt.return_unit);
      [%i"Sync workspace"], "sync", (fun () ->
          catch_with_alert @@ fun () ->
          sync () >>= fun _ -> Lwt.return_unit);
      [%i"Export to file"], "download", download_save;
      [%i"Import"], "upload", import_save;
      [%i"Logout"], "logout",
      (fun () -> Lwt.async logout_dialog; Lwt.return_unit);
    ];
  begin button
      ~container:El.toolbar
      ~theme:"white" ~icon: "menu" [%i"Menu"] @@ fun () ->
    menu_hidden := not !menu_hidden ;
    if !menu_hidden then
      Manip.addClass El.menu "hidden"
    else
      Manip.removeClass El.menu "hidden" ;
    Lwt.return ()
  end ;
  begin
    (try
       let nickname = Learnocaml_local_storage.(retrieve nickname) in
       (Tyxml_js.To_dom.of_input El.nickname_field)##.value := Js.string nickname;
     with Not_found -> ());
    let save_nickname () =
      Learnocaml_local_storage.(store nickname) @@
      Js.to_string (Tyxml_js.To_dom.of_input El.nickname_field)##.value
    in
    Manip.Ev.onreturn El.nickname_field (fun _ -> save_nickname ());
    Manip.Ev.onblur El.nickname_field (fun _ -> save_nickname (); true);
  end ;
  Manip.Ev.onclick El.hide_panel (fun _ ->
      Manip.SetCss.display El.toolbar "none";
      Manip.SetCss.display El.menu "none";
      Manip.SetCss.left El.content "0";
      Manip.SetCss.display El.show_panel "block";
      true);
  Manip.Ev.onclick El.show_panel (fun _ ->
      let xset elt f =
        Js.Opt.iter (Dom_html.CoerceTo.element (H.toelt elt)) f
      in
      xset El.toolbar (fun s -> s##.style##.display := Js.string "");
      xset El.menu (fun s -> s##.style##.display := Js.string "");
      xset El.content (fun s -> s##.style##.left := Js.string "");
      Manip.SetCss.display El.show_panel "none";
      true);
  init_sync_token sync_button_state >|= init_tabs >>= fun tabs ->
  try
    let activity = arg "activity" in
    let (_, select) = List.assoc activity tabs in
    select ()
  with Not_found ->
    no_tab_selected ();
    Lwt.return ()
;;
*)
