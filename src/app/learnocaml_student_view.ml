(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2023 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml
open Js_of_ocaml_tyxml
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
    }[@@ocaml.warning "-69"]
    let tid name = {
      name;
      btn = snd (id ("learnocaml-exo-button-" ^ name));
      tab = snd (id ("learnocaml-exo-tab-" ^ name));
    }
    let list = tid "list"
    let stats = tid "stats"
    let report = tid "report"
    let editor = tid "editor"
    let draft = tid "draft"
    let text = tid "text"

    let all = [list; stats; report; editor; draft; text]
  end

  let nickname_id, nickname = id "learnocaml-student-nickname"

  let token_id, token = id "learnocaml-token"

  module Dyn = struct
    let exercise_line_id id = "learnocaml-exercise-line-"^id
  end

end

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

let hl_prereq_signal, set_hl_prereq_signal = React.S.create None
let hl_focus_signal, set_hl_focus_signal = React.S.create None

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
    let line =
      H.tr ~a:[ H.a_id (El.Dyn.exercise_line_id id);
                H.a_class ["learnocaml-exercise-line"];
                H.a_onclick (fun _ ->
                    set_selected_exercise (Some id);
                    true) ] [
        H.td ~a:[ H.a_class ["exercise-id"] ] [ H.txt id ];
        H.td ~a:[ H.a_class ["exercise-title"] ]
          [ H.txt meta.Exercise.Meta.title ];
        H.td ~a:[ H.a_class ["exercise-kind"] ] [
          H.txt (string_of_exercise_kind meta.Exercise.Meta.kind);
        ];
        H.td ~a:[ H.a_class ["exercise-stars"] ]
          [ stars_div meta.Exercise.Meta.stars ];
        H.td ~a:[ H.a_class ["grade"]; grade_sty grade ] [
          match grade with
          | None -> H.txt ""
          | Some g -> H.txt (Printf.sprintf "%d%%" g)
        ];
        H.td ~a:[ H.a_class ["last-updated"] ] [
          match mtime with
          | None -> H.txt ""
          | Some t -> date ~time:true t
        ];
      ]
    in
    let cls = "exercise-highlight" in
    let prereq_sigs =
      React.S.map
        (function
          | Some sel when List.mem sel meta.Exercise.Meta.requirements ->
              Manip.addClass line cls
          | _ -> Manip.removeClass line cls)
        hl_prereq_signal
    in
    let focus_sigs =
      React.S.map
        (function
          | Some sel when List.mem sel meta.Exercise.Meta.focus ->
              Manip.addClass line cls
          | _ -> Manip.removeClass line cls)
        hl_focus_signal
    in
    line, (prereq_sigs, focus_sigs)
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
        let lines, sighandlers =
          List.split (List.map2 exercise_line ids states)
        in
        let text =
          match assg with
          | Some (start, _) when start > now ->
              [H.txt [%i"Future assignment (starting "];
               date start;
               H.txt ")"]
          | Some (_, stop) when stop < now ->
              [H.txt [%i"Terminated assignment ("];
               date stop;
               H.txt ")"]
          | Some (_, stop) ->
              [H.txt [%i"Ongoing assignment (due "];
               date stop;
               H.txt ")"]
          | None ->
              [H.txt [%i"Open exercises"]];
        in
        H.tr ~a:[ H.a_class ["learnocaml-assignment-line"];
                  grade_sty (Some (int_of_float avg_grade)) ] [
          H.th ~a:[ H.a_scope `Rowgroup; H.a_colspan 4 ] text;
          H.th ~a:[ H.a_scope `Rowgroup;
                    H.a_class ["grade"] ] [
            H.txt (Printf.sprintf "%01.1f%%" avg_grade)
          ];
          H.th ~a:[ H.a_scope `Rowgroup;
                    H.a_class ["last-updated"] ] [
            match mtime with Some t -> date ~time:true t | None -> H.txt "";
          ];
        ] ::
        lines,
        sighandlers
      )
      assignments
  in
  match assg_lines with
  | [] -> Lwt.return (H.txt "No assigned or open exercises found", None)
  | lines ->
      let lines, sighandlers = List.split lines in
      Lwt.return (H.table (List.concat lines), Some sighandlers)

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
  let item ?(indent=0) ?(fmt = H.txt) lbl title v =
    H.tr ~a:[H.a_title title] [
      H.td ~a:[H.a_class ["stats-label"];
               H.a_style ("padding-left:"^string_of_int (indent * 8)^"px")]
        [fmt lbl];
      H.td v
    ]
  in
  let pct x y =
    let cls = H.a_class ["grade"; "stats-pct"] in
    if y = 0 then
      H.div ~a:[cls; H.a_style ("background-color:"^grade_color None)]
        [H.txt "--%"]
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
      [H.txt (Printf.sprintf "%02.1f%%" r)]
  in [
    H.h3 [H.txt [%i"Student stats"]];
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
               [H.txt [%i"success over exercises training skills"]]] ::
       List.map (fun (sk, (tot, count)) ->
           let i =
             item ~indent:1 ~fmt:tag_span sk
               ([%i"Success over exercises training skill "]^sk)
               [pct tot (100 * count)]
           in
           mouseover_toggle_signal i sk set_hl_focus_signal;
           i
         )
         (SMap.bindings by_focus))
      @
      (if SMap.is_empty by_prereq then [] else
       H.tr [H.th ~a:[H.a_colspan 2]
               [H.txt [%i"success over exercises requiring skills"]]] ::
       List.map (fun (sk, (tot, count)) ->
           let i =
             item ~indent:1 ~fmt:tag_span sk
               ([%i"Success over exercises requiring skill "]^sk)
               [pct tot (100 * count)]
           in
           mouseover_toggle_signal i sk set_hl_prereq_signal;
           i
         )
         (SMap.bindings by_prereq))
    end
  ]

let init_exercises_and_stats_tabs student_token session answers =
  retrieve (Learnocaml_api.Exercise_index (Some session))
  >>= fun (index, _) ->
  retrieve (Learnocaml_api.Exercise_status_index session)
  >>= fun status ->
  let assignments = gather_assignments student_token index status in
  Manip.replaceChildren El.Tabs.(stats.tab) (stats_tab assignments answers);
  exercises_tab assignments answers >|= fun (tbl, sighandlers) ->
  Manip.replaceChildren El.Tabs.(list.tab) [tbl];
  sighandlers

let _exercise_selection_updater =
  let previously_selected = ref None in
  selected_exercise_signal |> React.S.map @@ fun id ->
  let line id = find_component (El.Dyn.exercise_line_id id) in
  Option.iter (fun id -> Manip.removeClass (line id) "selected") !previously_selected;
  previously_selected := id;
  Option.iter (fun id ->
      Manip.addClass (line id) "selected";
      let selected_tab =  React.S.value tab_select_signal in
      if selected_tab = El.Tabs.list || selected_tab = El.Tabs.stats then
        select_tab El.Tabs.report) id

let restore_report_button () =
  let report_button = El.Tabs.(report.btn) in
  let editor_button = El.Tabs.(editor.btn) in
  let remove_class (b1, b2) c = Manip.removeClass b1 c; Manip.removeClass b2 c in
  remove_class (report_button, editor_button) "success";
  remove_class (report_button, editor_button) "failure";
  remove_class (report_button, editor_button) "partial";
  Manip.replaceChildren report_button
    Tyxml_js.Html5.[ txt [%i"Report"] ]

let display_report exo report =
  let score, _failed = Report.result report in
  let report_button = El.Tabs.(report.btn) in
  let editor_button = El.Tabs.(editor.btn) in
  let add_class (b1, b2) c = Manip.addClass b1 c; Manip.addClass b2 c in
  restore_report_button ();
  let grade =
    let max = Learnocaml_exercise.(access File.max_score exo) in
    if max = 0 then 999 else score * 100 / max
  in
  if grade >= 100 then begin
    add_class (report_button, editor_button) "success" ;
    Manip.replaceChildren report_button
      Tyxml_js.Html5.[ txt [%i"Report"] ]
  end else if grade = 0 then begin
    add_class (report_button, editor_button) "failure" ;
    Manip.replaceChildren report_button
      Tyxml_js.Html5.[ txt [%i"Report"] ]
  end else begin
    add_class (report_button, editor_button) "partial" ;
    let pct = Format.asprintf "%2d%%" grade in
    Manip.replaceChildren report_button
      Tyxml_js.Html5.[ txt [%i"Report"] ;
                       span ~a: [ a_class [ "score" ] ] [ txt pct ]]
  end ;
  Manip.setInnerHtml El.Tabs.(report.tab)
    (Format.asprintf "%a" Report.(output_html ~bare: true) report) ;
  grade

let update_answer_tab, clear_answer_tab = ace_display El.Tabs.(editor.tab)

let init_draft_tab () =
  let draft_time = H.div ~a:[H.a_id "learnocaml-exo-draft-time"]
    [H.txt [%i"No draft available."]] in
  let draft_editor = find_component "learnocaml-exo-draft-editor" in
  Manip.appendChild ~before:draft_editor El.Tabs.(draft.tab) draft_time;
  Manip.replaceChildren El.Tabs.(draft.btn) Tyxml_js.Html5.[ txt [%i"Draft"] ]

let update_draft, clear_draft_tab =
  ace_display (find_component "learnocaml-exo-draft-editor")

let restore_draft_button () =
  Manip.removeClass El.Tabs.(draft.btn) "ongoing"

let clear_tabs () =
  restore_report_button ();
  restore_draft_button ();
  List.iter (fun t ->
      Manip.replaceChildren El.Tabs.(t.tab) [])
    El.Tabs.([report; text]);
  clear_draft_tab ();
  Manip.replaceChildren (find_component "learnocaml-exo-draft-time")
    [H.txt [%i"No draft available."]];
  clear_answer_tab ()

let update_draft_tab syn =
  restore_draft_button ();
  let draft_button = El.Tabs.(draft.btn) in
  let inner_div_time, syn_draft = match syn with
    | Some syn ->
       let () = Manip.addClass draft_button "ongoing" in
       ([H.txt [%i"Ungraded draft, synced on: "]; date ~time:true @@ fst syn], snd syn)
    | None ->
       ([H.txt [%i"No draft available."]], "")
  in
  Manip.replaceChildren (find_component "learnocaml-exo-draft-time") inner_div_time;
  update_draft syn_draft

let update_text_tab meta exo =
  let text_iframe = Dom_html.createIframe Dom_html.document in
  Manip.replaceChildren El.Tabs.(text.tab) [
    H.h1 [H.txt meta.Exercise.Meta.title];
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
                [H.txt [%i"GRADE DOESN'T MATCH: cheating suspected"]])
       | _ -> ())
  | None ->
      Manip.replaceChildren El.Tabs.(report.tab)
        [H.div [H.txt [%i"No report available"]]]

let update_tabs meta exo ans syn =
  update_text_tab meta exo;
  update_draft_tab syn;
  Option.iter (fun ans ->
      update_report_tab exo ans;
      update_answer_tab ans.Answer.solution) ans

let () =
  run_async_with_log @@ fun () ->
  (* set_string_translations (); *)
  (* Manip.setInnerText El.version ("v."^Learnocaml_api.version); *)
  Learnocaml_local_storage.init ();
  Option.iter Ocplib_i18n.set_lang (Js_utils.get_lang ());
  set_string_translations_view ();
  let is_teacher = Learnocaml_local_storage.(retrieve is_teacher) in
  let session = Learnocaml_local_storage.(retrieve sync_session) in
  if not (is_teacher) then
    (* No security here: it's client-side, and we don't check that the token is
       registered server-side *)
    failwith "The page you are trying to access is for teachers only";
  let student_token =
    try Token.parse (List.assoc "token" Url.Current.arguments)
    with Not_found | Failure _ -> failwith "Student token missing or invalid"
  in
  init_draft_tab ();
  Manip.setInnerText El.token
    ([%i"Status of student: "] ^ Token.to_string student_token);
  retrieve (Learnocaml_api.Fetch_save session)
  >>= fun save ->
  Manip.setInnerText El.nickname save.Save.nickname;
  init_exercises_and_stats_tabs
    student_token session save.Save.all_exercise_states
  >>= fun _sighandlers ->
  hide_loading ~id:El.loading_id ();
  let _sig =
    selected_exercise_signal |> React.S.map @@ function
    | None -> ()
    | Some ex_id ->
        Lwt.async @@ fun () ->
        retrieve (Learnocaml_api.Exercise (Some session, ex_id, true))
        >>= fun (meta, exo, _) ->
        clear_tabs ();
        let ans = SMap.find_opt ex_id save.Save.all_exercise_states in
        let syn = SMap.find_opt ex_id save.Save.all_exercise_editors in
        update_tabs meta exo ans syn;
        Lwt.return_unit
  in
  Lwt.return_unit
