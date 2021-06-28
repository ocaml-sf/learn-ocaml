(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml
open Js_utils
open Lwt
open Learnocaml_data
open Learnocaml_common

module H = Tyxml_js.Html5
module ES = Exercise.Status

let filter_input input_id list_id apply_fun =
  let input_field =
    H.input ~a:[
      H.a_id input_id;
      H.a_input_type `Search;
      H.a_list list_id;
    ] ()
  in
  Manip.Ev.oninput input_field (fun _ev ->
      apply_fun (Manip.value input_field);
      true);
  H.div ~a:[H.a_class ["filter_input"]] [
    H.datalist ~a:[H.a_id list_id] ();
    input_field;
    H.span ~a:[
      H.a_class ["filter_reset_cross"];
      H.a_onclick (fun _ ->
          (Tyxml_js.To_dom.of_input input_field)##.value := Js.string "";
          apply_fun "";
          true);
    ]
      [H.txt "\xe2\x9c\x96" (* U+2716 heavy multiplication x *)];
  ]

let tag_addremove list_id placeholder add_fun remove_fun =
  let tag_input =
    H.input ~a:[
      H.a_input_type `Text;
      H.a_list list_id;
      H.a_placeholder placeholder;
    ] ()
  in
  let get_input_list () =
    List.filter ((<>) "")
      (String.split_on_char ' ' (Manip.value tag_input))
  in
  H.div [
    tag_input;
    H.button ~a:[
      H.a_class ["addremove"];
      H.a_onclick (fun _ev ->
          add_fun (get_input_list ());
          true)
    ] [ H.txt "\xe2\x9e\x95" (* U+2795 heavy plus sign *) ];
    H.button ~a:[
      H.a_class ["addremove"];
      H.a_onclick (fun _ev ->
          remove_fun (get_input_list ());
          true);
    ] [ H.txt "\xe2\x9e\x96" (* U+2796 heavy minus sign *) ];
  ]

let rec teacher_tab token _select _params () =
  let action_new_token () =
    retrieve (Learnocaml_api.Create_teacher_token token)
    >|= fun new_token ->
    alert ~title:[%i"TEACHER TOKEN"]
      (Printf.sprintf [%if"New teacher token created:\n%s\n\n\
                           write it down."]
         (Token.to_string new_token))
  in
  let action_csv_export () =
    retrieve (Learnocaml_api.Students_csv (token, [], []))
    >|= fun csv ->
    Learnocaml_common.fake_download
      ~name:"learnocaml.csv"
      ~contents:(Js.string csv)
  in
  let indent_style lvl =
    H.a_style (Printf.sprintf "text-align: left; padding-left: %dem;" lvl)
  in
  let htbl_keys t = Hashtbl.fold (fun k _ acc -> k::acc) t [] in
  let elt e =
    Js.Opt.case
      (Dom_html.CoerceTo.element (H.toelt e))
      (fun () -> failwith "Bad coercion to element")
      (fun x -> x)
  in
  let descendants_by_tag parent tag =
    List.fold_left (fun acc node ->
        Js.Opt.case (Dom_html.CoerceTo.element node)
          (fun () -> acc)
          (fun elt -> elt :: acc))
      []
      (Dom.list_of_nodeList
         ((elt parent)##getElementsByTagName (Js.string tag)))
  in

  (* State storage *)
  let selected_exercises = Hashtbl.create 117 in
  let selected_students = Hashtbl.create 117 in
  let selected_assignment = ref None in
  let exercises_index = ref (Exercise.Index.Exercises []) in
  let all_exercise_skills = ref SSet.empty in
  let students_map = ref Token.Map.empty in
  let assignments_tbl = Hashtbl.create 59 in
  let students_changes = ref Token.Map.empty in
  let all_student_tags = ref SSet.empty in
  let status_map = ref SMap.empty in
  let (status_changes: ES.t SMap.t ref) = ref SMap.empty in
  let status_current () =
    SMap.merge (fun _ old newer -> match newer with None -> old | s -> s)
      !status_map !status_changes;
  in
  let get_status id =
    try SMap.find id !status_changes with Not_found ->
    try SMap.find id !status_map with Not_found ->
      ES.default id
  in
  let exercise_changed_status id =
    match SMap.find_opt id !status_changes with
    | None -> false
    | Some ch -> match SMap.find_opt id !status_map with
      | None -> ch <> ES.default id
      | Some st -> ch <> st
  in
  let students_current () =
    Token.Map.merge (fun _ old newer -> match newer with None -> old | s -> s)
      !students_map !students_changes
  in
  let get_student token =
    try Token.Map.find token !students_changes with Not_found ->
    try Token.Map.find token !students_map with Not_found ->
      Student.default token
  in
  let exercise_line_id id = "learnocaml_exercise_"^id in
  let exercise_group_id id = "exercise_group_"^id in
  let student_line_id student =
    "learnocaml_student_" ^ match student with
    | `Any -> "any"
    | `Token tok -> Token.to_string tok
  in
  let assg_line_id id = "assg_line_"^string_of_int id in

  let all_exercises g =
    Exercise.Index.fold_exercises (fun acc id _ -> id :: acc)
      [] g
    |> List.rev
  in

  let get_student_selection () =
    Hashtbl.fold (fun st () (toks, dft) -> match st with
        | `Token tk -> Token.Set.add tk toks, dft
        | `Any -> toks, true)
      selected_students (Token.Set.empty, false)
  in

  let auto_checkbox () =
    H.div ~a:[H.a_class ["auto_checkbox"]] []
  in
  let auto_checkbox_td () =
    H.td ~a:[H.a_class ["auto_checkbox"]] [auto_checkbox ()]
  in

  (* Action function callbacks *)
  let update_changed_status = ref (fun () -> assert false) in
  let toggle_selected_exercises =
    ref (fun ?force:_ ?update:_ _ -> assert false) in
  let toggle_selected_students =
    ref (fun ?force:_ ?update:_ _ -> assert false)
  in
  let toggle_select_assignment = ref (fun _ -> assert false) in
  let exercise_status_change = ref (fun _ -> assert false) in
  let student_change = ref (fun _ -> assert false) in
  let assignment_change = ref (fun _ -> assert false) in
  let assignment_remove = ref (fun _ -> assert false) in

  (* Exercises table *)
  let rec mk_table group_level acc status group =
    match group with
    | Exercise.Index.Groups groups_list ->
        List.fold_left (fun acc (id, g) ->
            let all_children = all_exercises g.Exercise.Index.contents in
            let acc =
              H.tr ~a:[
                H.a_id (exercise_group_id id);
                H.a_class ["exercise_group"];
                H.a_onclick (fun _ ->
                    !toggle_selected_exercises all_children; false);
              ] [
                H.td [];
                H.th ~a:[H.a_colspan 10; indent_style group_level]
                  [H.txt g.Exercise.Index.title];
              ] :: acc
            in
            mk_table (group_level + 1) acc status g.Exercise.Index.contents)
          acc groups_list
    | Exercise.Index.Exercises exlist ->
        List.fold_left (fun acc (id, meta) ->
            let open_exercise_ () =
              let _win = window_open ("/exercises/"^id^"/") "_blank" in
              false
            in
            let open_partition_ () =
              Lwt.async (fun () ->
                ask_string ~title:"Choose a function name" ~cancel_label:None
                  [H.txt @@ "Choose a function name to partition codes from "^ id ^": "]
                >|= fun funname ->
                let _win =
                  window_open
                    ("/partition-view.html?id="^ id ^"&function="^funname^"&prof=30") "_blank"
                in ());
                false
            in
            match meta with None -> acc | Some meta ->
            let st = status id in
            let hid = exercise_line_id id in
            let classes =
              (if exercise_changed_status id then ["changed"] else []) @
              (if Hashtbl.mem selected_exercises id then ["selected"] else [])
            in
            let skills_prereq = ES.skills_prereq meta st in
            let skills_focus = ES.skills_focus meta st in
            H.tr ~a:[
              H.a_id hid;
              H.a_class ("exercise_line" :: classes);
              H.a_onclick (fun _ -> !toggle_selected_exercises [id]; false);
              H.a_ondblclick (fun _ -> open_exercise_ ());
              H.a_onmouseup (fun ev ->
                  Js.Optdef.case ev##.which (fun () -> true) @@ fun btn ->
                  if btn = Dom_html.Middle_button then open_partition_ () else true);
            ] [
              auto_checkbox_td ();
              H.td ~a:[indent_style group_level]
                [ H.txt meta.Exercise.Meta.title ];
              H.td ~a:[H.a_class ["skills-prereq"]]
                (List.map tag_span skills_prereq @
                 if skills_prereq <> [] || skills_focus <> []
                 then [H.txt "\xe2\x87\xa2"]
                 (* U+21E2, rightwards dashed arrow *)
                 else []);
              H.td ~a:[H.a_class ["skills-focus"]]
                (List.map tag_span skills_focus);
              H.td [stars_div meta.Exercise.Meta.stars];
              H.td [
                let cls, text =
                  if Token.Map.is_empty ES.(st.assignments.token_map) then
                    match ES.(st.assignments.default) with
                    | ES.Open -> "exo_open", [%i"Open"]
                    | ES.Closed -> "exo_closed", [%i"Closed"]
                    | ES.Assigned _ -> "exo_assigned", [%i"Assigned"]
                  else "exo_assigned", [%i"Assigned"]
                in
                H.span ~a:[H.a_class [cls]] [H.txt text]
              ];
            ] :: acc)
          acc exlist
  in
  let set_exercise_filtering str =
    let skills, keywords =
      List.partition (fun s -> SSet.mem s !all_exercise_skills)
        (List.filter ((<>) "") (String.split_on_char ' ' str))
    in
    let res =
      List.map (fun s ->
          new%js Js.regExp_withFlags (Js.string s) (Js.string "i"))
        keywords
    in
    let matches id meta =
      let st = get_status id in
      (List.for_all (fun re ->
           let strmatch = function
             | None -> false
             | Some s -> Js.to_bool (re##test (Js.string s))
           in
           strmatch meta.Exercise.Meta.id ||
           strmatch (Some meta.Exercise.Meta.title) ||
           strmatch meta.Exercise.Meta.short_description)
          res)
      &&
      List.for_all (fun skill ->
          List.mem skill (ES.skills_focus meta st) ||
          List.mem skill (ES.skills_prereq meta st))
        skills
    in
    let rec hide = function
      | Exercise.Index.Groups groups_list ->
          List.fold_left (fun (empty0, hidden0) (id, g) ->
              let empty, hidden = hide g.Exercise.Index.contents in
              let elt = find_component (exercise_group_id id) in
              if empty then Manip.addClass elt "exercise_hidden"
              else Manip.removeClass elt "exercise_hidden";
              empty && empty0, List.rev_append hidden hidden0)
            (true, []) groups_list
      | Exercise.Index.Exercises l ->
          List.fold_left (fun (empty, hidden) (id, ex) ->
              let elt = find_component (exercise_line_id id) in
              match ex with
              | Some ex when matches id ex ->
                  Manip.removeClass elt "exercise_hidden";
                  false, hidden
              | _ ->
                  Manip.addClass elt "exercise_hidden";
                  empty, (id::hidden))
            (true, []) l
    in
    let _empty, hidden = hide !exercises_index in
    if !selected_assignment = None then
      !toggle_selected_exercises ~force:false ~update:true hidden
  in
  let exercises_list_div =
    H.div ~a:[H.a_id "exercises_list"] [H.txt [%i"Loading..."]]
  in
  let exercise_skills_list_id = "exercise_skills_list" in
  let exercises_div =
    let legend =
      H.legend ~a:[
        H.a_onclick (fun _ ->
            !toggle_selected_exercises (all_exercises !exercises_index);
            true);
      ] [H.txt [%i"Exercises"]; H.txt " \xe2\x98\x90" (* U+2610 *)]
    in
    H.div ~a:[H.a_id "exercises_pane"; H.a_class ["learnocaml_pane"]] [
      H.div ~a:[H.a_id "exercises_filter_box"] [
        H.datalist ~a:[H.a_id exercise_skills_list_id] ();
        filter_input "exercises_search_field"
          exercise_skills_list_id set_exercise_filtering];
      H.fieldset ~legend [ exercises_list_div ];
    ]
  in
  let students_list_div =
    H.div ~a:[H.a_id "students_list"] [H.txt [%i"Loading..."]];
  in
  let student_tags_list_id = "student_tags_list" in
  let sort_type = [`Nick; `Token; `Date; `Tags] in
  let sort_to_str = function
    | `Nick -> "nickname"
    | `Token -> "token"
    | `Date -> "date"
    | `Tags -> "tags"
  in
  let str_to_sort s =
    List.find (fun t -> sort_to_str t = s) sort_type
  in
  let get_student_sorting () =
    match
      Dom_html.getElementById_coerce "student_sort" Dom_html.CoerceTo.select
    with
    | Some s -> str_to_sort (Js.to_string s##.value)
    | None -> raise Not_found
  in
  let html_token tk =
    H.span ~a:[H.a_class ["learnocaml_token"]]
      [H.txt (Token.to_string tk)]
  in
  let make_student_line ?(selected=false) st contents =
    let open_student_tab =
      let f t =
        let _win =
          window_open (Printf.sprintf "/student-view.html?token=%s"
                         (Token.to_string t)) "_blank"
        in
        false
      in
      match st with
      | `Token t ->
          [ H.a_ondblclick (fun _ -> f t);
            H.a_onmouseup (fun ev ->
                Js.Optdef.case ev##.which (fun () -> true) @@ fun btn ->
                if btn = Dom_html.Middle_button then f t else true)
          ]
      | `Any -> []
    in
    H.tr ~a:([
        H.a_id (student_line_id st);
        H.a_class (["student_line"] @ if selected then ["selected"] else []);
        H.a_onclick (fun _ ->
            !toggle_selected_students [st];
            true);
      ] @ open_student_tab)
      (auto_checkbox_td () :: contents)
  in
  let anystudents_line =
    make_student_line `Any [
      H.td ~a:[H.a_colspan 10; H.a_class ["future_students"]] [
        H.txt [%i"any future students"]
      ];
    ]
  in
  let student_progression_id tok =
    "student-progression-" ^ Token.to_string tok
  in
  let fill_students_pane () =
    let compare =
      let open Student in
      let compare_nick st1 st2 = match st1.nickname, st2.nickname with
        | None, None -> 0
        | None, Some _ -> 1
        | Some _, None -> -1
        | Some n1, Some n2 ->
            compare (String.lowercase_ascii n1) (String.lowercase_ascii n2)
      in
      match get_student_sorting () with
      | `Token -> fun st1 st2 -> compare st1.token st2.token
      | `Nick -> fun st1 st2 ->
        (match compare_nick st1 st2 with
         | 0 -> compare st1 st2
         | n -> n)
      | `Date -> fun st1 st2 ->
        (match compare st1.creation_date st2.creation_date with
         | 0 -> compare st1 st2
         | n -> n)
      | `Tags -> fun st1 st2 ->
        (match compare st1.tags st2.tags with
         | 0 -> (match compare_nick st1 st2 with
             | 0 -> compare st1 st2
             | n -> n)
         | n -> n)
    in
    let all_students =
      Token.Map.fold (fun _ st acc -> st :: acc) (students_current ()) []
      |> List.sort compare
    in
    let table =
      anystudents_line ::
      List.map (fun st ->
          make_student_line
            ~selected:(Hashtbl.mem selected_students (`Token st.Student.token))
            (`Token  st.Student.token) [
            H.td [html_token st.Student.token];
            H.td (match st.Student.nickname with
                | Some n -> [H.txt n]
                | _ -> []);
            H.td (List.map tag_span (SSet.elements st.Student.tags));
            try find_component (student_progression_id st.Student.token)
            with Failure _ ->
              H.td ~a:[H.a_id (student_progression_id st.Student.token);
                       H.a_class ["student-progression"]]
                [];
          ])
        all_students
    in
    Manip.replaceChildren students_list_div [H.table table];
    all_student_tags :=
      List.fold_left (fun tags st -> SSet.union tags st.Student.tags)
        SSet.empty all_students;
    Manip.replaceSelf (find_component student_tags_list_id)
      (H.datalist ~a:[H.a_id student_tags_list_id] ~children:(
          `Options
            (SSet.fold (fun tag acc -> H.option (H.txt tag) :: acc)
               !all_student_tags []
            ))
          ())
  in
  let set_student_filtering str =
    let tags, keywords =
      List.partition (fun s -> SSet.mem s !all_student_tags)
        (List.filter ((<>) "") (String.split_on_char ' ' str))
    in
    let res =
      List.map (fun s ->
          new%js Js.regExp_withFlags (Js.string s) (Js.string "i"))
        keywords
    in
    let matches std =
      (match std.Student.nickname with
       | None -> keywords = []
       | Some n ->
           List.for_all (fun re -> Js.to_bool (re##test (Js.string n))) res)
      &&
      List.for_all (fun tag -> SSet.mem tag std.Student.tags) tags
    in
    let hidden =
      if tags = [] && keywords = [] then
        (Manip.removeClass anystudents_line "student_hidden"; [])
      else
        (Manip.addClass anystudents_line "student_hidden"; [`Any])
    in
    let hidden =
      Token.Map.fold (fun tok std hidden->
          let elt = find_component (student_line_id (`Token tok)) in
          if matches std then
            (Manip.removeClass elt "student_hidden";
             hidden)
          else (
            Manip.addClass elt "student_hidden";
            `Token tok :: hidden
          )
        )
        (students_current ()) hidden
    in
    if !selected_assignment = None then
      !toggle_selected_students ~force:false ~update:true hidden
  in
  let change_student_tags f =
    let selected, _ = get_student_selection () in
    students_changes :=
      Token.Set.fold (fun tok acc ->
          let student = get_student tok in
          let tags = f student.Student.tags in
          if tags <> student.Student.tags then
            Token.Map.add tok {student with Student.tags} acc
          else acc)
        selected !students_changes;
    fill_students_pane ();
    set_student_filtering
      (Manip.value (find_component "student_search_field"));
    !update_changed_status ();
  in
  let add_student_tags tags =
    change_student_tags (SSet.union (SSet.of_list tags))
  in
  let remove_student_tags tags =
    let tags = SSet.of_list tags in
    change_student_tags (fun t -> SSet.diff t tags)
  in
  let students_div =
    let legend =
      H.legend ~a:[
        H.a_onclick (fun _ ->
            let all =
              Token.Map.fold (fun k _ acc -> (`Token k)::acc)
                !students_map [`Any]
            in
            let all =
              List.filter (fun t ->
                  not (Manip.hasClass (find_component (student_line_id t))
                         "student_hidden"))
                all
            in
            !toggle_selected_students all;
            true
          );
      ] [H.txt [%i"Students"];
         H.txt " \xe2\x98\x90" (* U+2610 ballot box *)]
    in
    H.div ~a:[H.a_id "students_pane"; H.a_class ["learnocaml_pane"]] [
      H.div ~a:[H.a_id "students_filter_box"] [
        H.datalist ~a:[H.a_id student_tags_list_id] ();
        filter_input "student_search_field"
          student_tags_list_id set_student_filtering;
        H.div ~a:[H.a_class ["filler_h"]] [];
        H.label ~a:[H.a_label_for "student_sort"]
          [H.txt [%i"Sort by"]];
        H.select ~a:[
          H.a_id "student_sort";
          H.a_oninput (fun _ev -> fill_students_pane (); true);
        ] [
          H.option ~a:[H.a_value (sort_to_str `Nick); H.a_selected ()]
            (H.txt [%i"Nickname"]);
          H.option ~a:[H.a_value (sort_to_str `Token)]
            (H.txt [%i"Token"]);
          H.option ~a:[H.a_value (sort_to_str `Date)]
            (H.txt [%i"Creation date"]);
          H.option ~a:[H.a_value (sort_to_str `Tags)]
            (H.txt [%i"Tags"]);
        ]
      ];
      H.fieldset ~legend [ students_list_div ];
      H.div ~a:[H.a_id "student_controls"] [
        tag_addremove student_tags_list_id [%i"tags"]
          add_student_tags remove_student_tags;
      ]
    ]
  in
  let fill_exercises_pane () =
    let table = List.rev (mk_table 0 [] get_status !exercises_index) in
    Manip.replaceChildren exercises_list_div [H.table table];
    all_exercise_skills :=
      Exercise.Index.fold_exercises (fun acc id meta ->
          let st = get_status id in
          let acc =
            List.fold_left (fun acc sk -> SSet.add sk acc)
              acc (ES.skills_prereq meta st)
          in
          List.fold_left (fun acc sk -> SSet.add sk acc)
            acc (ES.skills_focus meta st))
        SSet.empty
        !exercises_index;
    Manip.replaceSelf (find_component exercise_skills_list_id)
      (H.datalist ~a:[H.a_id exercise_skills_list_id] ~children:(
          `Options
            (SSet.fold (fun skill acc -> H.option (H.txt skill) :: acc)
               !all_exercise_skills []
            ))
          ());
    match Manip.value (find_component "exercises_search_field") with
    | "" -> ()
    | s -> set_exercise_filtering s
  in

  let assignment_line id =
    let selected = !selected_assignment = Some id in
    let now = gettimeofday () in
    let date id assg_id t =
      let date = new%js Js.date_fromTimeValue (t *. 1000.) in
      let cls = if t <= now then ["date_past"] else ["date_future"] in
      H.div [
        H.input ~a:([
            H.a_id ("date_"^id);
            H.a_class ("assignment_date" :: cls);
            H.a_input_type `Date;
            H.a_value
              (Printf.sprintf "%04d-%02d-%02d"
                 date##getFullYear (date##getMonth + 1) date##getDate);
            H.a_onblur (fun _ -> !assignment_change assg_id; true);
            H.a_onkeydown (fun ev ->
                if ev##.keyCode = 13 then !assignment_change assg_id; true);
            H.a_pattern "[0-9]{4}-[0-9]{2}-[0-9]{2}";
            H.a_required ();
          ] @ if selected then [] else [H.a_readonly ()])
          ();
        H.input ~a:([
            H.a_id ("time_"^id);
            H.a_class ("assignment_date" :: cls);
            H.a_input_type `Time;
            H.a_value
              (Printf.sprintf "%02d:%02d"
                 date##getHours date##getMinutes);
            H.a_onblur (fun _ -> !assignment_change assg_id; true);
            H.a_onkeydown (fun ev ->
                if ev##.keyCode = 13 then !assignment_change assg_id; true);
            H.a_pattern "[0-9]{2}:[0-9]{2}";
            H.a_required ();
          ] @ if selected then [] else [H.a_readonly ()])
          ();
      ]
    in
    let hid = assg_line_id id in
    let ((start, stop), tokens, exo_ids, default) =
      Hashtbl.find assignments_tbl id
    in
    let cls = [] in
    let cls = if selected then "selected"::cls else cls in
    let cls = "assg_line"::cls in
    let n_exos = match SSet.cardinal exo_ids with
      | 1 -> [%i"1 exercise"]
      | n -> Printf.sprintf [%if"%d exercises"] n
    in
    let n_students = match Token.Set.cardinal tokens, default with
      | 1, false -> [%i"1 student"]
      | n, false -> Printf.sprintf [%if"%d students"] n
      | n, true -> Printf.sprintf [%if"%d+ students"] n
    in
    H.tr ~a:[
      H.a_id hid;
      H.a_class cls;
      H.a_onclick (fun ev ->
          if
            Js.Opt.case ev##.target (fun () -> true) (fun e ->
                String.lowercase_ascii (Js.to_string e##.tagName) <> "input"
                || Js.to_bool (e##hasAttribute (Js.string "readonly")))
          then !toggle_select_assignment id;
          true)
    ] [
      H.td [date ("start_"^hid) id start];
      H.td [date ("stop_"^hid) id stop];
      H.td [H.txt n_exos];
      H.td [H.txt n_students];
      H.td ~a:[H.a_onclick (fun _ -> !assignment_remove id; false);
               H.a_class ["remove-cross"]]
        [H.txt "\xe2\x9c\x96" (* U+2716 heavy multiplication x *)];
      (* todo: add common tags *)
    ]
  in
  let is_assigned status token =
    let open ES in
    match get_status token status.assignments with
    | Assigned _ -> true
    | Open | Closed -> false
  in
  let already_assigned_exercises students default =
    List.fold_left (fun acc ex ->
        (* fixme: inefficient *)
        let stat = get_status ex in
        if default &&
           ES.(match stat.assignments.default
               with Assigned _ -> true | _ -> false)
           || Token.Set.exists (is_assigned stat) students
        then SSet.add ex acc else acc)
      SSet.empty
      (all_exercises !exercises_index)
  in
  let unassigned_students exercises tokens =
    List.fold_left (fun acc ex ->
        let st = get_status ex in
        Token.Set.filter (fun t -> not (is_assigned st t)) acc)
      tokens
      exercises
  in
  let already_assigned_students exercises =
    let all_tokens =
      Token.Map.fold (fun t _ -> Token.Set.add t) !students_map Token.Set.empty
    in
    Token.Set.diff all_tokens (unassigned_students exercises all_tokens)
  in
  let assignments_table () =
    let all_tokens =
      Token.Map.fold (fun t _ -> Token.Set.add t) !students_map Token.Set.empty
    in
    let assignments = get_assignments all_tokens (status_current ()) in
    let line_n = ref 0 in
    let make_line assg tokens exo_ids mode =
      incr line_n;
      let id = !line_n in
      Hashtbl.add assignments_tbl id (assg, tokens, exo_ids, mode);
      assignment_line id
    in
    let new_assg_id = "new_assignment" in
    let new_assg_button = H.button [H.txt [%i"New assignment"]] in
    let table = H.table [] in
    let new_assg_line =
      H.tr ~a:[
        H.a_id new_assg_id;
      ] [
        H.td ~a:[H.a_colspan 10] [ new_assg_button ]
      ]
    in
    let new_assignment () =
      let start, stop =
        let tm = Unix.(gmtime (time ())) in
        let tm = Unix.{tm with tm_hour = 0; tm_min = 0; tm_sec = 0} in
        fst Unix.(mktime {tm with tm_mday = tm.tm_mday + 1}),
        fst Unix.(mktime {tm with tm_mday = tm.tm_mday + 8})
      in
      let tokens, default = get_student_selection () in
      let exercises =
        Hashtbl.fold (fun id () -> SSet.add id) selected_exercises SSet.empty
      in
      let exercises =
        SSet.diff exercises (already_assigned_exercises tokens default)
      in
      let line =
        make_line (start, stop) tokens exercises default
      in
      let id = !line_n in
      Dom.insertBefore (H.toelt table)
        (H.toelt line)
        (Js.some (H.toelt new_assg_line));
      !assignment_change id;
      !toggle_select_assignment id
    in
    Manip.Ev.onclick new_assg_button (fun _ -> new_assignment (); false);
    Manip.replaceChildren table @@
    List.map (fun (assg, tokens, dft, exos) -> make_line assg tokens exos dft)
      assignments @
    [new_assg_line];
    table
  in
  let change_exercise_skills op of_meta of_status update_status =
    status_changes :=
      Hashtbl.fold
        (fun id () ->
           let base = of_meta (Exercise.Index.find !exercises_index id) in
           let st = get_status id in
           let current = op (ES.get_skills ~base (of_status st)) in
           SMap.add id (update_status st (ES.make_skills ~base current)))
        selected_exercises
        !status_changes;
    fill_exercises_pane ();
    !update_changed_status ()
  in
  let add_requirements skills =
    change_exercise_skills
      (List.rev_append skills)
      (fun meta -> meta.Exercise.Meta.requirements)
      (fun st -> st.ES.skills_prereq)
      (fun st skills_prereq -> {st with ES.skills_prereq})
  in
  let remove_requirements skills =
    change_exercise_skills
      (List.filter (fun s -> not (List.mem s skills)))
      (fun meta -> meta.Exercise.Meta.requirements)
      (fun st -> st.ES.skills_prereq)
      (fun st skills_prereq -> {st with ES.skills_prereq})
  in
  let add_focus skills =
    change_exercise_skills
      (List.rev_append skills)
      (fun meta -> meta.Exercise.Meta.focus)
      (fun st -> st.ES.skills_focus)
      (fun st skills_focus -> {st with ES.skills_focus})
  in
  let remove_focus skills =
    change_exercise_skills
      (List.filter (fun s -> not (List.mem s skills)))
      (fun meta -> meta.Exercise.Meta.focus)
      (fun st -> st.ES.skills_focus)
      (fun st skills_focus -> {st with ES.skills_focus})
  in
  let open_close_button =
    H.button ~a:[
      H.a_onclick (fun _ ->
          let ids = htbl_keys selected_exercises in
          let fstat =
            if List.exists (fun id ->
                let st = get_status id in
                ES.(default_assignment st.assignments = Open))
                ids
            then ES.(fun assg ->
                (* fixme: invisible change if the exercise is assigned! *)
                match default_assignment assg with
                | Open -> set_default_assignment assg Closed
                | _ -> assg)
            else ES.(fun assg ->
                (* fixme: invisible change if the exercise is assigned! *)
                match default_assignment assg with
                | Closed -> set_default_assignment assg Open
                | _ -> assg)
          in
          !exercise_status_change (htbl_keys selected_exercises) fstat;
          true)
    ] [H.txt [%i"Open/Close"]];
  in
  let exercise_control_div =
    H.div ~a:[H.a_id "exercise_controls"] [
      open_close_button;
      H.div ~a:[H.a_class ["filler_h"]] [];
      tag_addremove exercise_skills_list_id [%i"required skills"]
        (add_requirements) (remove_requirements);
      H.div ~a:[H.a_id "skills-arrow"]
        [H.txt "\xe2\x87\xa2"]; (* U+21E2, rightwards dashed arrow *)
      tag_addremove exercise_skills_list_id [%i"trained skills"]
        (add_focus) (remove_focus);
    ]
  in
  Manip.appendChild exercises_div exercise_control_div;
  let assignments_div = H.div [] in
  let control_div =
    H.div ~a:[H.a_id "control_pane"] [
      H.fieldset
        ~legend:(H.legend [H.txt [%i"Assignments"]])
        [assignments_div];
    ]
  in
  let fill_control_div () =
    Manip.replaceSelf assignments_div
      (assignments_table ())
  in
  let set_readonly line onoff =
    let attr = Js.string "readonly" in
    List.iter
      (fun e ->
         if onoff then e##setAttribute attr (Js.string "")
         else e##removeAttribute attr)
      (descendants_by_tag line "input")
  in
  let unselect_assignment id =
    selected_assignment := None;
    match Manip.by_id (assg_line_id id) with
    | None -> ()
    | Some line ->
        Manip.removeClass line "selected";
        set_readonly line true
  in
  let apply_changes () =
    Lwt.async @@ fun () ->
    let changes_map =
      SMap.merge (fun id st0 -> function
          | None -> None
          | Some st ->
              let st0 = match st0 with
                | Some s -> s
                | None -> Exercise.Status.(default id)
              in
              if st <> st0 then Some (st0, st) else None)
        !status_map !status_changes
    in
    let changes = SMap.fold (fun _ x acc -> x::acc) changes_map [] in
    let students_changes_map =
      Token.Map.merge (fun tok std0 -> function
          | None -> None
          | Some std ->
              let std0 = match std0 with
                | Some s -> s
                | None -> Student.default tok
              in
              if std <> std0 then Some (std0, std) else None)
        !students_map !students_changes
    in
    let students_changes =
      Token.Map.fold (fun _ x acc -> x::acc) students_changes_map []
    in
    (if changes = [] then Lwt.return () else
       retrieve
         (Learnocaml_api.Set_exercise_status (token, changes)))
    >|= fun () ->
    (if students_changes = [] then Lwt.return () else
       retrieve
         (Learnocaml_api.Set_students_list (token, students_changes)))
    >|= fun () ->
    (* Reload the full tab: a bit more costly, but safer & simpler *)
    teacher_tab token _select _params () >|=
    Manip.replaceSelf (find_component "learnocaml-main-teacher")
    (* status_map := status_current ();
     * status_changes := SMap.empty;
     * Hashtbl.clear selected_exercises;
     * Hashtbl.clear selected_students;
     * Hashtbl.clear assignments_tbl;
     * (match !selected_assignment with None -> () | Some id ->
     *     unselect_assignment id);
     * fill_exercises_pane ();
     * fill_control_div ();
     * Manip.by_classname "selected"
     * |> List.iter (fun elt -> Manip.removeClass elt "selected") *)
  in
  let status_text_div = H.div ~a:[H.a_id "status-text-div"] [] in
  let actions_div =
    H.div ~a:[H.a_id "teacher_menubar"] [
      status_text_div;
      H.button ~a:[
        H.a_id "button_apply";
        (* H.a_disabled (); *)
        H.a_onclick (fun _ -> apply_changes (); true);
      ] [H.txt [%i"Apply"]];
      dropdown ~id:"teacher-actions" ~title:[H.txt [%i"Actions"]] [
        H.ul [
          H.li ~a: [ H.a_onclick (fun _ -> Lwt.async action_new_token; true) ]
            [ H.txt [%i"Create new teacher token"] ];
          H.li ~a: [ H.a_onclick (fun _ -> Lwt.async action_csv_export; true) ]
            [ H.txt [%i"Download student data as CSV"] ];
        ]
      ];
    ]
  in

  (* Implementation of the callbacks *)
  let select_exercise onoff id =
    let class_f, tbl_f = match onoff with
      | true -> Manip.addClass, (fun t k -> Hashtbl.replace t k ())
      | false -> Manip.removeClass, Hashtbl.remove
    in
    tbl_f selected_exercises id;
    match Manip.by_id (exercise_line_id id) with
    | Some elt -> class_f elt "selected"
    | None -> ()
  in
  let select_student onoff std =
    let class_f, tbl_f = match onoff with
      | true -> Manip.addClass, (fun std k -> Hashtbl.replace std k ())
      | false -> Manip.removeClass, Hashtbl.remove
    in
    tbl_f selected_students std;
    match Manip.by_id (student_line_id std) with
    | Some elt -> class_f elt "selected"
    | None -> ()
  in
  let update_disabled_exercises () =
    let disabled ex_ids =
      SSet.iter (Hashtbl.remove selected_exercises) ex_ids;
      List.iter (fun ex ->
          match Manip.by_id (exercise_line_id ex) with
          | None -> ()
          | Some el ->
              if SSet.mem ex ex_ids then
                (Manip.addClass el "disabled";
                 Manip.removeClass el "selected")
              else Manip.removeClass el "disabled")
        (all_exercises !exercises_index)
    in
    let current_assignment = match !selected_assignment with
      | Some id -> let _, _, exos, _ = Hashtbl.find assignments_tbl id in exos
      | None -> SSet.empty
    in
    let tokens, default = get_student_selection () in
    disabled
      (SSet.diff
         (already_assigned_exercises tokens default)
         current_assignment)
  in
  let set_assignment ?assg ?students ?exos ?default id =
    let (assg0, students0, exos0, default0) = Hashtbl.find assignments_tbl id in
    let dft x0 = function Some x -> x | None -> x0 in
    let start, stop = dft assg0 assg in
    let students = dft students0 students in
    let exos = dft exos0 exos in
    let default = dft default0 default in
    Hashtbl.replace assignments_tbl id
      ((start, stop), students, exos, default);
    (match Manip.by_id (assg_line_id id) with
     | Some l -> Manip.replaceSelf l (assignment_line id)
     | None -> failwith "Assignment line not found");
    let status = ES.(Assigned {start; stop}) in
    let exercise_status_changes =
      SSet.fold (fun ex_id acc ->
          let st = get_status ex_id in
          let assg = st.ES.assignments in
          let old_default = assg.ES.default in
          let new_default =
            if default then status
            else if default0 then ES.Closed
            else old_default
          in
          let add tk st tmap =
            if st = new_default then tmap
            else Token.Map.add tk st tmap
          in
          let token_map =
            Token.Map.fold (fun tk _ acc ->
                if Token.Set.mem tk students then
                  if default then acc
                  else Token.Map.add tk status acc
                else if Token.Set.mem tk students0 then
                  if default then Token.Map.add tk ES.Closed acc
                  else Token.Map.remove tk acc
                else add tk (ES.get_status tk assg) acc)
              !students_map Token.Map.empty
          in
          SMap.add ex_id
            ES.{st with assignments = {
                token_map;
                default = new_default;
              }}
            acc)
        exos
        !status_changes
    in
    let exercise_status_changes =
      SSet.fold (fun ex_id acc ->
          let st = get_status ex_id in
          let assg = st.ES.assignments in
          let dft_status =
            if default0 then ES.Closed else ES.default_assignment assg
          in
          let token_map =
            Token.Set.fold Token.Map.remove students0 assg.ES.token_map
          in
          let token_map =
            Token.Map.filter (fun _ a -> a <> dft_status) token_map
          in
          SMap.add ex_id
            ES.{st with assignments = {
                token_map;
                default = dft_status
              }}
            acc)
        (SSet.diff exos0 exos)
        exercise_status_changes
    in
    status_changes := exercise_status_changes;
    fill_exercises_pane ();
    !update_changed_status ();
  in
  let update_disabled_students () =
    let set_enabled onoff student =
      let el = find_component (student_line_id student) in
      if onoff then
        Manip.removeClass el "disabled"
      else
        (Manip.addClass el "disabled";
         Manip.removeClass el "selected")
    in
    let disabled tokens =
      Token.Set.iter
        (fun tk -> Hashtbl.remove selected_students (`Token tk))
        tokens;
      Token.Map.iter
        (fun tk _ -> set_enabled (not (Token.Set.mem tk tokens)) (`Token tk))
        !students_map
    in
    let current_assignment, cur_default = match !selected_assignment with
      | Some id ->
          let _, std, _, dft = Hashtbl.find assignments_tbl id in std, dft
      | None -> Token.Set.empty, false
    in
    disabled
      (Token.Set.diff
         (already_assigned_students (htbl_keys selected_exercises))
         current_assignment);
    let has_default =
      not cur_default &&
      try
        Hashtbl.iter (fun ex _ ->
            match (get_status ex).ES.assignments.ES.default with
            | ES.Assigned _ -> raise Exit
            | _ -> ())
          selected_exercises;
        false
      with Exit -> true
    in
    set_enabled (not has_default) `Any
  in
  let update_disabled_both () =
    update_disabled_exercises ();
    update_disabled_students ();
  in
  update_changed_status := begin fun () ->
    if SMap.is_empty !status_changes &&
       Token.Map.is_empty !students_changes then
      (Manip.replaceChildren status_text_div [];
       Manip.removeClass status_text_div "warning")
    else
      (Manip.replaceChildren status_text_div [H.txt [%i"Unsaved changes"]];
       Manip.addClass status_text_div "warning")
  end;
  toggle_selected_exercises := begin
    fun ?force ?(update = force=None) ids ->
    Lwt.async @@ fun () ->
    let ids, onoff = match force with
      | Some set -> ids, set
      | None ->
          let ids =
            List.filter (fun id ->
                match Manip.by_id (exercise_line_id id) with
                | Some elt ->
                    not (Manip.hasClass elt "disabled") &&
                    not (Manip.hasClass elt "exercise_hidden")
                | None -> false)
              ids
          in
          ids, not @@ List.exists (Hashtbl.mem selected_exercises) ids
    in
    List.iter (select_exercise onoff) ids;
    (match !selected_assignment with
     | None -> ()
     | Some aid ->
         set_assignment aid ~exos:(SSet.of_list (htbl_keys selected_exercises)));
    if update then update_disabled_both ();
    Lwt.return_unit
  end;
  toggle_selected_students := begin
    fun ?force ?(update = force=None) students ->
    Lwt.async @@ fun () ->
    let students, onoff = match force with
      | Some set -> students, set
      | None ->
          let students =
            List.filter (fun tk ->
                match Manip.by_id (student_line_id tk) with
                | Some elt -> not (Manip.hasClass elt "disabled")
                | None -> false)
              students
          in
          students, not @@ List.exists (Hashtbl.mem selected_students) students
    in
    List.iter (select_student onoff) students;
    if update then
      ((match !selected_assignment with
          | None -> ()
          | Some aid ->
              let students, default = get_student_selection () in
              set_assignment aid ~students ~default);
       update_disabled_exercises ());
    Lwt.return_unit
  end;
  toggle_select_assignment := begin fun assg_id ->
    Lwt.async @@ fun () ->
    let select id =
      match Manip.by_id (assg_line_id id) with
      | None -> ()
      | Some line ->
          let (_assg, students, exos, default) =
            Hashtbl.find assignments_tbl id
          in
          !toggle_selected_exercises ~force:false
            (all_exercises !exercises_index);
          !toggle_selected_exercises ~force:true (SSet.elements exos);

          !toggle_selected_students ~force:false
            (Token.Map.fold (fun tk _ acc -> (`Token tk)::acc)
               !students_map
               [`Any]);
          !toggle_selected_students ~force:true
            (Token.Set.fold (fun tk acc -> (`Token tk)::acc)
               students
               (if default then [`Any] else []));

          selected_assignment := Some id;
          Manip.addClass line "selected";
          set_readonly line false
    in
    (match !selected_assignment with
     | Some aid ->
         unselect_assignment aid;
         if aid <> assg_id then select assg_id
     | None ->
         select assg_id);
    (match !selected_assignment with
     | Some _ -> Manip.disable open_close_button
     | None -> Manip.enable open_close_button);
    update_disabled_both ();
    Lwt.return_unit
  end;
  exercise_status_change := begin fun ids f ->
    status_changes :=
      List.fold_left (fun acc id ->
          let st = get_status id in
          SMap.add id ES.{st with assignments = f st.assignments} acc)
        !status_changes ids;
    fill_exercises_pane ();
    update_disabled_exercises ();
    !update_changed_status ();
  end;
  student_change := begin fun _tk () ->
    fill_students_pane ();
  end;
  assignment_change := begin fun assg_id ->
    let ((start0, stop0), _, _, _) = Hashtbl.find assignments_tbl assg_id in
    let get_date id =
      let retr =
        match Manip.by_id ("date_"^id), Manip.by_id ("time_"^id) with
        | Some d, Some t ->
            (try
               Some
                 (Scanf.sscanf (Manip.value d) "%d-%d-%d"
                    (fun yr mon d -> yr, mon, d),
                  try Scanf.sscanf (Manip.value t) "%d:%d" (fun hr mn -> hr, mn)
                  with Scanf.Scan_failure _ | End_of_file -> 0, 0)
             with Scanf.Scan_failure _ | End_of_file -> None)
        | _ -> None
      in
      match retr with
      | Some ((yr, mon, d), (hr, min)) ->
          let t = new%js Js.date_min yr (mon - 1) d hr min in
          Some (t##getTime /. 1000.)
      | None -> None
    in
    let start = match get_date ("start_"^assg_line_id assg_id) with
      | Some t -> t
      | None -> start0
    in
    let stop = match get_date ("stop_"^assg_line_id assg_id) with
      | Some t -> if t < start then start else t
      | None -> stop0
    in
    set_assignment assg_id ~assg:(start, stop)
  end;

  assignment_remove := begin fun assg_id ->
    Lwt.async @@ fun () ->
    set_assignment assg_id
      ~students:Token.Set.empty
      ~exos:SSet.empty;
    Hashtbl.remove assignments_tbl assg_id;
    selected_assignment := None;
    fill_exercises_pane ();
    update_disabled_both ();
    (match Manip.by_id (assg_line_id assg_id) with
     | None -> ()
     | Some el -> Manip.removeSelf el);
    Lwt.return_unit
  end;

  let fill_students_progression () =
    let now = gettimeofday () in
    let students_assignments =
      let addl x tok tmap =
        Token.Map.add tok
          (x :: try Token.Map.find tok tmap with Not_found -> [])
          tmap
      in
      Hashtbl.fold (fun _ ((start, stop), tokens, exos, _dft) acc ->
          if start > now then acc
          else Token.Set.fold (addl (stop, exos)) tokens acc)
        assignments_tbl
        (Token.Map.map (fun _ -> []) !students_map)
    in
    let open_exercises =
      SMap.fold (fun ex st acc ->
          if ES.(st.assignments.default = Open) then ex::acc else acc)
        !status_map []
      |> List.rev
    in
    let css_gradient = function
      | [] -> "background:white"
      | _::_ as l ->
          let step = 100. /. float_of_int (List.length l) in
          Printf.sprintf "background:linear-gradient(to right,%s)"
            (String.concat ","
               (List.mapi (fun i score ->
                    Printf.sprintf "%s %.0f%%,%s %.0f%%"
                      (grade_color score) (float_of_int i *. step)
                      (grade_color score) (float_of_int (i+1) *. step))
                   l))
    in
    let div_assg ~cls grades =
      H.div ~a:[H.a_class [cls]; H.a_style (css_gradient grades)] []
    in
    Token.Map.iter (fun tok st ->
        let parent_div = find_component (student_progression_id tok) in
        let assgs = Token.Map.find tok students_assignments in
        let assgs = List.sort compare assgs in
        let status = st.Student.results in
        let grades exlist =
          List.map (fun ex ->
              match SMap.find_opt ex status with
              | Some (_, g) -> g
              | _ -> None)
            exlist
        in
        Manip.replaceChildren parent_div @@
        List.map (fun (deadl,exos) ->
            div_assg
              ~cls:(if deadl < now then "progr-closed" else "progr-assigned")
              (grades (SSet.elements exos)))
          assgs;
        if open_exercises <> [] then
          Manip.appendChild parent_div @@
          div_assg ~cls:"progr-open" (grades open_exercises))
      !students_map
  in

  let div =
    H.div ~a: [H.a_id "learnocaml-main-teacher"] [
      exercises_div;
      students_div;
      (* skills_div; *)
      control_div;
      actions_div;
    ]
  in
  let fetch_exercises =
    retrieve (Learnocaml_api.Exercise_index (Some token))
    >|= fun (index, _) ->
    exercises_index := index
  in
  let fetch_stats =
    retrieve (Learnocaml_api.Exercise_status_index token)
    >|= fun statuses ->
    let map =
      List.fold_left (fun m ex -> SMap.add ex.ES.id ex m)
        SMap.empty statuses
    in
    status_map := map
  in
  let fetch_students =
    retrieve (Learnocaml_api.Students_list token)
    >|= fun students ->
    students_map :=
      List.fold_left (fun m st -> Token.Map.add st.Student.token st m)
        Token.Map.empty students
  in
  let content_div = find_component "learnocaml-main-content" in
  Manip.appendChild content_div div;
  Lwt.join [fetch_exercises; fetch_stats; fetch_students] >>= fun () ->
  exercises_index :=
    Exercise.Index.map_exercises (fun id meta ->
        let st = get_status id in
        Exercise.Meta.{ meta with
          requirements =
            ES.skills_base ~current:meta.requirements st.ES.skills_prereq;
          focus =
            ES.skills_base ~current:meta.focus st.ES.skills_focus; })
      !exercises_index;
  fill_exercises_pane ();
  (* fill_skills_pane (); *)
  fill_students_pane ();
  fill_control_div ();
  fill_students_progression ();
  Lwt.return div
