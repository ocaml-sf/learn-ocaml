(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2018 OCamlPro.
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
open Learnocaml_index
open Learnocaml_common

module H = Tyxml_js.Html5
module ES = Exercise.Status

let teacher_tab token _select _params () =
  let action_new_token () =
    Server_caller.request_exn (Learnocaml_api.Create_teacher_token token)
    >|= fun new_token ->
    alert ~title:[%i"TEACHER TOKEN"]
      (Printf.sprintf [%if"New teacher token created:\n%s\n\n\
                           write it down."]
         (Token.to_string new_token))
  in
  let action_csv_export () =
    Server_caller.request_exn (Learnocaml_api.Students_csv (token, [], []))
    >|= fun csv ->
    Learnocaml_common.fake_download
      ~name:"learnocaml.csv"
      ~contents:(Js.string csv)
  in
  let indent_style lvl =
    H.a_style (Printf.sprintf "text-align: left; padding-left: %dem;" lvl)
  in
  let tag_div tag =
    H.span ~a:[H.a_class ["exercise-tag"]] [H.pcdata tag]
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
  let students_map = ref Token.Map.empty in
  let assignments_tbl = Hashtbl.create 59 in
  (* let students_changes = ref Token.Map.empty in -- TODO *)
  let status_map = ref SMap.empty in
  let (status_changes: ES.t SMap.t ref) = ref SMap.empty in
  let status_current () =
    SMap.merge (fun id old newer -> match newer with None -> old | s -> s)
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
  let exercise_line_id id = "learnocaml_exercise_"^id in
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
  let toggle_selected_exercises = ref (fun ?force _ -> assert false) in
  let toggle_selected_students = ref (fun ?force _ -> assert false) in
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
                H.a_id ("exercise_group_"^id);
                H.a_class ["exercise_group"];
                H.a_onclick (fun _ ->
                    !toggle_selected_exercises all_children; false);
              ] [
                H.td [];
                H.th ~a:[H.a_colspan 10; indent_style group_level]
                  [H.pcdata g.Exercise.Index.title];
              ] :: acc
            in
            mk_table (group_level + 1) acc status g.Exercise.Index.contents)
          acc groups_list
    | Exercise.Index.Exercises exlist ->
        List.fold_left (fun acc (id, meta) ->
            match meta with None -> acc | Some meta ->
            let st = status id in
            let hid = exercise_line_id id in
            let classes =
              (if exercise_changed_status id then ["changed"] else []) @
              (if Hashtbl.mem selected_exercises id then ["selected"] else [])
            in
            H.tr ~a:[
              H.a_id hid;
              H.a_class ("exercise_line" :: classes);
              H.a_onclick (fun _ -> !toggle_selected_exercises [id]; false);
            ] [
              auto_checkbox_td ();
              H.td ~a:[indent_style group_level]
                [ H.pcdata meta.Exercise.Meta.title ];
              H.td (List.map H.pcdata meta.Exercise.Meta.focus);
              H.td [stars_div meta.Exercise.Meta.stars];
              H.td (List.map tag_div st.ES.tags);
              H.td [
                let cls, text =
                  if Token.Map.is_empty ES.(st.assignments.token_map) then
                    match ES.(st.assignments.default) with
                    | ES.Open -> "exo_open", [%i"Open"]
                    | ES.Closed -> "exo_closed", [%i"Closed"]
                    | ES.Assigned a -> "exo_assigned", [%i"Assigned"]
                  else "exo_assigned", [%i"Assigned"]
                in
                H.span ~a:[H.a_class [cls]] [H.pcdata text]
              ];
            ] :: acc)
          acc exlist
  in
  let exercises_list_div =
    H.div ~a:[H.a_id "exercises_list"] [H.pcdata [%i"Loading..."]]
  in
  let exercises_div =
    let legend =
      H.legend ~a:[
        H.a_onclick (fun _ ->
            !toggle_selected_exercises (all_exercises !exercises_index);
            true);
      ] [H.pcdata [%i"Exercises"]; H.pcdata " \xe2\x98\x90" (* U+2610 *)]
    in
    H.div ~a:[H.a_id "exercises_pane"; H.a_class ["learnocaml_pane"]] [
      H.div ~a:[H.a_id "exercises_filter_box"] [
        (* TODO: filtering tools *)
      ];
      H.fieldset ~legend [ exercises_list_div ]
    ]
  in
  let students_list_div =
    H.div ~a:[H.a_id "students_list"] [H.pcdata [%i"Loading..."]];
  in
  let students_div =
    let legend =
      H.legend ~a:[
        H.a_onclick (fun _ ->
            !toggle_selected_students
              (Token.Map.fold (fun k _ acc -> (`Token k)::acc)
                 !students_map [`Any]);
            true
          );
      ] [H.pcdata [%i"Students"];  H.pcdata " \xe2\x98\x90" (* U+2610 *)]
    in
    H.div ~a:[H.a_id "students_pane"; H.a_class ["learnocaml_pane"]] [
      H.div ~a:[H.a_id "students_filter_box"] [
        (* TODO: filtering tools *)
      ];
      H.fieldset ~legend [ students_list_div ];
    ]
  in
  let fill_exercises_pane () =
    let table = List.rev (mk_table 0 [] get_status !exercises_index) in
    Manip.replaceChildren exercises_list_div [H.table table];
  in
  let html_token tk =
    H.span ~a:[H.a_class ["learnocaml_token"]]
      [H.pcdata (Token.to_string tk)]
  in
  let make_student_line st contents =
    H.tr ~a:[
      H.a_id (student_line_id st);
      H.a_class ["student_line"];
      H.a_onclick (fun _ ->
          !toggle_selected_students [st];
          true);
    ] (auto_checkbox_td () :: contents)
  in
  let anystudents_line =
    make_student_line `Any [
      H.td ~a:[H.a_colspan 10; H.a_class ["future_students"]] [
        H.pcdata [%i"any future students"]
          (* U+3008, U+3009 *)
      ];
    ]
  in
  let fill_students_pane () =
    let table =
      anystudents_line ::
      List.rev_map (fun st ->
          make_student_line (`Token  st.Student.token) [
            H.td [html_token st.Student.token];
            H.td (match st.Student.nickname with
                | Some n -> [H.pcdata n]
                | _ -> []);
          ])
        (Token.Map.fold (fun _ st acc -> st :: acc) !students_map [])
    in
    Manip.replaceChildren students_list_div [H.table table]
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
    (* let switch_autobox ~current_mode mode1 mode2 =
     *   let switch = ref current_mode in
     *   let next () = switch := not !switch in
     *   let get () = if !switch then mode1 else mode2 in
     *   let id = Random.(Printf.sprintf "switch_%x%x" (bits ()) (bits ())) in
     *   fun cb ->
     *   let cb _ =
     *     next ();
     *     cb !switch;
     *     begin match Manip.by_id id with
     *     | None ->
     *        ()
     *     | Some id ->
     *        Manip.(replaceChildren id [get ()]);
     *     end;
     *     true
     *   in
     *   H.span ~a:[H.a_id id; H.a_onclick cb] [get ()]
     * in *)
    let hid = assg_line_id id in
    let ((start, stop), tokens, exo_ids, default) =
      Hashtbl.find assignments_tbl id
    in
    let cls = [] in
      (* let n = gettimeofday () in
     *   if stop < n then ["assg_finished"]
     *   else if start > n then ["assg_notstarted"]
     *   else ["assg_active"]
     * in *)
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
      H.td [H.pcdata n_exos];
      H.td [H.pcdata n_students];
      H.td ~a:[H.a_onclick (fun _ -> !assignment_remove id; false);
               H.a_class ["remove-cross"]]
        [H.pcdata "\xe2\xa8\x82" (* U+2A02 *)];
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
    let module ATM = Map.Make(struct
        type t = (float * float) * Token.Set.t * bool
        let compare (d1, ts1, dft1) (d2, ts2, dft2) =
          match compare d1 d2 with
          | 0 -> (match Token.Set.compare ts1 ts2 with
              | 0 -> compare dft1 dft2
              | n -> n)
          | n -> n
      end)
    in
    let all_tokens =
      Token.Map.fold (fun t _ -> Token.Set.add t) !students_map Token.Set.empty
    in
    let atm =
      SMap.fold (fun id st atm ->
          let assg = st.ES.assignments in
          let stl = ES.by_status all_tokens assg in
          let default = ES.default_assignment assg in
          List.fold_left (fun atm (status, tokens) ->
              match status with
              | ES.Open | ES.Closed -> atm
              | ES.Assigned {start; stop} ->
                  let key = (start, stop), tokens, (status = default) in
                  match ATM.find_opt key atm with
                  | None ->
                      ATM.add key (SSet.singleton id) atm
                  | Some ids ->
                     ATM.add key (SSet.add id ids) atm)
            atm
            stl)
        (status_current ())
        ATM.empty
    in
    let line_n = ref 0 in
    let make_line ?selected assg tokens exo_ids mode =
      incr line_n;
      let id = !line_n in
      Hashtbl.add assignments_tbl id (assg, tokens, exo_ids, mode);
      assignment_line id
    in
    let new_assg_id = "new_assignment" in
    let table = H.table [] in
    let new_assg_line =
      H.tr ~a:[
        H.a_id new_assg_id;
      ] [
        H.td ~a:[H.a_colspan 10]
          [ H.button [H.pcdata [%i"New assignment"]] ]
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
    Manip.Ev.onclick new_assg_line (fun _ -> new_assignment (); false);
    Manip.replaceChildren table @@
    (List.rev
       (ATM.fold (fun (assg, tokens, dft) exos acc ->
            make_line assg tokens exos dft :: acc)
           atm [])) @
    [new_assg_line];
    table
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
    ] [H.pcdata [%i"Open/Close"]];
  in
  let exercise_control_div =
    H.div ~a:[H.a_id "exercise_controls"] [
      open_close_button
      (* H.button ~a:[ H.a_disabled () ]
       *   [H.pcdata [%i"Add assignment"]];
       * H.button ~a:[ H.a_disabled () ]
       *   [H.pcdata [%i"Add tag"]];
       * H.button ~a:[ H.a_disabled () ]
       *   [H.pcdata [%i"Remove tag"]]; *)
    ]
  in
  Manip.appendChild exercises_div exercise_control_div;
  let assignments_div = H.div [] in
  let control_div =
    H.div ~a:[H.a_id "control_pane"] [
      H.fieldset
        ~legend:(H.legend [H.pcdata [%i"Assignments"]])
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
                | None -> Exercise.Status.(default st.id)
              in
              if st <> st0 then Some (st0, st) else None)
        !status_map !status_changes
    in
    let changes = SMap.fold (fun _ x acc -> x::acc) changes_map [] in
    Server_caller.request_exn
      (Learnocaml_api.Set_exercise_status (token, changes)) >|= fun () ->
    reload ();
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
      ] [H.pcdata [%i"Apply"]];
      dropdown "teacher-actions" [H.pcdata [%i"Actions"]] [
        H.ul [
          H.li ~a: [ H.a_onclick (fun _ -> Lwt.async action_new_token; true) ]
            [ H.pcdata [%i"Create new teacher token"] ];
          H.li ~a: [ H.a_onclick (fun _ -> Lwt.async action_csv_export; true) ]
            [ H.pcdata [%i"Download student data as CSV"] ];
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
            Token.Map.filter (fun tok a -> a <> dft_status) token_map
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


    (* let set_assg st tmap default_assignment =
 *       let a = Exercise.Status.make_assignments tmap default_assignment in
 * (\* =======
 *  *         let mode =
 *  *           (\\* For the moment, we only offer two modes, manual
 *  *              or automatic. *\\)
 *  *           Exercise.Status.(if mode then True else False)
 *  *         in
 *  *         let a =
 *  *           Exercise.Status.(make_assignments tmap default_assignment0 mode)
 *  *         in
 *  * >>>>>>> f55556f4... Allow the selection of two modes for assignments *\)
 *       Exercise.Status.(set_status st.assignments a)
 *     in
 *     let ch =
 *       SSet.fold (fun ex_id acc ->
 *           let st = get_status ex_id in
 *           let open Exercise.Status in
 *           let tmap0 =
 *             match st.status with
 *             | Open | Closed -> Token.Map.empty
 *             | Assigned a -> token_map_of_assignments a
 *           in
 *           let tmap =
 *             Token.Set.fold (fun tk -> Token.Map.remove tk)
 *               (Token.Set.diff students0 students) tmap0
 *           in
 *           let tmap =
 *             Token.Set.fold (fun tk -> Token.Map.add tk assg)
 *               students tmap
 *           in
 *           (\* The most recent assignment is used for students that will
 *              register *after* the creation of this homework. *\)
 *           let default_assignment =
 *             assg
 *           in
 *           SMap.add ex_id (set_assg st tmap default_assignment) acc)
 *         exos
 *         !status_changes
 *     in
 *     let ch =
 *       SSet.fold (fun ex_id acc ->
 *           let st = get_status ex_id in
 *           let open Exercise.Status in
 *           let tmap0 =
 *             match st.status with
 *             | Open | Closed -> Token.Map.empty
 *             | Assigned a -> token_map_of_assignments a
 *           in
 *           let tmap =
 *             Token.Set.fold (fun tk -> Token.Map.remove tk) students0 tmap0
 *           in
 *           let default_assignment =
 *             assg
 *           in
 *           SMap.add ex_id (set_assg st tmap default_assignment) acc)
 *         (SSet.diff exos0 exos)
 *         ch
 *     in
 *     status_changes := ch;
 *     fill_exercises_pane ();
 *     !update_changed_status ();
 *   in *)
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
    if SMap.is_empty !status_changes then
      (Manip.replaceChildren status_text_div [];
       Manip.removeClass status_text_div "warning")
    else
      (Manip.replaceChildren status_text_div [H.pcdata [%i"Unsaved changes"]];
       Manip.addClass status_text_div "warning")
  end;
  toggle_selected_exercises := begin fun ?force ids ->
    Lwt.async @@ fun () ->
    let ids, onoff = match force with
      | Some set -> ids, set
      | None ->
          let ids =
            List.filter (fun id ->
                match Manip.by_id (exercise_line_id id) with
                | Some elt -> not (Manip.hasClass elt "disabled")
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
    if force = None then update_disabled_both ();
    Lwt.return_unit
  end;
  toggle_selected_students := begin fun ?force students ->
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
    match force with
    | Some _ -> Lwt.return_unit
    | None ->
        (match !selected_assignment with
         | None -> ()
         | Some aid ->
             let students, default = get_student_selection () in
             set_assignment aid ~students ~default);
        update_disabled_exercises ();
        Lwt.return_unit
  end;
  toggle_select_assignment := begin fun assg_id ->
    Lwt.async @@ fun () ->
    let select id =
      match Manip.by_id (assg_line_id id) with
      | None -> ()
      | Some line ->
          let (assg, students, exos, default) = Hashtbl.find assignments_tbl id in
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
  student_change := begin fun tk () ->
    ()
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

  let div =
    H.div ~a: [H.a_id "learnocaml-main-teacher"] [
      exercises_div;
      students_div;
      control_div;
      actions_div;
    ]
  in
  let fetch_exercises =
    Server_caller.request_exn (Learnocaml_api.Exercise_index token)
    >|= fun (index, _) -> exercises_index := index
  in
  let fetch_stats =
    Server_caller.request_exn (Learnocaml_api.Exercise_status_index token)
    >|= fun statuses ->
    let map =
      List.fold_left (fun m ex -> SMap.add ex.ES.id ex m)
        SMap.empty statuses
    in
    status_map := map
  in
  let fetch_students =
    Server_caller.request_exn (Learnocaml_api.Students_list token)
    >|= fun students ->
    students_map :=
      List.fold_left (fun m st -> Token.Map.add st.Student.token st m)
        Token.Map.empty students
  in
  let content_div = find_component "learnocaml-main-content" in
  Manip.appendChild content_div div;
  Lwt.join [fetch_exercises; fetch_stats; fetch_students] >>= fun () ->
  fill_exercises_pane ();
  fill_students_pane ();
  fill_control_div ();
  Lwt.return div
