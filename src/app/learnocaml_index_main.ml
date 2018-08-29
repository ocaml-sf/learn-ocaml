(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2016 OCamlPro.
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

let stars_div stars =
  H.div ~a:[ H.a_class [ "stars" ] ] [
    let num = 5 * int_of_float (stars *. 2.) in
    let num = max (min num 40) 0 in
    let alt = Format.asprintf [%if"difficulty: %d / 40"] num in
    let src = Format.asprintf "icons/stars_%02d.svg" num in
    H.img ~alt ~src ()
  ]

let exercises_tab token _ _ () =
  show_loading ~id:"learnocaml-main-loading"
    Tyxml_js.Html5.[ ul [ li [ pcdata [%i"Loading exercises"] ] ] ] ;
  Lwt_js.sleep 0.5 >>= fun () ->
  Server_caller.request_exn (Learnocaml_api.Exercise_index token)
  >>= fun (index, deadlines) ->
  let content_div = find_component "learnocaml-main-content" in
  let format_exercise_list all_exercise_states =
    let rec format_contents lvl acc contents =
      let open Tyxml_js.Html5 in
      match contents with
      | Exercise.Index.Exercises exercises ->
          List.fold_left
            (fun acc (exercise_id, meta_opt) ->
              match meta_opt with None -> acc | Some meta ->
              let {Exercise.Meta.kind; title; short_description; stars; _ } =
                meta
              in
              let pct_init =
                match SMap.find exercise_id all_exercise_states with
                | exception Not_found -> None
                | { Answer.grade } -> grade in
              let pct_signal, pct_signal_set = React.S.create pct_init in
              Learnocaml_local_storage.(listener (exercise_state exercise_id)) :=
                Some (function
                    | Some { Answer.grade } -> pct_signal_set grade
                    | None -> pct_signal_set None) ;
              let pct_text_signal =
                React.S.map
                  (function
                    | None -> "--"
                    | Some 0 -> "0%"
                    | Some pct -> string_of_int pct ^ "%")
                  pct_signal in
              let time_left = match List.assoc_opt exercise_id deadlines with
                | None -> ""
                | Some 0. -> [%i"Exercise closed"]
                | Some f -> Printf.sprintf [%if"Time left: %s"]
                              (string_of_seconds (int_of_float f))
              in
              let status_classes_signal =
                React.S.map
                  (function
                    | None -> [ "stats" ]
                    | Some 0 -> [ "stats" ; "failure" ]
                    | Some pct when  pct >= 100 -> [ "stats" ; "success" ]
                    | Some _ -> [ "stats" ; "partial" ])
                  pct_signal in
              a ~a:[ a_href ("exercise.html#id=" ^ exercise_id ^ "&action=open") ;
                     a_class [ "exercise" ] ] [
                div ~a:[ a_class [ "descr" ] ] [
                  h1 [ pcdata title ] ;
                  p [ match short_description with
                      | None -> pcdata [%i"No description available."]
                      | Some text -> pcdata text ] ;
                ] ;
                div ~a:[ a_class [ "time-left" ] ] [H.pcdata time_left];
                div ~a:[ Tyxml_js.R.Html5.a_class status_classes_signal ] [
                  stars_div stars;
                  div ~a:[ a_class [ "length" ] ] [
                    match kind with
                    | Exercise.Meta.Project -> pcdata [%i"project"]
                    | Exercise.Meta.Problem -> pcdata [%i"problem"]
                    | Exercise.Meta.Exercise -> pcdata [%i"exercise"] ] ;
                  div ~a:[ a_class [ "score" ] ] [
                    Tyxml_js.R.Html5.pcdata pct_text_signal
                  ]
                ] ] ::
              acc)
            acc exercises
      | Exercise.Index.Groups groups ->
          let h = match lvl with 1 -> h1 | 2 -> h2 | _ -> h3 in
          List.fold_left
            (fun acc (_, Exercise.Index.{ title ; contents }) ->
               format_contents (succ lvl)
                 (h ~a:[ a_class [ "pack" ] ] [ pcdata title ] :: acc)
                 contents)
            acc groups in
    List.rev (format_contents 1 [] index) in
  let list_div =
    match format_exercise_list
            Learnocaml_local_storage.(retrieve all_exercise_states)
    with
    | [] -> H.div [H.pcdata [%i"No open exercises at the moment"]]
    | l -> H.div ~a:[H.a_id "learnocaml-main-exercise-list"] l
  in
    Manip.appendChild content_div list_div;
  hide_loading ~id:"learnocaml-main-loading" () ;
  Lwt.return list_div
;;

let lessons_tab select (arg, set_arg, delete_arg) () =
  show_loading ~id:"learnocaml-main-loading"
    Tyxml_js.Html5.[ ul [ li [ pcdata [%i"Loading lessons"] ] ] ] ;
  Lwt_js.sleep 0.5 >>= fun () ->
  Server_caller.fetch_lesson_index () >>= fun index ->
  let content_div = find_component "learnocaml-main-content" in
  let navigation_div =
    Tyxml_js.Html5.(div ~a: [ a_class [ "navigation" ] ] []) in
  let main_div =
    Tyxml_js.Html5.(div ~a: [ a_class [ "toplevel-pane" ] ] []) in
  let options =
    List.map
      (fun (lesson_id, lesson_title) ->
         lesson_id,
         Tyxml_js.Html5.
           (option ~a: [ a_value lesson_id ] (pcdata lesson_title)))
      index in
  let prev_and_next id =
    let rec loop = function
      | [] -> assert false
      | [ _ ] (* assumes single id *) -> None, None
      | (one, _) :: (two, _) :: _ when id = one -> None, Some two
      | (one, _) :: (two, _) :: [] when id = two -> Some one, None
      | (one, _) :: (two, _) :: (three, _) :: _ when id = two -> Some one, Some three
      |  _ :: rest -> loop rest
    in loop index in
  let selector =
    Tyxml_js.Html5.(select (snd (List.split options))) in
  let prev_button_state = button_state () in
  let next_button_state = button_state () in
  let load_lesson ~loading () =
    let selector = Tyxml_js.To_dom.of_select selector in
    let id = Js.to_string selector##.value in
    Server_caller.fetch_lesson id >>= fun { Lesson.steps } ->
    Manip.removeChildren main_div ;
    if loading then begin
      show_loading ~id:"learnocaml-main-loading"
        Tyxml_js.Html5.[ ul [ li [ pcdata [%i"Running OCaml examples"] ] ] ]
    end ;
    let timeout_prompt =
      Learnocaml_toplevel.make_timeout_popup
        ~on_show: (fun () -> Lwt.async select)
        () in
    let flood_prompt =
      Learnocaml_toplevel.make_flood_popup
        ~on_show: (fun () -> Lwt.async select)
        () in
    let history =
      let storage_key =
        Learnocaml_local_storage.toplevel_history ("lesson-" ^ id) in
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
    let toplevel_buttons_group = button_group () in
    disable_button_group toplevel_buttons_group (* enabled after init *) ;
    Learnocaml_toplevel.create
      ~display_welcome: false
      ~on_disable_input: (fun _ -> disable_button_group toplevel_buttons_group)
      ~on_enable_input: (fun _ -> enable_button_group toplevel_buttons_group)
      ~history ~timeout_prompt ~flood_prompt
      ~container: main_div
      () >>= fun top ->
    Lwt_list.iter_s
      (fun { Lesson.step_title ; step_phrases } ->
         Learnocaml_toplevel.print_html top ("<h3>" ^ step_title ^ "</h3>") ;
         let do_phrase = function
           | Lesson.Text text ->
               Learnocaml_toplevel.print_html top text ;
               Lwt.return ()
           | Lesson.Code code ->
               Learnocaml_toplevel.execute_phrase top code >>= fun _ ->
               Lwt.return () in
         Lwt_list.iter_s do_phrase step_phrases >>= fun () ->
         Lwt.return ()
      )
      steps >>= fun () ->
    set_arg "lesson" id ;
    begin match prev_and_next id with
      | None, None ->
          disable_button prev_button_state ;
          disable_button next_button_state
      | Some _, None ->
          enable_button prev_button_state ;
          disable_button next_button_state
      | None, Some _ ->
          disable_button prev_button_state ;
          enable_button next_button_state
      | Some _, Some _ ->
          enable_button prev_button_state ;
          enable_button next_button_state
    end ;
    if loading then begin
      hide_loading ~id:"learnocaml-main-loading" () ;
    end ;
    Lwt.return () in
  let group = button_group () in
  begin button
      ~group ~state: prev_button_state ~container: navigation_div
      ~theme: "black" ~icon: "left" [%i"Prev"] @@ fun () ->
    let selector = Tyxml_js.To_dom.of_select selector in
    let id = Js.to_string selector##.value in
    match prev_and_next id with
    | Some prev, _ ->
        let option = Tyxml_js.To_dom.of_option (List.assoc prev options) in
        option##.selected := Js._true ;
        load_lesson ~loading: true ()
    | _ -> Lwt.return ()
  end ;
  Manip.appendChild navigation_div selector ;
  disable_with_button_group (Tyxml_js.To_dom.of_select selector) group ;
  (Tyxml_js.To_dom.of_select selector)##.onchange :=
    Dom_html.handler (fun _ -> Lwt.async (load_lesson ~loading: true) ; Js._true) ;
  begin button
      ~group ~state: next_button_state ~container: navigation_div
      ~theme: "black" ~icon: "right" [%i"Next"] @@ fun () ->
    let selector = Tyxml_js.To_dom.of_select selector in
    let id = Js.to_string selector##.value in
    match prev_and_next id with
    | _, Some next ->
        let option = Tyxml_js.To_dom.of_option (List.assoc next options) in
        option##.selected := Js._true ;
        load_lesson ~loading: true ()
    | _ -> Lwt.return ()
  end ;
  let lesson_div =
    Tyxml_js.Html5.(div ~a: [ a_id "learnocaml-main-lesson" ])
      [ navigation_div ; main_div ] in
  Manip.appendChild content_div lesson_div ;
  begin try
      let id = match arg "lesson" with
        | id -> id
        | exception Not_found -> match index with
          | [] -> raise Not_found
          | (id, _) :: _ -> id in
      let option = Tyxml_js.To_dom.of_option (List.assoc id options) in
      option##.selected := Js._true ;
      load_lesson ~loading: false ()
    with Not_found -> failwith "lesson not found"
  end >>= fun () ->
  hide_loading ~id:"learnocaml-main-loading" () ;
  Lwt.return lesson_div
;;

let tryocaml_tab select (arg, set_arg, delete_arg) () =
  let open Tutorial in
  let navigation_div =
    Tyxml_js.Html5.(div ~a: [ a_class [ "navigation" ] ] []) in
  let step_title_container =
    Tyxml_js.Html5.h3 [] in
  let step_title =
    Tyxml_js.Html5.span [] in
  let step_items_container =
    Tyxml_js.Html5.div [] in
  let step_div =
    Tyxml_js.Html5.(div ~a: [ a_class [ "step-pane" ] ]
                      [ step_title_container ;
                        step_items_container ]) in
  let toplevel_div =
    Tyxml_js.Html5.(div ~a: [ a_class [ "toplevel-pane" ] ] []) in
  let buttons_div =
    Tyxml_js.Html5.(div ~a: [ a_class [ "buttons" ] ] []) in
  let tutorial_div =
    Tyxml_js.Html5.(div ~a: [ a_id "learnocaml-main-tryocaml" ])
      [ navigation_div ; step_div ; toplevel_div ; buttons_div ] in
  let timeout_prompt =
    Learnocaml_toplevel.make_timeout_popup
      ~on_show: (fun () -> Lwt.async select)
      () in
  let flood_prompt =
    Learnocaml_toplevel.make_flood_popup
      ~on_show: (fun () -> Lwt.async select)
      () in
  let history =
    let storage_key =
      Learnocaml_local_storage.toplevel_history "tryocaml" in
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
  let toplevel_buttons_group = button_group () in
  disable_button_group toplevel_buttons_group (* enabled after init *) ;
  let toplevel_launch =
    Learnocaml_toplevel.create
      ~on_disable_input: (fun _ ->
          Manip.addClass step_div "disabled" ;
          disable_button_group toplevel_buttons_group)
      ~on_enable_input: (fun _ ->
          Manip.removeClass step_div "disabled" ;
          enable_button_group toplevel_buttons_group)
      ~history ~timeout_prompt ~flood_prompt
      ~container: toplevel_div
      () in
  show_loading ~id:"learnocaml-main-loading"
    Tyxml_js.Html5.[ ul [ li [ pcdata [%i"Loading tutorials"] ] ] ] ;
  Lwt_js.sleep 0.5 >>= fun () ->
  let content_div = find_component "learnocaml-main-content" in
  Manip.appendChild content_div tutorial_div ;
  Server_caller.fetch_tutorial_index () >>= fun index ->
  let index =
    List.flatten @@ List.fold_left
      (fun acc (_, { Index.series_tutorials }) ->
         series_tutorials :: acc)
      [] index in
  let options =
    List.map
      (fun { Tutorial.Index.name; title } ->
         name,
         H.option ~a: [ H.a_value name ]
           (H.pcdata (extract_text_from_rich_text title)))
      index in
  let selector =
    Tyxml_js.Html5.(select (snd (List.split options))) in
  let dom_selector =
    Tyxml_js.To_dom.of_select selector in
  let prev_and_next id =
    let rec loop = function
      | [] -> assert false
      | [ _ ] (* assumes single id *) -> None, None
      | { Tutorial.Index.name = one } ::
        { Tutorial.Index.name = two } :: _ when id = one -> None, Some two
      | { Tutorial.Index.name = one } ::
        { Tutorial.Index.name = two } :: [] when id = two -> Some one, None
      | { Tutorial.Index.name = one } ::
        { Tutorial.Index.name = two } ::
        { Tutorial.Index.name = three } :: _ when id = two -> Some one, Some three
      |  _ :: rest -> loop rest
    in loop index in
  let current_tutorial_name = ref @@
    match arg "tutorial" with
    | exception Not_found -> (List.hd index).Tutorial.Index.name
    | tutorial_name -> tutorial_name in
  let current_step_id = ref @@
    match int_of_string (arg "step") with
    | exception _ -> 0
    | step_id -> step_id in
  let prev_button_state = button_state () in
  let next_button_state = button_state () in
  let prev_step_button_state = button_state () in
  let next_step_button_state = button_state () in
  let rec load_tutorial tutorial_name step_id () =
    Server_caller.fetch_tutorial tutorial_name >>= fun { Tutorial.steps } ->
    set_arg "tutorial" tutorial_name ;
    set_arg "step" (string_of_int step_id) ;
    let prev, next = prev_and_next tutorial_name in
    begin match prev with
      | None -> disable_button prev_button_state
      | Some _ -> enable_button prev_button_state
    end ;
    begin match next with
      | None -> disable_button next_button_state
      | Some _ -> enable_button next_button_state
    end ;
    let option =
      Tyxml_js.To_dom.of_option (List.assoc tutorial_name options) in
    option##.selected := Js._true ;
    let step = try
        List.nth steps step_id
      with _ -> failwith "unknown step" in
    if step_id = 0 then
      disable_button prev_step_button_state
    else
      enable_button prev_step_button_state ;
    if step_id = List.length steps - 1 then
      disable_button next_step_button_state
    else
      enable_button next_step_button_state ;
    current_tutorial_name := tutorial_name ;
    current_step_id := step_id ;
    Manip.replaceChildren step_title
      (render_rich_text step.step_title) ;
    let items =
      let on_runnable_clicked code =
        Lwt.async @@ fun () ->
        toplevel_launch >>= fun top ->
        if button_group_disabled toplevel_buttons_group then
          Lwt.return ()
        else
          disabling_button_group toplevel_buttons_group
            (fun () ->
               Learnocaml_toplevel.execute_phrase top code >>= fun _ ->
               Lwt.return ()) in
      let rec render_phrases phrases =
        List.map
          (function
            | Paragraph text ->
                Tyxml_js.Html5.p
                  (render_rich_text ~on_runnable_clicked text)
            | Code_block { code ; runnable } ->
                let elt = Tyxml_js.Html.pre [ Tyxml_js.Html.pcdata code ] in
                if runnable then begin
                  Manip.addClass elt "runnable" ;
                  Manip.Ev.onclick elt (fun _ -> on_runnable_clicked code ; true)
                end ;
                elt
            | Enum items ->
                Tyxml_js.Html5.ul
                  (List.map (fun phrases ->
                       Tyxml_js.Html5.li (render_phrases phrases))
                      items))
          phrases in
      render_phrases step.step_contents in
    Manip.replaceChildren step_items_container items ;
    toplevel_launch >>= fun top ->
    Learnocaml_toplevel.scroll top ;
    Lwt.return () in
  begin button
      ~group: toplevel_buttons_group
      ~state: prev_button_state ~container: navigation_div
      ~theme: "black" ~icon: "left" [%i"Prev"] @@ fun () ->
    match prev_and_next !current_tutorial_name with
    | Some prev, _ ->
        load_tutorial prev 0 ()
    | _ -> Lwt.return ()
  end ;
  Manip.appendChild navigation_div selector ;
  disable_with_button_group (Tyxml_js.To_dom.of_select selector)
    toplevel_buttons_group ;
  dom_selector##.onchange :=
    Dom_html.handler (fun _ ->
        let id = Js.to_string (dom_selector##.value) in
        Lwt.async (load_tutorial id 0) ;
        Js._true) ;
  begin button
      ~group: toplevel_buttons_group
      ~state: next_button_state ~container: navigation_div
      ~theme: "black" ~icon: "right" [%i"Next"] @@ fun () ->
    match prev_and_next !current_tutorial_name with
    | _, Some next ->load_tutorial next 0 ()
    | _ -> Lwt.return ()
  end ;
  begin button
      ~group: toplevel_buttons_group
      ~state: prev_step_button_state ~container: step_title_container
      ~theme: "black" ~icon: "left" "" @@ fun () ->
    load_tutorial !current_tutorial_name (!current_step_id - 1) ()
  end ;
  Manip.appendChild step_title_container step_title ;
  begin button
      ~group: toplevel_buttons_group
      ~state: next_step_button_state ~container: step_title_container
      ~theme: "black" ~icon: "right" "" @@ fun () ->
    load_tutorial !current_tutorial_name (!current_step_id + 1) ()
  end ;
  load_tutorial !current_tutorial_name !current_step_id () >>= fun () ->
  begin button
      ~container: buttons_div ~theme: "dark"
      ~group: toplevel_buttons_group ~icon: "cleanup" [%i"Clear"] @@ fun () ->
    toplevel_launch >>= fun top ->
    Learnocaml_toplevel.clear top ;
    Lwt.return ()
  end ;
  begin button
      ~container: buttons_div ~theme: "dark"
      ~icon:"reload" [%i"Reset"] @@ fun () ->
    toplevel_launch >>= fun top ->
    disabling_button_group toplevel_buttons_group (fun () -> Learnocaml_toplevel.reset top)
  end ;
  begin button
      ~container: buttons_div ~theme: "dark"
      ~group: toplevel_buttons_group ~icon: "run" [%i"Eval phrase"] @@ fun () ->
    toplevel_launch >>= fun top ->
    Learnocaml_toplevel.execute top ;
    Lwt.return ()
  end ;
  toplevel_launch >>= fun _ ->
  hide_loading ~id:"learnocaml-main-loading" () ;
  Lwt.return tutorial_div
;;

let toplevel_tab select _ () =
  let content_div = find_component "learnocaml-main-content" in
  let container =
    Tyxml_js.Html5.(div ~a: [ a_class [ "toplevel-pane" ] ]) [] in
  let buttons_div =
    Tyxml_js.Html5.(div ~a: [ a_class [ "buttons" ] ]) [] in
  let div =
    Tyxml_js.Html5.(div ~a: [ a_id "learnocaml-main-toplevel" ])
      [ container ; buttons_div ] in
  show_loading ~id:"learnocaml-main-loading"
    Tyxml_js.Html5.[ ul [ li [ pcdata [%i"Launching OCaml"] ] ] ] ;
  let timeout_prompt =
    Learnocaml_toplevel.make_timeout_popup
      ~on_show: (fun () -> Lwt.async select)
      () in
  let flood_prompt =
    Learnocaml_toplevel.make_flood_popup
      ~on_show: (fun () -> Lwt.async select)
      () in
  let history =
    let storage_key =
      Learnocaml_local_storage.toplevel_history "toplevel" in
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
  let toplevel_buttons_group = button_group () in
  disable_button_group toplevel_buttons_group (* enabled after init *) ;
  Learnocaml_toplevel.create
    ~on_disable_input: (fun _ -> disable_button_group toplevel_buttons_group)
    ~on_enable_input: (fun _ -> enable_button_group toplevel_buttons_group)
    ~history ~timeout_prompt ~flood_prompt
    ~container
    () >>= fun top ->
  Manip.appendChild content_div div ;
  let button = button ~container: buttons_div ~theme: "dark" in
  begin button
      ~group: toplevel_buttons_group ~icon: "cleanup" [%i"Clear"] @@ fun () ->
    Learnocaml_toplevel.clear top ;
    Lwt.return ()
  end ;
  begin button
      ~icon:"reload" [%i"Reset"] @@ fun () ->
    disabling_button_group toplevel_buttons_group (fun () -> Learnocaml_toplevel.reset top)
  end ;
  begin button
      ~group: toplevel_buttons_group ~icon: "run" [%i"Eval phrase"] @@ fun () ->
    Learnocaml_toplevel.execute top ;
    Lwt.return ()
  end ;
  hide_loading ~id:"learnocaml-main-loading" () ;
  Lwt.return div

let teacher_tab token _select _params () =
  let module H = Tyxml_js.Html5 in
  let action_new_token () =
    Server_caller.request_exn (Learnocaml_api.Create_teacher_token token)
    >|= fun new_token ->
    alert ~title:[%i"TEACHER TOKEN"]
      (Printf.sprintf [%if"New teacher token created:\n%s\n\n\
                           write it down."]
         (Token.to_string new_token))
  in
  let action_csv_export () =
    Server_caller.request_exn (Learnocaml_api.Students_csv token)
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
  let students_changes = ref Token.Map.empty in
  let status_map = ref SMap.empty in
  let status_changes = ref SMap.empty in
  let status_current () =
    SMap.merge (fun id old newer -> match newer with None -> old | s -> s)
      !status_map !status_changes;
  in
  let get_status id =
    try SMap.find id !status_changes with Not_found ->
    try SMap.find id !status_map with Not_found ->
      Exercise.Status.default id
  in
  let exercise_changed_status id =
    match SMap.find_opt id !status_changes with
    | None -> false
    | Some ch -> match SMap.find_opt id !status_map with
      | None -> ch <> Exercise.Status.default id
      | Some st -> ch <> st
  in
  let exercise_line_id id = "learnocaml_exercise_"^id in
  let student_line_id token = "learnocaml_student_"^Token.to_string token in
  let assg_line_id id = "assg_line_"^string_of_int id in

  let all_exercises g =
    Exercise.Index.fold_exercises (fun acc id _ -> id :: acc)
      [] g
    |> List.rev
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
                H.th ~a:[H.a_colspan 0; indent_style group_level]
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
              H.td ~a:[indent_style group_level]
                [ H.pcdata meta.Exercise.Meta.title ];
              H.td (List.map H.pcdata meta.Exercise.Meta.focus);
              H.td [stars_div meta.Exercise.Meta.stars];
              H.td (List.map tag_div st.Exercise.Status.tags);
              H.td [
                let cls, text = match st.Exercise.Status.status with
                  | Exercise.Status.Open -> "exo_open", [%i"Open"]
                  | Exercise.Status.Closed -> "exo_closed", [%i"Closed"]
                  | Exercise.Status.Assigned a -> "exo_assigned", [%i"Assigned"]
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
      ] [H.pcdata [%i"Exercises"]]
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
              (Token.Map.fold (fun k _ acc -> k::acc) !students_map []);
            true
          );
      ] [H.pcdata [%i"Students"]]
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
  let fill_students_pane () =
    let table =
      List.rev_map (fun st ->
          let open Student in
          H.tr ~a:[
            H.a_id (student_line_id st.Student.token);
            H.a_class ["student_line"];
            H.a_onclick (fun _ ->
                !toggle_selected_students [st.Student.token];
                true);
          ] [
            H.td [html_token st.token];
            H.td (match st.nickname with Some n -> [H.pcdata n] | _ -> []);
          ])
        (Token.Map.fold (fun _ st acc -> st :: acc) !students_map [])
    in
    Manip.replaceChildren students_list_div [H.table table]
  in

  let assignment_line id =
    let selected = !selected_assignment = Some id in
    let date id assg_id t =
      let date = new%js Js.date_fromTimeValue (t *. 1000.) in
      H.div [
        H.input ~a:([
            H.a_id ("date_"^id);
            H.a_class ["assignment_date"];
            H.a_input_type `Date;
            H.a_value
              (Printf.sprintf "%04d-%02d-%02d"
                 date##getFullYear (date##getMonth + 1) date##getDate);
            H.a_onblur (fun _ -> !assignment_change assg_id; true);
            H.a_onkeydown (fun ev ->
                if ev##.keyCode = 13 then !assignment_change assg_id; true);
            H.a_pattern "[0-9]{4}-[0-9]{2}-[0-9]{2}";
          ] @ if selected then [] else [H.a_readonly ()])
          ();
        H.input ~a:([
            H.a_id ("time_"^id);
            H.a_class ["assignment_date"];
            H.a_input_type `Time;
            H.a_value
              (Printf.sprintf "%02d:%02d"
                 date##getHours date##getMinutes);
            H.a_onblur (fun _ -> !assignment_change assg_id; true);
            H.a_onkeydown (fun ev ->
                if ev##.keyCode = 13 then !assignment_change assg_id; true);
            H.a_pattern "[0-9]{2}:[0-9]{2}";
          ] @ if selected then [] else [H.a_readonly ()])
          ();
      ]
    in
    let hid = assg_line_id id in
    let (assg, tokens, exo_ids) = Hashtbl.find assignments_tbl id in
    let cls =
      let n = gettimeofday () in
      if assg.Exercise.Status.stop < n then ["assg_finished"]
      else if assg.Exercise.Status.start > n then ["assg_notstarted"]
      else ["assg_active"]
    in
    let cls = if selected then "selected"::cls else cls in
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
      H.td [date ("start_"^hid) id assg.Exercise.Status.start];
      H.td [date ("stop_"^hid) id assg.Exercise.Status.stop];
      H.td [H.pcdata (Printf.sprintf [%if"%d exercises"] (SSet.cardinal exo_ids))];
      H.td [H.pcdata (Printf.sprintf [%if"%d students"] (Token.Set.cardinal tokens))];
      H.td ~a:[H.a_onclick (fun _ -> !assignment_remove id; false);
               H.a_class ["remove-cross"]]
        [H.pcdata "\xe2\xa8\x82" (* U+2A02 *)];
      (* todo: add common tags *)
    ]
  in
  let already_assigned_exercises students =
    SSet.filter (fun ex ->
        let Exercise.Status.{status; _} = get_status ex in
        match status with
        | Exercise.Status.Assigned a ->
            Exercise.Status.exists_assignment a (fun tk _ ->
              Token.Set.mem tk students
            )
        | _ -> false)
      (SSet.of_list (all_exercises !exercises_index))
  in
  let already_assigned_students exercises =
    SSet.fold (fun ex acc ->
        let Exercise.Status.{status; _} = get_status ex in
        match status with
        | Exercise.Status.Assigned a ->
            Exercise.Status.fold_over_assignments a (fun tk _ acc ->
              Token.Set.add tk acc
            ) acc
        | _ -> acc)
      exercises
      Token.Set.empty
  in
  let assignments_table () =
    let module AM = Map.Make(struct
        type t = Exercise.Status.assignment
        let compare = compare
      end)
    in
    let module ATM = Map.Make(struct
        type t = Exercise.Status.assignment * Token.Set.t
        let compare (a1, ts1) (a2, ts2) =
          match compare a1 a2 with
          | 0 -> Token.Set.compare ts1 ts2
          | n -> n
      end)
    in
    let atm =
      SMap.fold (fun id st atm ->
          match st.Exercise.Status.status with
          | Exercise.Status.Open | Exercise.Status.Closed -> atm
          | Exercise.Status.Assigned a ->
              let am =
                Exercise.Status.fold_over_assignments a (fun tok assg am ->
                    match AM.find_opt assg am with
                    | None -> AM.add assg (Token.Set.singleton tok) am
                    | Some ts -> AM.add assg (Token.Set.add tok ts) am)
                  (AM.singleton
                     (Exercise.Status.default_assignment a)
                     Token.Set.empty)
              in
              AM.fold (fun assg toks atm ->
                  match ATM.find_opt (assg, toks) atm with
                  | None -> ATM.add (assg, toks) (SSet.singleton id) atm
                  | Some ids -> ATM.add (assg, toks) (SSet.add id ids) atm)
                am atm)
        (status_current ())
        ATM.empty
    in
    let line_n = ref 0 in
    let make_line ?selected assg tokens exo_ids =
      incr line_n;
      let id = !line_n in
      Hashtbl.add assignments_tbl id (assg, tokens, exo_ids);
      assignment_line id
    in
    let new_assg_id = "new_assignment" in
    let table = H.table [] in
    let new_assg_line =
      H.tr ~a:[
        H.a_id new_assg_id;
      ] [
        H.td ~a:[H.a_colspan 0]
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
      let tokens =
        Hashtbl.fold (fun tk () -> Token.Set.add tk)
          selected_students Token.Set.empty
      in
      let exercises =
        Hashtbl.fold (fun id () -> SSet.add id) selected_exercises SSet.empty
      in
      let exercises =
        SSet.diff exercises (already_assigned_exercises tokens)
      in
      let line =
        make_line
          Exercise.Status.{start; stop} tokens exercises
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
       (ATM.fold (fun (assg, tokens) exos acc ->
            make_line assg tokens exos :: acc)
           atm [])) @
    [new_assg_line];
    table
  in
  let open_close_button =
    H.button ~a:[
      H.a_onclick (fun _ ->
          let open Exercise.Status in
          let ids = htbl_keys selected_exercises in
          let fstat =
            if List.exists (fun id -> (get_status id).status = Open) ids
            then function
              | {status = Open} as st -> {st with status = Closed}
              | st -> st
            else function
              | {status = Closed} as st -> {st with status = Open}
              | st -> st
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
    let changes =
      SMap.fold (fun id st acc ->
          if Some st <> SMap.find_opt id !status_map then st :: acc
          else acc)
        !status_changes []
    in
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
  let select_student onoff tk =
    let class_f, tbl_f = match onoff with
      | true -> Manip.addClass, (fun t k -> Hashtbl.replace t k ())
      | false -> Manip.removeClass, Hashtbl.remove
    in
    tbl_f selected_students tk;
    match Manip.by_id (student_line_id tk) with
    | Some elt -> class_f elt "selected"
    | None -> ()
  in
  let set_assignment ?(reopen=false) ?assg ?students ?exos id =
    let (assg0, students0, exos0) = Hashtbl.find assignments_tbl id in
    let dft x0 = function Some x -> x | None -> x0 in
    let assg = dft assg0 assg in
    let students = dft students0 students in
    let exos = dft exos0 exos in
    Hashtbl.replace assignments_tbl id (assg, students, exos);
    (match Manip.by_id (assg_line_id id) with
     | Some l -> Manip.replaceSelf l (assignment_line id)
     | None -> failwith "Assignment line not found");
    let set_assg st tmap default_assignment =
      if reopen then
        Exercise.Status.{st with status = Open}
      else
        let a = Exercise.Status.make_assignments tmap default_assignment in
        Exercise.Status.{st with status = Assigned a}
    in
    let ch =
      SSet.fold (fun ex_id acc ->
          let st = get_status ex_id in
          let open Exercise.Status in
          let tmap0 =
            match st.status with
            | Open | Closed -> Token.Map.empty
            | Assigned a -> token_map_of_assignments a
          in
          let tmap =
            Token.Set.fold (fun tk -> Token.Map.remove tk)
              (Token.Set.diff students0 students) tmap0
          in
          let tmap =
            Token.Set.fold (fun tk -> Token.Map.add tk assg)
              students tmap
          in
          (* The most recent assignment is used for students that will
             register *after* the creation of this homework. *)
          let default_assignment =
            assg
          in
          SMap.add ex_id (set_assg st tmap default_assignment) acc)
        exos
        !status_changes
    in
    let ch =
      SSet.fold (fun ex_id acc ->
          let st = get_status ex_id in
          let open Exercise.Status in
          let tmap0 =
            match st.status with
            | Open | Closed -> Token.Map.empty
            | Assigned a -> token_map_of_assignments a
          in
          let tmap =
            Token.Set.fold (fun tk -> Token.Map.remove tk) students0 tmap0
          in
          let default_assignment =
            assg
          in
          SMap.add ex_id (set_assg st tmap default_assignment) acc)
        (SSet.diff exos0 exos)
        ch
    in
    status_changes := ch;
    fill_exercises_pane ();
    !update_changed_status ();
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
      | Some id -> let _, _, exos = Hashtbl.find assignments_tbl id in exos
      | None -> SSet.empty
    in
    disabled
      (SSet.diff
         (already_assigned_exercises
            (Token.Set.of_list (htbl_keys selected_students)))
         current_assignment)
  in
  let update_disabled_students () =
    let disabled tokens =
      Token.Set.iter (Hashtbl.remove selected_students) tokens;
      Token.Map.iter (fun tk _ ->
          match Manip.by_id (student_line_id tk) with
          | None -> ()
          | Some el ->
              if Token.Set.mem tk tokens then
                (Manip.addClass el "disabled";
                 Manip.removeClass el "selected")
              else Manip.removeClass el "disabled")
        !students_map
    in
    let current_assignment = match !selected_assignment with
      | Some id -> let _, std, _ = Hashtbl.find assignments_tbl id in std
      | None -> Token.Set.empty
    in
    disabled
      (Token.Set.diff
         (already_assigned_students
            (SSet.of_list (htbl_keys selected_exercises)))
         current_assignment)
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
  toggle_selected_students := begin fun ?force tokens ->
    Lwt.async @@ fun () ->
    let tokens, onoff = match force with
      | Some set -> tokens, set
      | None ->
          let tokens =
            List.filter (fun tk ->
                match Manip.by_id (student_line_id tk) with
                | Some elt -> not (Manip.hasClass elt "disabled")
                | None -> false)
              tokens
          in
          tokens, not @@ List.exists (Hashtbl.mem selected_students) tokens
    in
    List.iter (select_student onoff) tokens;
    (match !selected_assignment with
     | None -> ()
     | Some aid ->
         set_assignment aid
           ~students:(Token.Set.of_list (htbl_keys selected_students)));
    if force = None then update_disabled_exercises ();
    Lwt.return_unit
  end;
  toggle_select_assignment := begin fun assg_id ->
    Lwt.async @@ fun () ->
    let select id =
      match Manip.by_id (assg_line_id id) with
      | None -> ()
      | Some line ->
          let (assg, students, exos) = Hashtbl.find assignments_tbl id in
          !toggle_selected_exercises ~force:false
            (all_exercises !exercises_index);
          !toggle_selected_exercises ~force:true (SSet.elements exos);

          !toggle_selected_students ~force:false
            (Token.Map.fold (fun tk _ acc -> tk::acc) !students_map []);
          !toggle_selected_students ~force:true (Token.Set.elements students);

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
          SMap.add id (f (get_status id)) acc)
        !status_changes ids;
    fill_exercises_pane ();
    update_disabled_exercises ();
    !update_changed_status ();
  end;
  student_change := begin fun tk () ->
    ()
  end;
  assignment_change := begin fun assg_id ->
    let (assg0, _, _) = Hashtbl.find assignments_tbl assg_id in
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
      | None -> assg0.Exercise.Status.start
    in
    let stop = match get_date ("stop_"^assg_line_id assg_id) with
      | Some t -> if t < start then start else t
      | None -> assg0.Exercise.Status.stop
    in
    let assg = Exercise.Status.{start; stop} in
    set_assignment assg_id ~assg
  end;
  assignment_remove := begin fun assg_id ->
    Lwt.async @@ fun () ->
    set_assignment
      ~reopen:true assg_id
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
      List.fold_left (fun m ex -> SMap.add ex.Exercise.Status.id ex m)
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

let token_input_field () =
  let id = "learnocaml-save-token-field" in
  let input = find_component id in
  Tyxml_js.To_dom.of_input input

let get_stored_token () =
  Learnocaml_local_storage.(retrieve sync_token)

let sync () =
  let token_input = Js.to_string ((token_input_field ())##.value) in
  let stored_token = get_stored_token () in
  let reset_token_input () =
    (token_input_field ())##.value :=
      Js.string (Token.to_string stored_token);
  in
  if token_input = "" then
    (reset_token_input (); sync stored_token)
  else
  match Token.parse token_input with
  | exception (Failure _) ->
      reset_token_input ();
      Lwt.fail_with "Invalid token entered"
  | token ->
      if token = stored_token then
        sync token
      else (* New token entered, replace current save by remote *)
        Server_caller.request (Learnocaml_api.Fetch_save token) >>= function
        | Ok save ->
            set_state_from_save_file ~token save;
            let nickname_field = find_component "learnocaml-nickname" in
            (Tyxml_js.To_dom.of_input nickname_field)##.value :=
              Js.string save.Save.nickname;
            Lwt.return save
        | Error (`Not_found _) ->
            reset_token_input ();
            Lwt.fail_with "The entered token is unknown"
        | Error e -> Lwt.fail_with (Server_caller.string_of_error e)

let init_token_dialog () =
  let login_overlay = find_component "login-overlay" in
  Manip.SetCss.display login_overlay "block";
  let input_nick = find_component "login-nickname-input" in
  let button_new = find_component "login-new-button" in
  let input_tok = find_component "login-token-input" in
  let button_connect = find_component "login-connect-button" in
  let get_token, got_token = Lwt.task () in
  let create_token () =
    let nickname = match Manip.value input_nick with
      | "" -> None
      | n ->
          Learnocaml_local_storage.(store nickname) n;
          let nickname_field = find_component "learnocaml-nickname" in
          (Tyxml_js.To_dom.of_input nickname_field)##.value := Js.string n;
          Some n
    in
    Server_caller.request_exn (Learnocaml_api.Create_token (None, nickname))
    >>= fun token ->
    Learnocaml_local_storage.(store sync_token) token;
    Lwt.return_some token
  in
  let login_token () =
    let input = input_tok in
    match Token.parse (Manip.value input) with
    | exception (Failure _) ->
        Manip.SetCss.borderColor input "#f44";
        Lwt.return_none
    | token ->
        Server_caller.request (Learnocaml_api.Fetch_save token) >>= function
        | Ok save ->
            set_state_from_save_file ~token save;
            Lwt.return_some token
        | Error (`Not_found _) ->
            alert ~title:[%i"TOKEN NOT FOUND"]
              [%i"The entered token couldn't be recognised."];
            Lwt.return_none
        | Error e ->
            Lwt.fail_with (Server_caller.string_of_error e)
  in
  let handler f t = fun _ ->
    Lwt.async (fun () ->
        f () >|= function
        | Some token -> Lwt.wakeup got_token token
        | None -> ());
    t
  in
  Manip.Ev.onclick button_new (handler create_token false);
  Manip.Ev.onreturn input_nick (handler create_token ());
  Manip.Ev.onclick button_connect (handler login_token false);
  Manip.Ev.onreturn input_tok (handler login_token ());
  get_token >|= fun token ->
  Manip.SetCss.display login_overlay "none";
  token

let init_sync_token button_state =
  catch
    (fun () ->
       begin try
           Lwt.return Learnocaml_local_storage.(retrieve sync_token)
         with Not_found -> init_token_dialog ()
       end >>= fun token ->
       (token_input_field ())##.value :=
         Js.string (Token.to_string token) ;
       enable_button button_state ;
       Lwt.return (Some token))
    (fun _ -> Lwt.return None)

let set_string_translations () =
  let translations = [
    "txt_welcome",
    [%i"Welcome to <emph>LearnOCaml</emph> by OCamlPro."];
    "txt_choose_activity",
    [%i"Choose your activity below."];
    "txt_token_doc",
    [%i"Your storage token on OCamlPro's servers:"];
    "txt_token_share",
    [%i"You can share it between devices."];
    "txt_progression_local",
    [%i"Progression is saved locally in the browser."];
    "txt_save_doc",
    [%i"Save it to a file using the <img src=\"icons/icon_download_white.svg\" \
        class=\"icon\" alt=\"save\"> button above."];
    "txt_restore_doc",
    [%i"Restore it from a file using the <img \
        src=\"icons/icon_upload_white.svg\" class=\"icon\" alt=\"restore\"> \
        button above."];
    "txt_sync_doc",
    [%i"Save online using the <img src=\"icons/icon_sync_white.svg\" \
        class=\"icon\" alt=\"sync\"> button above."];
    "learnocaml-logout",
    [%i"Logout"];
    "txt_login_welcome", [%i"Welcome to Learn OCaml"];
    "txt_first_connection", [%i"First connection"];
    "txt_first_connection_dialog", [%i"Choose a nickname"];
    "txt_login_new", [%i"Create new token"];
    "txt_returning", [%i"Returning user"];
    "txt_returning_dialog", [%i"Enter your token"];
    "txt_login_returning",  [%i"Connect"];
  ] in
  List.iter
    (fun (id, text) ->
       Manip.setInnerHtml (find_component id) text)
    translations;
  let placeholder_translations = [
    "learnocaml-nickname", [%i"Nickname"];
    "login-nickname-input", [%i"Nickname"];
  ] in
  List.iter
    (fun (id, text) ->
       (Tyxml_js.To_dom.of_input (find_component id))##.placeholder :=
         Js.string text)
    placeholder_translations


class type learnocaml_config = object
  method enableTryocaml: bool Js.prop
  method enableLessons: bool Js.prop
  method enableExercises: bool Js.prop
  method enableToplevel: bool Js.prop
end

let config : learnocaml_config Js.t = Js.Unsafe.js_expr "learnocaml_config"

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
  set_string_translations ();
  Manip.setInnerText (find_component "learnocaml-version")
    ("v."^Learnocaml_api.version);
  Learnocaml_local_storage.init () ;
  let sync_button_state = button_state () in
  disable_button sync_button_state ;
  let menu = find_component "learnocaml-main-panel" in
  let menu_hidden = ref true in
  let content_div = find_component "learnocaml-main-content" in
  let no_tab_selected () =
    Manip.removeChildren content_div ;
    let content_div = find_component "learnocaml-main-content" in
    let div =
      Tyxml_js.Html5.(div ~a: [ a_class [ "placeholder" ] ])
        Tyxml_js.Html5.[ div [ pcdata [%i"Choose an activity."] ]] in
    Manip.removeChildren content_div ;
    Manip.appendChild content_div div ;
    delete_arg "activity"
  in
  let init_tabs token =
    let tabs =
      (if config##.enableTryocaml
       then [ "tryocaml", ([%i"Try OCaml"], tryocaml_tab) ] else []) @
      (if config##.enableLessons
       then [ "lessons", ([%i"Lessons"], lessons_tab) ] else []) @
      (match token, config##.enableExercises with
       | Some token, true -> [ "exercises", ([%i"Exercises"], exercises_tab token) ]
       | _ -> []) @
      (if config##.enableToplevel
       then [ "toplevel", ([%i"Toplevel"], toplevel_tab) ] else []) @
      (match token with
       | Some t when Token.is_teacher t ->
           [ "teacher", ([%i"Teach"], teacher_tab t) ]
       | _ -> [])
    in
    let container = find_component "learnocaml-tab-buttons-container" in
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
           Manip.removeChildren content_div ;
           List.iter (fun (n, _) -> delete_arg n) !(!current_args) ;
           begin match !div with
             | Some div ->
                 List.iter (fun (n, v) -> set_arg n v) !args ;
                 Manip.appendChild content_div div ;
                 Lwt.return div
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
                 Lwt.return fresh
           end >>= fun div ->
           set_arg "activity" id ;
           Manip.addClass btn "active" ;
           menu_hidden := true ;
           Manip.addClass menu "hidden" ;
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
  let sync_buttons = find_component "learnocaml-sync-buttons" in
  Manip.removeChildren sync_buttons ;
  begin button
      ~container: sync_buttons
      ~theme:"white" ~icon: "download" [%i"Save"] @@ fun () ->
    let name = "learnocaml-main.json" in
    let contents =
      let json =
        Json_repr_browser.Json_encoding.construct
          Save.enc
          (get_state_as_save_file ()) in
      Js._JSON##(stringify json) in
    Learnocaml_common.fake_download ~name ~contents ;
    Lwt.return ()
  end ;
  begin button
      ~container: sync_buttons
      ~theme:"white" ~icon: "upload" [%i"Restore"] @@ fun () ->
    Learnocaml_common.fake_upload () >>= fun (_, contents) ->
    let save_file =
      Json_repr_browser.Json_encoding.destruct
        Save.enc
        (Js._JSON##(parse contents)) in
    let token = try Some (get_stored_token ()) with Not_found -> None in
    set_state_from_save_file ?token save_file ;
    let nickname_field = find_component "learnocaml-nickname" in
    (Tyxml_js.To_dom.of_input nickname_field)##.value :=
      Js.string save_file.Save.nickname;
    let _tabs = init_tabs token in
    no_tab_selected ();
    Lwt.return ()
  end ;
  begin button
      ~container: sync_buttons
      ~state: sync_button_state
      ~theme:"white" ~icon: "sync" [%i"Sync"] @@ fun () ->
    catch_with_alert @@ fun () ->
    sync () >>= fun _ -> Lwt.return_unit;
  end ;
  begin button
      ~container: (find_component "learnocaml-main-toolbar")
      ~theme:"white" ~icon: "menu" [%i"Menu"] @@ fun () ->
    menu_hidden := not !menu_hidden ;
    if !menu_hidden then
      Manip.addClass menu "hidden"
    else
      Manip.removeClass menu "hidden" ;
    Lwt.return ()
  end ;
  begin
    let logout () =
      Lwt.catch
        (fun () -> sync () >>= fun _ -> Lwt.return_unit)
        (fun _ -> Lwt.return_unit) >>= fun () ->
      Learnocaml_local_storage.clear ();
      no_tab_selected ();
      (token_input_field ())##.value := Js.string "";
      let nickname_field = find_component "learnocaml-nickname" in
      (Tyxml_js.To_dom.of_input nickname_field)##.value := Js.string "";
      reload ();
      Lwt.return_unit
    in
    Manip.Ev.onclick (find_component "learnocaml-logout")
      (function _ -> Lwt.async logout; false)
  end;
  begin
    let nickname_field = find_component "learnocaml-nickname" in
    (try
       let nickname = Learnocaml_local_storage.(retrieve nickname) in
       (Tyxml_js.To_dom.of_input nickname_field)##.value := Js.string nickname;
     with Not_found -> ());
    let save_nickname () =
      Learnocaml_local_storage.(store nickname) @@
      Js.to_string (Tyxml_js.To_dom.of_input nickname_field)##.value
    in
    Manip.Ev.onreturn nickname_field (fun _ -> save_nickname ());
    Manip.Ev.onblur nickname_field (fun _ -> save_nickname (); true);
  end ;
  begin
    let token_field = find_component "learnocaml-save-token-field" in
    let input = Tyxml_js.To_dom.of_input token_field in
    Manip.Ev.onfocus token_field (fun _ ->
        input##.value := Js.string "";
        true
      );
    let update () =
      let t = get_stored_token () in
      let v = Js.to_string (input##.value) in
      if v = "" then
        Lwt.return (input##.value := Js.string (Token.to_string t))
      else if v = Token.to_string t then
        Lwt.return_unit
      else
        catch_with_alert @@ fun () ->
        sync () >|= fun _ ->
        get_stored_token () |> fun token ->
        no_tab_selected () |> fun () ->
        init_tabs (Some token) |> fun _ -> ()
    in
    Manip.Ev.onblur token_field (fun _ -> Lwt.async update; true);
    Manip.Ev.onreturn token_field (fun _ -> Lwt.async update);
  end ;
  init_sync_token sync_button_state >|= init_tabs >>= fun tabs ->
  try
    let activity = arg "activity" in
    let (_, select) = List.assoc activity tabs in
    select ()
  with Not_found ->
    no_tab_selected ();
    Lwt.return ()
;;
