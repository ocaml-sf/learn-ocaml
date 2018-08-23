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

let exercises_tab token _ _ () =
  show_loading ~id:"learnocaml-main-loading"
    Tyxml_js.Html5.[ ul [ li [ pcdata [%i"Loading exercises"] ] ] ] ;
  Lwt_js.sleep 0.5 >>= fun () ->
  Server_caller.request_exn (Learnocaml_api.Exercise_index token) >>= fun index ->
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
                div ~a:[ Tyxml_js.R.Html5.a_class status_classes_signal ] [
                  div ~a:[ a_class [ "stars" ] ] [
                    let num = 5 * int_of_float (stars *. 2.) in
                    let num = max (min num 40) 0 in
                    let alt = Format.asprintf [%if"difficulty: %d / 40"] num in
                    let src = Format.asprintf "icons/stars_%02d.svg" num in
                    img ~alt ~src ()
                  ] ;
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
    Tyxml_js.Html5.(div ~a: [ Tyxml_js.Html5.a_id "learnocaml-main-exercise-list" ])
      (format_exercise_list Learnocaml_local_storage.(retrieve all_exercise_states)) in
  Manip.appendChild content_div list_div ;
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
  let actions_div =
    H.div ~a: [ H.a_class [ "teacher-actions" ] ] [
      H.h4 [ H.pcdata "Actions" ];
      H.ul [
        H.li ~a: [ H.a_onclick (fun _ -> Lwt.async action_new_token; true) ]
          [ H.pcdata [%i"Create new teacher token"] ];
        H.li ~a: [ H.a_onclick (fun _ -> Lwt.async action_csv_export; true) ]
          [ H.pcdata [%i"Download student data as CSV"] ];
      ]
    ]
  in
  let indent_style lvl =
    H.a_style (Printf.sprintf "text-align: left; padding-left: %dem;" lvl)
  in
  let rec mk_table group_level acc = function
    | Exercise.Index.Groups groups_list ->
        List.fold_left (fun acc (id, g) ->
            let acc =
              H.tr ~a:[H.a_id ("exercise_group_"^id)] [
                H.th ~a:[H.a_colspan 0; indent_style group_level]
                  [H.pcdata g.Exercise.Index.title];
              ] :: acc
            in
            mk_table (group_level + 1) acc g.Exercise.Index.contents)
          acc groups_list
    | Exercise.Index.Exercises exlist ->
        List.fold_left (fun acc (id, meta) ->
            match meta with None -> acc | Some meta ->
            H.tr ~a:[
              H.a_id ("learnocaml_exercise_"^id);
            ] [
              H.td ~a:[indent_style group_level]
                [ H.pcdata meta.Exercise.Meta.title ];
              H.td (List.map H.pcdata meta.Exercise.Meta.focus);
              H.td [H.pcdata (string_of_float meta.Exercise.Meta.stars)];
            ] :: acc)
          acc exlist
  in
  let exercises_list_div =
    H.div ~a:[H.a_id "exercises_list"] [H.pcdata [%i"Loading..."]]
  in
  let exercises_div =
    H.div ~a:[H.a_id "exercises_pane"] [
      H.div ~a:[H.a_id "exercises_filter_box"] [
        H.pcdata "filtering tools here"
      ];
      exercises_list_div;
    ]
  in
  let students_list_div =
    H.div ~a:[H.a_id "students_list"] [H.pcdata [%i"Loading..."]];
  in
  let students_div =
    H.div ~a:[H.a_id "students_pane"] [
      H.div ~a:[H.a_id "students_filter_box"] [
        H.pcdata "filtering tools here"
      ];
      students_list_div;
    ]
  in
  let div =
    H.div ~a: [H.a_id "learnocaml-main-teacher"] [
      exercises_div;
      students_div;
      actions_div;
    ]
  in
  let fill_exercises_pane =
    Server_caller.request_exn (Learnocaml_api.Exercise_index token)
    >>= fun exercises ->
    let table = List.rev (mk_table 0 [] exercises) in
    Manip.replaceChildren exercises_list_div [H.table table];
    Lwt.return_unit
  in
  let fill_students_pane =
    Server_caller.request_exn (Learnocaml_api.Students_list token)
    >>= fun students ->
    let table =
      List.map (fun st ->
          let open Student in
          H.tr [
            H.td [H.pcdata (Token.to_string st.token)];
            H.td (match st.nickname with Some n -> [H.pcdata n] | _ -> []);
          ])
        students
    in
    Manip.replaceChildren students_list_div [H.table table];
    Lwt.return_unit
  in
  let content_div = find_component "learnocaml-main-content" in
  Manip.appendChild content_div div;
  Lwt.join [fill_exercises_pane; fill_students_pane] >>= fun () ->
  Lwt.return div

let token_input_field () =
  let id = "learnocaml-save-token-field" in
  let input = find_component id in
  Tyxml_js.To_dom.of_input input

let init_sync_token button_state =
  catch
    (fun () ->
       begin try
           Lwt.return Learnocaml_local_storage.(retrieve sync_token)
         with Not_found ->
           Server_caller.request_exn (Learnocaml_api.Create_token None)
           >>= fun token ->
           Learnocaml_local_storage.(store sync_token) token ;
           Lwt.return token
       end >>= fun token ->
       (token_input_field ())##.value :=
         Js.string (Token.to_string token) ;
       enable_button button_state ;
       Lwt.return (Some token))
    (fun _ -> Lwt.return None)

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

let set_string_translations () =
  let translations = [
    "txt_welcome",
    [%i"Welcome to <emph>LearnOCaml</emph> by OCamlPro."];
    "txt_construction",
    [%i"This App is still under construction."];
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
  ] in
  List.iter
    (fun (id, text) ->
       Manip.setInnerHtml (find_component id) text)
    translations

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
