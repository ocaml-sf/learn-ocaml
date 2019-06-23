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
open Learnocaml_index
open Learnocaml_common
open Editor_lib
module StringMap = Map.Make (String)


let exercises_tab _ _ () =
  show_loading ~id:"learnocaml-main-loading"
    Tyxml_js.Html5.[ ul [ li [ pcdata [%i"Loading exercises"] ] ] ] ;
  Lwt_js.sleep 0.5 >>= fun () ->
  Server_caller.fetch_exercise_index () >>= fun index ->
  let content_div = find_component "learnocaml-main-content" in
  let format_exercise_list all_exercise_states =
    let rec format_contents lvl acc contents =
      let open Tyxml_js.Html5 in
      match contents with
      | Learnocaml_exercises exercises ->
          StringMap.fold
            (fun exercise_id { exercise_kind ;
                               exercise_title ;
                               exercise_short_description ;
                               exercise_stars } acc ->
              let pct_init =
                match StringMap.find exercise_id all_exercise_states with
                | exception Not_found -> None
                | { Learnocaml_exercise_state.grade } -> grade in
              let pct_signal, pct_signal_set = React.S.create pct_init in
              Learnocaml_local_storage.(listener
                                          (exercise_state exercise_id)) :=
                Some (function
                    | Some { Learnocaml_exercise_state.grade } ->
                       pct_signal_set grade
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
              a ~a:[ a_href ("exercise.html#id=" ^ exercise_id ^
                               "&action=open") ;
                     a_class [ "exercise" ] ] [
                div ~a:[ a_class [ "descr" ] ] [
                  h1 [ pcdata exercise_title ] ;
                  p [ match exercise_short_description with
                      | None -> pcdata [%i"No description available."]
                      | Some text -> pcdata text ] ;
                ] ;
                div ~a:[ Tyxml_js.R.Html5.a_class status_classes_signal ] [
                  div ~a:[ a_class [ "stars" ] ] [
                    let num = 5 * int_of_float (exercise_stars *. 2.) in
                    let num = max (min num 40) 0 in
                    let alt = Format.asprintf [%if"difficulty: %d / 40"] num in
                    let src = Format.asprintf "icons/stars_%02d.svg" num in
                    img ~alt ~src ()
                  ] ;
                  div ~a:[ a_class [ "length" ] ] [
                    match exercise_kind with
                    | Project -> pcdata [%i"project"]
                    | Problem -> pcdata [%i"problem"]
                    | Learnocaml_exercise -> pcdata [%i"exercise"] ] ;
                  div ~a:[ a_class [ "score" ] ] [
                    Tyxml_js.R.Html5.pcdata pct_text_signal
                  ]
                ] ] ::
              acc)
            exercises acc
      | Groups groups ->
         let h = match lvl with 1 -> h1 | 2 -> h2 | _ -> h3 in
          StringMap.fold
            (fun _ { group_title ; group_contents } acc ->
               format_contents (succ lvl)
                 (h ~a:[ a_class [ "pack" ] ] [ pcdata group_title ] :: acc)
                 group_contents)
            groups acc in
    List.rev (format_contents 1 [] index) in
  let list_div =
    Tyxml_js.Html5.(div ~a:
                      [ Tyxml_js.Html5.a_id "learnocaml-main-exercise-list" ])
      (format_exercise_list
         Learnocaml_local_storage.(retrieve all_exercise_states)) in
  Manip.appendChild content_div list_div ;
  hide_loading ~id:"learnocaml-main-loading" () ;
 Lwt.return list_div;;


let init () =
  Learnocaml_local_storage.(store (index_state "index"))
  {Learnocaml_exercise_state.exos = StringMap.empty; mtime = gettimeofday ()};;

let delete_button_handler exercise_id =
  (fun _ ->
     begin
       let messages = Tyxml_js.Html5.ul [] in
       let aborted, abort_message =
         let t, u = Lwt.task () in
         let btn_no = Tyxml_js.Html5.(button [ pcdata [%i"No"] ]) in
         Manip.Ev.onclick btn_no ( fun _ ->
             hide_loading ~id:"learnocaml-main-loading" () ; true) ;
         let btn_yes = Tyxml_js.Html5.(button [ pcdata [%i"Yes"] ]) in
         Manip.Ev.onclick btn_yes (fun _ ->
             remove_exo exercise_id;
             Dom_html.window##.location##reload ; true) ;
         let div =
           Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                             [ pcdata [%i"Are you sure you want \
                                          to delete this exercise?\n"] ;
                               btn_yes ;
                               pcdata " " ;
                               btn_no ]) in
         Manip.SetCss.opacity div (Some "0") ;
         t, div in
       Manip.replaceChildren messages
         Tyxml_js.Html5.[ li [ pcdata "" ] ] ;
       show_loading ~id:"learnocaml-main-loading" [ abort_message ] ;
       Manip.SetCss.opacity abort_message (Some "1") ;
     end ;
  true) ;;

let rec editor_tab _ _ () =
  Learnocaml_local_storage.init ();
  let pct_init = None in
    let pct_signal, pct_signal_set = React.S.create pct_init in
              Learnocaml_local_storage.(listener (index_state "index")) :=
                Some (fun _-> pct_signal_set None) ;
  let () =
    match Learnocaml_local_storage.(retrieve (index_state "index")) with
    | exception Not_found -> init ()
    | _ -> () in
    Server_caller.fetch_editor_index () >>= fun index ->
    show_loading ~id:"learnocaml-main-loading"
      Tyxml_js.Html5.[ ul [ li [ pcdata [%i"Loading editor"]]]] ;
    Lwt_js.sleep 0.5 >>= fun () ->
    let content_div = find_component "learnocaml-main-content" in
    let format_exercise_list all_exercise_states =
      let rec format_contents lvl acc contents =
        let open Tyxml_js.Html5 in
        match contents with
        | Learnocaml_exercises exercises ->
           StringMap.fold
             (fun exercise_id { exercise_kind ;
                                exercise_title ;
                                exercise_short_description ;
                                exercise_stars } acc ->
              let pct_init =None in
              let pct_signal, pct_signal_set = React.S.create pct_init in
              Learnocaml_local_storage.(listener (editor_state exercise_id)) :=
                Some (fun _-> pct_signal_set None) ;
              let status_classes_signal =
                React.S.map
                  (function
                   | None -> [ "stats" ]
                   | Some 0 -> [ "stats" ; "failure" ]
                   | Some pct when pct >= 100 -> [ "stats" ; "success" ]
                   | Some _ -> [ "stats" ; "partial" ])
                  pct_signal in
              div ~a:[a_id "toolbar"; a_class ["button"]] [
                  (div ~a:[a_id "button_delete"] [
                       let button = button ~a:[a_id exercise_id]
                                      [img ~src:"icons/icon_cleanup_dark.svg"
                                         ~alt:"" () ; pcdata "" ] in
                       Manip.Ev.onclick button
                         (delete_button_handler exercise_id) ;button
                  ] );
                  (div ~a:[a_id "button_download"] [
                       let button = button ~a:[a_id exercise_id]
                                      [img ~src:"icons/icon_download_dark.svg"
                                         ~alt:"" () ; pcdata "" ] in
                       Manip.Ev.onclick button
                         (fun _ ->
                           let name = exercise_id ^ ".json" in
                           let content =
                             Learnocaml_local_storage.(retrieve
                             (editor_state exercise_id)) in
                           let json =
                       Json_repr_browser.Json_encoding.construct
                         Learnocaml_exercise_state.editor_state_enc
                         content in
                           let contents =
                             (Js._JSON##stringify json) in
                           Learnocaml_common.fake_download ~name ~contents;
                           true) ;button
                ] )] ::
                a ~a:[ a_href ("editor.html#id="^exercise_id^"&action=open") ;
                     a_class [ "exercise" ] ] [
                    div ~a:[ a_class [ "descr" ] ] [
                        h1 [ pcdata exercise_title ] ;
                        p [ match exercise_short_description with
                            | None -> pcdata [%i"No description available."]
                            | Some text -> pcdata text ] ;
                      ] ;
                    div ~a:[ Tyxml_js.R.Html5.a_class status_classes_signal ] [
                        div ~a:[ a_class [ "stars" ] ] [
                            let num = 5 * int_of_float (exercise_stars *. 2.) in
                            let num = max (min num 40) 0 in
                            let alt =
                              Format.asprintf "difficulty: %d / 40" num in
                            let src =
                              Format.asprintf "icons/stars_%02d.svg" num in
                            img ~alt ~src ()
                          ] ;
                        div ~a:[ a_class [ "length" ] ] [
                            match exercise_kind with
                            | Project -> pcdata "editor project"
                            | Problem -> pcdata "editor problem"
                            | Learnocaml_exercise ->
                               pcdata "editor exercise" ] ;
                      ];
                  ] ::
                  acc)
             exercises acc
        | Groups groups ->
           let h = match lvl with 1 -> h1 | 2 -> h2 | _ -> h3 in
           StringMap.fold
             (fun _ { group_title ; group_contents } acc ->
               format_contents (succ lvl)
                 (h ~a:[ a_class [ "pack" ] ] [ pcdata group_title ] :: acc)
                 group_contents)
             groups acc in
      let open Tyxml_js.Html5 in
      let restore_bar = a ~a:[ a_onclick (fun _ ->
         let _ = begin
             Learnocaml_common.fake_upload () >>= fun (_, contents) ->
             let open Learnocaml_exercise_state in
             let save_file =
               Json_repr_browser.Json_encoding.destruct
                 Learnocaml_exercise_state.editor_state_enc
                 (Js._JSON##(parse contents)) in
             let messages = Tyxml_js.Html5.ul [] in
             if idUnique save_file.metadata.id &&
                  titleUnique save_file.metadata.titre then
               begin
                 Learnocaml_local_storage.(store
                 (editor_state save_file.metadata.id) save_file);
                store_in_index save_file.metadata;
                Dom_html.window##.location##reload;
                Lwt.return_unit
               end
             else
               begin
                 let aborted, abort_message =
                   let t, u = Lwt.task () in
                   let btn_ok = Tyxml_js.Html5.(button [ pcdata [%i"OK"] ]) in
                   Manip.Ev.onclick btn_ok (fun _ -> hide_loading
                                            ~id:"learnocaml-main-loading" () ;
                                            true) ;

                   let div =
                     Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                                       [ pcdata [%i"Identifier and/or title \
                                                    not unique\n"] ;
                                         btn_ok
                     ]) in
                   Manip.SetCss.opacity div (Some "0") ;
                   t, div in
                 Manip.replaceChildren messages
                   Tyxml_js.Html5.[ li [ pcdata "" ] ] ;
                 show_loading ~id:"learnocaml-main-loading" [ abort_message ] ;
                 Manip.SetCss.opacity abort_message (Some "1");
                 Lwt.return_unit
               end;
             Lwt.return ();
           end in ();
            true); a_class [ "exercise"] ]
                     [ div ~a:[ a_class [ "descr" ] ] [
                           h1 [ pcdata [%i"Import an exercise"] ];
                           p [pcdata [%i"Import a new exercise \
                                         from a json file"]]]] in
      List.rev (format_contents 1
                  [a ~a:[ a_href "new_exercise.html#&action=open";
                                          a_class [ "exercise" ] ] [
                                       div ~a:[ a_class [ "descr" ] ] [
                                           h1 [ pcdata [%i"New exercise"] ];
                                           p [pcdata [%i"Create \
                                                         a new exercise"]]]];
                                   restore_bar] index) in
    let list_div =
      Tyxml_js.Html5.(div ~a:
                        [ Tyxml_js.Html5.a_id "learnocaml-main-exercise-list" ])
        (format_exercise_list
           Learnocaml_local_storage.(retrieve all_exercise_states)) in
    Manip.appendChild content_div list_div ;
    hide_loading ~id:"learnocaml-main-loading" () ;
    Lwt.return list_div;;

let lessons_tab select (arg, set_arg, delete_arg) () =
  let open Learnocaml_lesson in
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
      | (one, _) :: (two, _) :: (three, _) :: _ when id = two ->
         Some one, Some three
      |  _ :: rest -> loop rest
    in loop index in
  let selector =
    Tyxml_js.Html5.(select (snd (List.split options))) in
  let prev_button_state = button_state () in
  let next_button_state = button_state () in
  let load_lesson ~loading () =
    let selector = Tyxml_js.To_dom.of_select selector in
    let id = Js.to_string selector##.value in
    Server_caller.fetch_lesson id >>= fun { lesson_steps } ->
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
      (fun { step_title ; step_phrases } ->
         Learnocaml_toplevel.print_html top ("<h3>" ^ step_title ^ "</h3>") ;
         let do_phrase = function
           | Text text ->
               Learnocaml_toplevel.print_html top text ;
               Lwt.return ()
           | Code code ->
               Learnocaml_toplevel.execute_phrase top code >>= fun _ ->
               Lwt.return () in
         Lwt_list.iter_s do_phrase step_phrases >>= fun () ->
         Lwt.return ()
      )
      lesson_steps >>= fun () ->
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
    Dom_html.handler
      (fun _ -> Lwt.async (load_lesson ~loading: true) ; Js._true) ;
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
        | exception Not_found ->
           (match index with
            | [] -> raise Not_found
            | (id, _) :: _ -> id) in
      let option = Tyxml_js.To_dom.of_option (List.assoc id options) in
      option##.selected := Js._true ;
      load_lesson ~loading: false ()
    with Not_found -> failwith "lesson not found"
  end >>= fun () ->
  hide_loading ~id:"learnocaml-main-loading" () ;
  Lwt.return lesson_div ;;


let tryocaml_tab select (arg, set_arg, delete_arg) () =
  let open Learnocaml_tutorial in
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
    List.flatten @@ StringMap.fold
      (fun _ { series_tutorials } acc ->
         series_tutorials :: acc)
      index [] in
  let options =
    List.map
      (fun { tutorial_name; tutorial_title } ->
         tutorial_name,
         Tyxml_js.Html5.
           (option ~a: [ a_value tutorial_name ]
              (pcdata (extract_text_from_rich_text tutorial_title))))
      index in
  let selector =
    Tyxml_js.Html5.(select (snd (List.split options))) in
  let dom_selector =
    Tyxml_js.To_dom.of_select selector in
  let prev_and_next id =
    let rec loop = function
      | [] -> assert false
      | [ _ ] (* assumes single id *) -> None, None
      | { tutorial_name = one } ::
        { tutorial_name = two } :: _ when id = one -> None, Some two
      | { tutorial_name = one } ::
        { tutorial_name = two } :: [] when id = two -> Some one, None
      | { tutorial_name = one } ::
        { tutorial_name = two } ::
        { tutorial_name = three } :: _ when id = two -> Some one, Some three
      |  _ :: rest -> loop rest
    in loop index in
  let current_tutorial_name = ref @@
    match arg "tutorial" with
    | exception Not_found -> (List.hd index).tutorial_name
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
    Server_caller.fetch_tutorial tutorial_name >>= fun { tutorial_steps } ->
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
        List.nth tutorial_steps step_id
      with _ -> failwith "unknown step" in
    if step_id = 0 then
      disable_button prev_step_button_state
    else
      enable_button prev_step_button_state ;
    if step_id = List.length tutorial_steps - 1 then
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
                  Manip.Ev.onclick elt
                    (fun _ -> on_runnable_clicked code ; true)
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
    disabling_button_group toplevel_buttons_group
      (fun () -> Learnocaml_toplevel.reset top)
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
  Lwt.return tutorial_div;;

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
          ~icon:"reload" [%i"Reset"] @@
          fun () -> disabling_button_group toplevel_buttons_group
                      (fun () -> Learnocaml_toplevel.reset top)
  end ;
  begin button
      ~group: toplevel_buttons_group ~icon: "run" [%i"Eval phrase"] @@ fun () ->
    Learnocaml_toplevel.execute top ;
    Lwt.return ()
  end ;
  hide_loading ~id:"learnocaml-main-loading" () ;
  Lwt.return div;;

let token_format =
  Json_encoding.(obj1 (req "token" string))

let init_sync_token button_state =
  catch
    (fun () ->
       let id = "learnocaml-save-token-field" in
       let input = find_component id in
       let input = Tyxml_js.To_dom.of_input input in
       begin try
           Lwt.return Learnocaml_local_storage.(retrieve sync_token)
         with Not_found ->
           Lwt_request.get ~headers: [] ~url: "/sync/gimme"
             ~args: [] >>= fun token ->
           let token = Js.string token in
           let json = Js._JSON##(parse token) in
           let token =
             Json_repr_browser.Json_encoding.destruct token_format json in
           Learnocaml_local_storage.(store sync_token) token ;
           Lwt.return token
       end >>= fun token ->
       input##.value := Js.string token ;
       enable_button button_state ;
       Lwt.return ())
    (fun _ -> Lwt.return ())

let set_state_from_save_file {
    Learnocaml_sync.all_index_states;
    all_editor_states;
    all_exercise_states ;
    all_toplevel_histories ;
    all_exercise_toplevel_histories
  } =
  Learnocaml_local_storage.(store all_index_states)
    all_index_states ;
  Learnocaml_local_storage.(store all_editor_states)
    all_editor_states ;
  Learnocaml_local_storage.(store all_exercise_states)
    all_exercise_states ;
  Learnocaml_local_storage.(store all_toplevel_histories)
    all_toplevel_histories ;
  Learnocaml_local_storage.(store all_exercise_toplevel_histories)
    all_exercise_toplevel_histories

let get_state_as_save_file () =
  {Learnocaml_sync.all_index_states =
     Learnocaml_local_storage.(retrieve all_index_states);
    Learnocaml_sync.all_editor_states =
      Learnocaml_local_storage.(retrieve all_editor_states) ;

    Learnocaml_sync.all_exercise_states =
      Learnocaml_local_storage.(retrieve all_exercise_states) ;
    all_toplevel_histories =
      Learnocaml_local_storage.(retrieve all_toplevel_histories) ;
    all_exercise_toplevel_histories =
      Learnocaml_local_storage.(retrieve all_exercise_toplevel_histories) }

let sync () =
  let token =
    let id = "learnocaml-save-token-field" in
    let input = find_component id in
    let input = Tyxml_js.To_dom.of_input input in
    Js.to_string input ##. value in
  let req = Server_caller.fetch_save_file ~token in
  let local_save_file = get_state_as_save_file () in
  req >>= fun server_save_file ->
  Learnocaml_local_storage.(store sync_token) token ;
  let save_file = Learnocaml_sync.sync local_save_file server_save_file in
  set_state_from_save_file save_file ;
  Server_caller.upload_save_file ~token save_file

let () =
  Lwt.async_exception_hook := begin function
    | Failure message -> fatal message
    | Server_caller.Cannot_fetch message -> fatal message
    | exn -> fatal (Printexc.to_string exn)
  end ;
  (match Js_utils.get_lang() with
   | Some l -> Ocplib_i18n.set_lang l
   | None -> ());
  Lwt.async @@ fun () ->
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
  Translate.set_string_translations translations;
  Learnocaml_local_storage.init () ;
  let sync_button_state = button_state () in
  disable_button sync_button_state ;
  Lwt.async @@ (fun () -> init_sync_token sync_button_state) ;
  let sync_buttons = find_component "learnocaml-sync-buttons" in
  Manip.removeChildren sync_buttons ;
  begin button
      ~container: sync_buttons
      ~theme:"white" ~icon: "download" [%i"Save"] @@ fun () ->
    let name = "learnocaml-main.json" in
    let contents =
      let json =
        Json_repr_browser.Json_encoding.construct
          Learnocaml_sync.save_file_enc
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
        Learnocaml_sync.save_file_enc
        (Js._JSON##(parse contents)) in
    set_state_from_save_file save_file ;
    Lwt.return ()
  end ;
  begin button
      ~container: sync_buttons
      ~state: sync_button_state
      ~theme:"white" ~icon: "sync" [%i"Sync"] @@ fun () ->
    sync ()
  end ;
  let menu_hidden = ref true in
  let menu = find_component "learnocaml-main-panel" in
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

  let tabs =
    [ "tryocaml", ([%i"Try OCaml"], tryocaml_tab) ;
      "lessons", ([%i"Lessons"], lessons_tab) ;
      "exercises", ([%i"Exercises"], exercises_tab) ;
      "toplevel", ([%i"Toplevel"], toplevel_tab) ;
      "editor", ([%i"Editor"], editor_tab)] in
  let tabs =
    let container = find_component "learnocaml-tab-buttons-container" in
    let content_div = find_component "learnocaml-main-content" in
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
      tabs in
  try
    let activity = arg "activity" in
    let (_, select) =
      try List.assoc activity tabs with Not_found ->
        failwith "Bad value for argument activity" in
    select ()
  with Not_found ->
    let content_div = find_component "learnocaml-main-content" in
    let div =
      Tyxml_js.Html5.(div ~a: [ a_class [ "placeholder" ] ])
        Tyxml_js.Html5.[ div [ pcdata [%i"Choose an activity."] ]] in
    Manip.appendChild content_div div ;
    Lwt.return () ;;
