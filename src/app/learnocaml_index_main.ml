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
open Learnocaml_config

module H = Tyxml_js.Html5

module El = struct
  (** Defines the static elements that should be present from index.html *)

  let id s = s, find_component s

  let loading_id, loading = id "learnocaml-main-loading"

  let content_id, content = id "learnocaml-main-content"

  let toolbar_id, toolbar = id "learnocaml-main-toolbar"

  let menu_id, menu = id "learnocaml-main-panel"

  let nickname_field_id, nickname_field = id "learnocaml-nickname"

  let version_id, version = id "learnocaml-version"

  let tab_buttons_container_id, tab_buttons_container =
    id "learnocaml-tab-buttons-container"

  let op_buttons_container_id, op_buttons_container =
    id "learnocaml-op-buttons-container"

  let sync_buttons_id, sync_buttons = id "learnocaml-sync-buttons"

  let show_panel_id, show_panel = id "learnocaml-show-panel"

  let hide_panel_id, hide_panel = id "learnocaml-hide-panel"

  module Login_overlay = struct
    let login_overlay_id, login_overlay = id "login-overlay"
    let login_new_token_id, login_new_token = id "login-new-token"
    let login_new_id, login_new = id "login-new"
    let login_returning_id, login_returning = id "login-returning"

    let token_nickname_id, token_nickname = id "token-nickname-input"
    let token_secret_id, token_secret = id "token-secret-input"
    let token_new_button_id, token_new_button = id "token-new-button"
    let reg_input_email_id, reg_input_email = id "register-email-input"
    let reg_input_nick_id, reg_input_nick = id "register-nick-input"
    let reg_input_password_id, reg_input_password = id "register-password-input"
    let input_secret_id, input_secret = id "register-secret-input"
    let input_consent_id, input_consent = id "first-connection-consent"
    let button_new_id, button_new = id "login-new-button"
    let login_input_email_id, login_input_email = id "login-email-input"
    let login_input_password_id, login_input_password = id "login-password-input"
    let login_forgotten_id, login_forgotten = id "txt_login_forgotten"
    let button_connect_id, button_connect = id "login-connect-button"
    let login_input_token_id, login_input_token = id "login-token-input"
    let button_token_connect_id, button_token_connect = id "login-token-button"
    let nickname_field_id, nickname_field = id "learnocaml-nickname"
  end

  module Dyn = struct
    (** Elements that are dynamically created (ids only) *)
    let exercise_list_id = "learnocaml-main-exercise-list"
    let tryocaml_id = "learnocaml-main-tryocaml"
    let lesson_id = "learnocaml-main-lesson"
    let toplevel_id = "learnocaml-main-toplevel"
  end
end

class type learnocaml_config = object
  method enableTryocaml: bool Js.optdef_prop
  method enableLessons: bool Js.optdef_prop
  method enableExercises: bool Js.optdef_prop
  method enableToplevel: bool Js.optdef_prop
  method enablePasswd: bool Js.optdef_prop
  method enablePlayground: bool Js.optdef_prop
  method txtLoginWelcome: Js.js_string Js.t Js.optdef_prop
  method txtNickname: Js.js_string Js.t Js.optdef_prop
end

let config : learnocaml_config Js.t = Js.Unsafe.js_expr "learnocaml_config"
let get_opt o = Js.Optdef.get o (fun () -> false)

let show_loading msg = show_loading ~id:El.loading_id H.[ul [li [txt msg]]]

let get_url token dynamic_url static_url id =
  match token with
  | Some _ -> dynamic_url ^ Url.urlencode id ^ "/"
  | None -> api_server ^ "/" ^ static_url ^ Url.urlencode id

let exercises_tab token _ _ () =
  show_loading [%i"Loading exercises"] @@ fun () ->
  Lwt_js.sleep 0.5 >>= fun () ->
  retrieve (Learnocaml_api.Exercise_index token)
  >>= fun (index, deadlines) ->
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
                | { Answer.grade ; _ } -> grade in
              let pct_signal, pct_signal_set = React.S.create pct_init in
              Learnocaml_local_storage.(listener (exercise_state exercise_id)) :=
                Some (function
                    | Some { Answer.grade ; _ } -> pct_signal_set grade
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
              a ~a:[ a_href (get_url token "/exercises/" "exercise.html#id=" exercise_id) ;
                     a_class [ "exercise" ] ] [
                div ~a:[ a_class [ "descr" ] ] (
                  h1 [ txt title ] ::
                  begin match short_description with
                    | None -> []
                    | Some text -> [ txt text ]
                  end
                );
                div ~a:[ a_class [ "time-left" ] ] [H.txt time_left];
                div ~a:[ Tyxml_js.R.Html5.a_class status_classes_signal ] [
                  stars_div stars;
                  div ~a:[ a_class [ "length" ] ] [
                    match kind with
                    | Exercise.Meta.Project -> txt [%i"project"]
                    | Exercise.Meta.Problem -> txt [%i"problem"]
                    | Exercise.Meta.Exercise -> txt [%i"exercise"] ] ;
                  div ~a:[ a_class [ "score" ] ] [
                    Tyxml_js.R.Html5.txt pct_text_signal
                  ]
                ] ] ::
              acc)
            acc exercises
      | Exercise.Index.Groups groups ->
          let h = match lvl with 1 -> h1 | 2 -> h2 | _ -> h3 in
          List.fold_left
            (fun acc (_, Exercise.Index.{ title ; contents }) ->
               format_contents (succ lvl)
                 (h ~a:[ a_class [ "pack" ] ] [ txt title ] :: acc)
                 contents)
            acc groups in
    List.rev (format_contents 1 [] index) in
  let list_div =
    match format_exercise_list
            Learnocaml_local_storage.(retrieve all_exercise_states)
    with
    | [] -> H.div [H.txt [%i"No open exercises at the moment"]]
    | l -> H.div ~a:[H.a_id El.Dyn.exercise_list_id] l
  in
    Manip.appendChild El.content list_div;
    Lwt.return list_div

let playground_tab token _ _ () =
  show_loading [%i"Loading playground"] @@ fun () ->
  Lwt_js.sleep 0.5 >>= fun () ->
  retrieve (Learnocaml_api.Playground_index ())
  >>= fun index ->
  let list_div =
    let format_contents (id, pmeta) =
      let open Tyxml_js.Html5 in
      let title = pmeta.Playground.Meta.title in
      let short_description = pmeta.Playground.Meta.short_description in
      a ~a:[ a_href (get_url token "/playground/" "playground.html#id=" id) ;
             a_class [ "exercise" ] ] [
          div ~a:[ a_class [ "descr" ] ] (
              h1 [ txt title ] ::
                begin match short_description with
                | None -> []
                | Some text -> [ txt text ]
                end
            );
        ]
    in
    List.map format_contents index in
  let list_div = H.div ~a:[H.a_id El.Dyn.exercise_list_id] list_div in
  Manip.appendChild El.content list_div;
  Lwt.return list_div

let lessons_tab select (arg, set_arg, _delete_arg) () =
  show_loading [%i"Loading lessons"] @@ fun () ->
  Lwt_js.sleep 0.5 >>= fun () ->
  retrieve (Learnocaml_api.Lesson_index ()) >>= fun index ->
  let navigation_div =
    Tyxml_js.Html5.(div ~a: [ a_class [ "navigation" ] ] []) in
  let main_div =
    Tyxml_js.Html5.(div ~a: [ a_class [ "toplevel-pane" ] ] []) in
  let options =
    List.map
      (fun (lesson_id, lesson_title) ->
         lesson_id,
         Tyxml_js.Html5.
           (option ~a: [ a_value lesson_id ] (txt lesson_title)))
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
    retrieve ~ignore:Lesson.{title=""; steps=[]}
      (Learnocaml_api.Lesson id) >>= fun { Lesson.steps; _ } ->
    Manip.removeChildren main_div ;
    (if loading then show_loading [%i"Running OCaml examples"]
     else fun f -> f ()) @@ fun () ->
    let toplevel_buttons_group = button_group () in
    disable_button_group toplevel_buttons_group (* enabled after init *) ;
    toplevel_launch ~display_welcome:false main_div
      Learnocaml_local_storage.toplevel_history
      (fun () -> Lwt.async select) toplevel_buttons_group ("lesson-" ^ id)
    >>= fun top ->
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
    Tyxml_js.Html5.(div ~a: [ a_id El.Dyn.lesson_id ])
      [ navigation_div ; main_div ] in
  Manip.appendChild El.content lesson_div ;
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
  Lwt.return lesson_div

let tryocaml_tab select (arg, set_arg, _delete_arg) () =
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
    Tyxml_js.Html5.(div ~a: [ a_id El.Dyn.tryocaml_id ])
      [ navigation_div ; step_div ; toplevel_div ; buttons_div ] in
  let toplevel_buttons_group = button_group () in
  disable_button_group toplevel_buttons_group (* enabled after init *) ;
  let toplevel_launch =
    let on_disable () = Manip.addClass step_div "disabled" in
    let on_enable () = Manip.removeClass step_div "disabled" in
    toplevel_launch ~on_disable ~on_enable toplevel_div
      Learnocaml_local_storage.toplevel_history
      (fun () -> Lwt.async select) toplevel_buttons_group "tryocaml"
  in
  show_loading [%i"Loading tutorials"] @@ fun () ->
  Lwt_js.sleep 0.5 >>= fun () ->
  Manip.appendChild El.content tutorial_div ;
  retrieve ~ignore:[] (Learnocaml_api.Tutorial_index ()) >>= fun index ->
  let index =
    List.flatten @@ List.fold_left
      (fun acc (_, { Index.series_tutorials; _ }) ->
         series_tutorials :: acc)
      [] index in
  let options =
    List.map
      (fun { Tutorial.Index.name; title } ->
         name,
         H.option ~a: [ H.a_value name ]
           (H.txt (extract_text_from_rich_text title)))
      index in
  let selector =
    Tyxml_js.Html5.(select (snd (List.split options))) in
  let dom_selector =
    Tyxml_js.To_dom.of_select selector in
  let prev_and_next id =
    let rec loop = function
      | [] -> assert false
      | [ _ ] (* assumes single id *) -> None, None
      | { Tutorial.Index.name = one ; _ } ::
        { Tutorial.Index.name = two ; _ } :: _ when id = one -> None, Some two
      | { Tutorial.Index.name = one ; _ } ::
        { Tutorial.Index.name = two ; _ } :: [] when id = two -> Some one, None
      | { Tutorial.Index.name = one ; _ } ::
        { Tutorial.Index.name = two ; _ } ::
        { Tutorial.Index.name = three ; _} :: _ when id = two ->
          Some one, Some three
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
  let load_tutorial tutorial_name step_id () =
    retrieve ~ignore:{Tutorial.title = []; steps = []}
      (Learnocaml_api.Tutorial tutorial_name) >>= fun { Tutorial.steps; _ } ->
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
                let elt = Tyxml_js.Html.pre [ Tyxml_js.Html.txt code ] in
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
  toplevel_launch >>= fun top ->
  let toplevel_button =
    button ~container: buttons_div ~theme: "dark" ~group:toplevel_buttons_group ?state:None in
  init_toplevel_pane toplevel_launch top toplevel_buttons_group toplevel_button ;
  Lwt.return tutorial_div

let toplevel_tab select _ () =
  let container =
    Tyxml_js.Html5.(div ~a: [ a_class [ "toplevel-pane" ] ]) [] in
  let buttons_div =
    Tyxml_js.Html5.(div ~a: [ a_class [ "buttons" ] ]) [] in
  let div =
    Tyxml_js.Html5.(div ~a: [ a_id El.Dyn.toplevel_id ])
      [ container ; buttons_div ] in
  show_loading [%i"Launching OCaml"] @@ fun () ->
  let toplevel_buttons_group = button_group () in
  disable_button_group toplevel_buttons_group (* enabled after init *) ;
  toplevel_launch container
    Learnocaml_local_storage.toplevel_history
    (fun _ -> Lwt.async select) toplevel_buttons_group "toplevel"
  >>= fun top ->
  Manip.appendChild El.content div ;
  let button = button ~container: buttons_div ~theme: "dark" ?group:None ?state:None in
  init_toplevel_pane (Lwt.return top) top toplevel_buttons_group button ;
  Lwt.return div

let teacher_tab token a b () =
  show_loading [%i"Loading student info"] @@ fun () ->
  Learnocaml_teacher_tab.teacher_tab token a b () >>= fun div ->
  Lwt.return div

let get_stored_token () =
  Learnocaml_local_storage.(retrieve sync_token)

let can_show_token () =
  try
    Lwt.return Learnocaml_local_storage.(retrieve can_show_token)
  with Not_found ->
        Server_caller.request (Learnocaml_api.Can_login (get_stored_token ())) >|= function
        | Error _ -> false
        | Ok res ->
           Learnocaml_local_storage.(store can_show_token) res;
           res

let sync () = sync (get_stored_token ())

let token_disp_div token =
  H.input ~a: [
    H.a_input_type `Text;
    H.a_size 17;
    H.a_style "font-size: 110%; font-weight: bold;";
    H.a_class ["learnocaml_token"];
    H.a_readonly ();
    H.a_value (Token.to_string token);
  ] ()

let show_token_dialog token =
  ext_alert ~title:[%i"Your Learn-OCaml token"] [
      H.p [H.txt [%i"Your token is displayed below. It identifies you and \
                     allows to share your workspace between devices."]];
      H.p [H.txt [%i"Please write it down."]];
      H.div ~a:[H.a_style "text-align: center;"] [token_disp_div token];
    ]

let complete_reset_password cb = function
  | Ok email ->
     alert ~title:[%i"RESET REQUEST SENT"]
       ([%i"A reset link has been sent to the following address: "]
       ^ email);
     Lwt.return_none
  | Error (`Not_found _) ->
     alert ~title:[%i"ERROR"]
       [%i"The entered e-mail couldn't be recognised."];
     Lwt.return_none
  | Error e ->
     lwt_alert ~title:[%i"REQUEST ERROR"] [
         H.p [H.pcdata [%i"Could not retrieve data from server"]];
         H.code [H.pcdata (Server_caller.string_of_error e)];
       ] ~buttons:[
         [%i"Retry"], (fun () -> cb ());
         [%i"Cancel"], (fun () -> Lwt.return_none);
       ]

let complete_change_email cb new_email = function
  | Ok () ->
     alert ~title:[%i"RESET REQUEST SENT"]
       ([%i"A confirmation e-mail has been sent to the address: "]
       ^ new_email);
     Lwt.return_none
  | Error (`Not_found _) ->
     alert ~title:[%i"ERROR"]
       [%i"The entered e-mail couldn't be recognised."];
     Lwt.return_none
  | Error e ->
     lwt_alert ~title:[%i"REQUEST ERROR"] [
         H.p [H.pcdata [%i"Could not retrieve data from server"]];
         H.code [H.pcdata (Server_caller.string_of_error e)];
       ] ~buttons:[
         [%i"Retry"], (fun () -> cb ());
         [%i"Cancel"], (fun () -> Lwt.return_none);
       ]

let check_email_js email =
  let re = Regexp.regexp Learnocaml_data.email_regexp_js in
  match Regexp.string_match re email 0 with
  | Some _ -> true
  | None -> false

let validate_email email =
  if check_email_js email then Lwt.return_some email
  (* FIXME: the dialog does not show up *)
  else begin
      alert ~title:[%i"ERROR"]
        ([%i"The entered e-mail is invalid: "] ^ email);
      Lwt.return_none
    end

let init_token_dialog () =
  let open El.Login_overlay in
  Manip.SetCss.display login_overlay "block";
  if get_opt config##.enablePasswd then
    Manip.SetCss.display login_new_token "none"
  else
    begin
      Manip.SetCss.display login_new "none";
      Manip.SetCss.display login_returning "none"
    end;
  let get_token, got_token = Lwt.task () in
  let create_raw_token () =
    if not (get_opt config##.enablePasswd) then
      let nickname = String.trim (Manip.value token_nickname) in
      if Token.check nickname || String.length nickname < 2 then
        (Manip.SetCss.borderColor token_nickname "#f44";
         Lwt.return_none)
      else
        let secret = Sha.sha512 (String.trim (Manip.value token_secret)) in
        retrieve (Learnocaml_api.Nonce ())
        >>= fun nonce ->
        let secret = Sha.sha512 (nonce ^ secret) in
        (Learnocaml_local_storage.(store nickname) nickname;
         retrieve
           (Learnocaml_api.Create_token (secret, None, Some nickname))
         >>= fun token ->
         Learnocaml_local_storage.(store sync_token) token;
         Learnocaml_local_storage.(store can_show_token) true;
         show_token_dialog token;
         Lwt.return_some (token, nickname))
    else
      Lwt.return_none
  in
  let create_token () =
    if get_opt config##.enablePasswd then
      let email = Manip.value reg_input_email and
          password = Manip.value reg_input_password and
          consent = Manip.checked input_consent and
          consent_label = find_component "txt_first_connection_consent" in
      let email_criteria = not (check_email_js email) and
          passwd_criteria = String.length password < 8 in
      Manip.SetCss.borderColor reg_input_email "";
      Manip.SetCss.borderColor reg_input_password "";
      Manip.SetCss.fontWeight consent_label "";
      if email_criteria || passwd_criteria || not consent then
        begin
          if email_criteria then
            Manip.SetCss.borderColor reg_input_email "#f44";
          if passwd_criteria then
            Manip.SetCss.borderColor reg_input_password "#f44";
          if not consent then
            Manip.SetCss.fontWeight consent_label "bold";
          Lwt.return_none
        end
      else
        let nickname = String.trim (Manip.value reg_input_nick) and
            secret = Sha.sha512 (String.trim (Manip.value input_secret)) in
        retrieve (Learnocaml_api.Nonce ())
        >>= fun nonce ->
        let secret = Sha.sha512 (nonce ^ secret) in
        (Learnocaml_local_storage.(store nickname) nickname;
         retrieve
           (Learnocaml_api.Create_user (email, nickname, password, secret))
         >>= fun token ->
         Learnocaml_local_storage.(store sync_token) token;
         Learnocaml_local_storage.(store can_show_token) false;
         Lwt.return_some (token, nickname))
    else
      Lwt.return_none
  in
  let rec login_passwd () =
    let input = Manip.value login_input_email and
        password = Manip.value login_input_password in
    if get_opt config##.enablePasswd then
      Server_caller.request (Learnocaml_api.Login (input, password)) >>= function
      | Error e ->
         alert ~title:[%i"ERROR"] (Server_caller.string_of_error e);
         Lwt.return_none
      | Ok token ->
         Server_caller.request (Learnocaml_api.Fetch_save token) >>= function
         | Ok save ->
            set_state_from_save_file ~token save;
            Learnocaml_local_storage.(store can_show_token) false;
            Lwt.return_some (token, save.Save.nickname)
         | Error (`Not_found _) ->
            alert ~title:[%i"TOKEN NOT FOUND"]
              [%i"The entered token couldn't be recognised."];
            Lwt.return_none
         | Error e ->
            lwt_alert ~title:[%i"REQUEST ERROR"] [
                H.p [H.txt [%i"Could not retrieve data from server"]];
                H.code [H.txt (Server_caller.string_of_error e)];
              ] ~buttons:[
                [%i"Retry"], (fun () -> login_passwd ());
                [%i"Cancel"], (fun () -> Lwt.return_none);
              ]
    else
      Lwt.return_none
  in
  let rec login_token () =
    let input = login_input_token in
    match Token.parse (Manip.value input) with
    | exception (Failure _) ->
       Manip.SetCss.borderColor input "#f44";
       Lwt.return_none
    | token ->
       Server_caller.request (Learnocaml_api.Can_login token) >>= function
       | Error _ | Ok false ->
          alert ~title:[%i"TOKEN NOT FOUND"]
            [%i"The entered token couldn't be recognised."];
          Lwt.return_none
       | _ ->
          Server_caller.request (Learnocaml_api.Fetch_save token) >>= function
          | Ok save ->
             set_state_from_save_file ~token save;
             Learnocaml_local_storage.(store can_show_token) true;
             Lwt.return_some (token, save.Save.nickname)
          | Error (`Not_found _) ->
             alert ~title:[%i"TOKEN NOT FOUND"]
               [%i"The entered token couldn't be recognised."];
             Lwt.return_none
          | Error e ->
             lwt_alert ~title:[%i"REQUEST ERROR"] [
                 H.p [H.pcdata [%i"Could not retrieve data from server"]];
                 H.code [H.pcdata (Server_caller.string_of_error e)];
               ] ~buttons:[
                 [%i"Retry"], (fun () -> login_token ());
                 [%i"Cancel"], (fun () -> Lwt.return_none);
               ]
  in
  let rec reset_password () =
    if get_opt config##.enablePasswd then
      let email = Manip.value login_input_email in
      Server_caller.request (Learnocaml_api.Send_reset_password email)
      >>= complete_reset_password reset_password
    else
      Lwt.return_none
  in
  let handler f t = fun _ ->
    Lwt.async (fun () ->
        f () >|= function
        | Some token -> Lwt.wakeup got_token token
        | None -> ());
    t
  in
  Manip.Ev.onclick token_new_button (handler create_raw_token false);
  Manip.Ev.onreturn token_nickname (handler create_raw_token ());
  Manip.Ev.onclick button_new (handler create_token false);
  Manip.Ev.onreturn reg_input_nick (handler create_token ());
  Manip.Ev.onclick button_connect (handler login_passwd false);
  Manip.Ev.onreturn login_input_password (handler login_passwd ());
  Manip.Ev.onclick login_forgotten (handler reset_password false);
  Manip.Ev.onclick button_token_connect (handler login_token false);
  Manip.Ev.onreturn login_input_token (handler login_token ());
  get_token >|= fun (token, nickname) ->
  (Tyxml_js.To_dom.of_input nickname_field)##.value := Js.string nickname;
  Manip.SetCss.display login_overlay "none";
  token

let get_cookie name =
  Js.(to_array (str_array (Dom_html.document##.cookie##split (string ";"))))
  |> Array.fold_left
       (fun res v ->
         match res with
         | Some _ -> res
         | None -> let cookie = Js.to_string v
                                |> String.trim
                                |> String.split_on_char '=' in
                   match cookie with
                   | n :: v when n = name -> Some (String.concat "=" v)
                   | _ -> None)
       None

let delete_cookie name =
  Dom_html.document##.cookie := Js.string (Printf.sprintf "%s=; Max-age=-1;" name)

let init_sync_token button_group =
  catch
    (fun () ->
      begin try
           if get_cookie "token" <> None then
             Learnocaml_local_storage.(store can_show_token) false;
           Lwt.return Learnocaml_local_storage.(retrieve sync_token)
         with Not_found ->
               match get_cookie "token" with
               | None -> init_token_dialog ()
               | Some token ->
                  let token = Learnocaml_data.Token.parse token in
                  Server_caller.request (Learnocaml_api.Fetch_save token) >>= function
                  | Ok save ->
                     set_state_from_save_file ~token save;
                     Learnocaml_local_storage.(store can_show_token) false;
                     Lwt.return token
                  | Error _ -> init_token_dialog ()
       end >>= fun token ->
       enable_button_group button_group ;
       Lwt.return (Some token))
    (fun _ -> Lwt.return None)

let set_string_translations () =
  let configured v s = Js.Optdef.case v (fun () -> s) Js.to_string in
  let translations = [
    "txt_connected_as",
    [%i"Connected as"];
    "txt_choose_activity",
    [%i"Activities"];
    "txt_login_welcome", configured config##.txtLoginWelcome
      [%i"Welcome to Learn OCaml"];
    "txt_token_first_connection", [%i"First connection"];
    "txt_token_first_connection_dialog", [%i"Choose a nickname"];
    "txt_token_secret", [%i"Enter the secret"];
    "txt_token_new", [%i"Create new token"];
    "txt_first_connection", [%i"First connection"];
    "txt_first_connection_email", [%i"E-mail address"];
    "txt_first_connection_nickname", [%i"Nickname"];
    "txt_first_connection_password", [%i"Password"];
    "txt_first_connection_secret", [%i"Secret"];
    "txt_secret_label", [%i"The secret is the passphrase provided by \
                            your teacher to sign-up."];
    "txt_login_new", [%i"Create new token"];
    "txt_returning", [%i"Returning user"];
    "txt_returning_email", [%i"E-mail address"];
    "txt_returning_password", [%i"Password"];
    "txt_login_returning",  [%i"Connect"];
    "txt_login_forgotten", [%i"Forgot your password?"];
    "txt_first_connection_consent", [%i"By submitting this form, I accept that the \
                                       information entered will be used in the \
                                        context of the Learn-OCaml plateform."];
    "txt_returning_with_token", [%i"Login with a legacy token"];
    "txt_returning_token", [%i"Token"];
    "txt_token_returning", [%i"Connect"];
    "txt_upgrade", [%i"Upgrade account"];
  ] in
  List.iter
    (fun (id, text) ->
       Manip.setInnerHtml (find_component id) text)
    translations;
  let placeholder_translations = [
    El.nickname_field, configured config##.txtNickname
      [%i"Nickname"];
  ] in
  List.iter
    (fun (el, text) ->
       (Tyxml_js.To_dom.of_input el)##.placeholder := Js.string text)
    placeholder_translations

let () =
  Lwt.async_exception_hook := begin fun e ->
    Firebug.console##log (Js.string
                            (Printexc.to_string e ^
                             if Printexc.backtrace_status () then
                               Printexc.get_backtrace ()
                             else ""));
    match e with
    | Lwt.Canceled -> ()
    | Failure message -> fatal message
    | Server_caller.Cannot_fetch message -> fatal message
    | exn -> fatal (Printexc.to_string exn)
  end ;
  (match Js_utils.get_lang() with Some l -> Ocplib_i18n.set_lang l | None -> ());
  Lwt.async @@ fun () ->
  set_string_translations ();
  Dom_html.document##.title :=
    Js.string ("Learn OCaml" ^ " v."^Learnocaml_api.version);
  Manip.setInnerText El.version ("v."^Learnocaml_api.version);
  Learnocaml_local_storage.init () ;
  let sync_button_group = button_group () in
  disable_button_group sync_button_group;
  let menu_hidden = ref true in
  let no_tab_selected () =
    Manip.removeChildren El.content ;
    let div =
      Tyxml_js.Html5.(div ~a: [ a_class [ "placeholder" ] ])
        Tyxml_js.Html5.[ div [ txt [%i"Choose an activity."] ]] in
    Manip.removeChildren El.content ;
    Manip.appendChild El.content div ;
    delete_arg "activity"
  in
  let show_upgrade_button () =
    let token = Learnocaml_local_storage.(retrieve sync_token) and
        input = Js.Unsafe.coerce @@ H.toelt (find_component "upgrade-token") in
    input##.value := Js.string @@ Token.to_string token;
    Manip.SetCss.display (find_component "learnocaml-upgrade-container") "block"
  in
  let init_op () =
    let rec change_password () =
      Server_caller.request (Learnocaml_api.Change_password
                               Learnocaml_local_storage.(retrieve sync_token))
      >>= complete_reset_password change_password in
    let rec change_email () =
      Lwt.catch
        (fun () ->
          ask_string ~title:[%i"New e-mail address"]
            [H.txt [%i"Enter your new e-mail address: "]]
          >>= validate_email
          >>= function
          | Some address ->
             Server_caller.request
               (Learnocaml_api.Change_email (Learnocaml_local_storage.(retrieve sync_token),
                                             address))
             >>= complete_change_email change_email address
          | None -> Lwt.return_none)
        (fun _exn -> Lwt.return_none) in
    let buttons = [[%i"Change password"], change_password;
                   [%i"Change e-mail"], change_email] in
    let container = El.op_buttons_container in
    Manip.removeChildren container;
    List.iter (fun (name, callback) ->
        let btn = Tyxml_js.Html5.(button [txt name]) in
        Manip.Ev.onclick btn (fun _ -> Lwt.async callback; true);
        Manip.appendChild container btn) buttons
  in
  let init_tabs token =
    let tabs =
      (if get_opt config##.enableTryocaml
       then [ "tryocaml", ([%i"Try OCaml"], tryocaml_tab) ] else []) @
      (if get_opt config##.enableLessons
       then [ "lessons", ([%i"Lessons"], lessons_tab) ] else []) @
        (if get_opt config##.enableExercises then
           ["exercises", ([%i"Exercises"], exercises_tab token)]
        else []) @
      (if get_opt config##.enableToplevel
       then [ "toplevel", ([%i"Toplevel"], toplevel_tab) ] else []) @
        (if get_opt config##.enablePlayground
       then [ "playground", ([%i"Playground"], playground_tab token) ] else []) @
      (match token with
       | Some t when Token.is_teacher t ->
           [ "teacher", ([%i"Teach"], teacher_tab t) ]
       | _ -> [])
    in
    let container = El.tab_buttons_container in
    let current_btn = ref None in
    let current_args = ref (ref []) in
    let select_thread = ref None in
    Manip.removeChildren container ;
    List.map
      (fun (id, (name, callback)) ->
         let btn = Tyxml_js.Html5.(button [ txt name]) in
         let div = ref None in
         let args = ref [] in
         let rec select () =
           let th () =
             Lwt.pause () >>= fun () ->
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
             Lwt.return_unit
           in
           (match !select_thread with None -> () | Some th -> Lwt.cancel th);
           Lwt.finalize (fun () ->
               let th = th () in
               select_thread := Some th;
               th)
             (fun () -> select_thread := None; Lwt.return_unit)
         in
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
  let download_all () =
    let token = get_stored_token () |> Token.to_string in
    Dom_html.window##.location##assign
    (Js.string @@ "/archive.zip?token=" ^ token);
    Lwt.return_unit
  in
  let logout_dialog () =
    can_show_token () >>= fun show_token ->
    Server_caller.request
      (Learnocaml_api.Update_save
         (get_stored_token (), get_state_as_save_file ()))
    >|= (function
         | Ok _ ->
            if show_token then
              [%i"Be sure to write down your token before logging out:"]
            else
              [%i"Are you sure you want to logout?"]
        | Error _ ->
            [%i"WARNING: the data could not be synchronised with the server. \
                Logging out will lose your local changes, be sure you exported \
                a backup."])
    >|= fun s ->
    let dialog_content =
      (H.p [H.txt s]) ::
        if show_token then
          [H.div ~a:[H.a_style "text-align: center;"]
             [token_disp_div (get_stored_token ())]]
        else
          [] in
    confirm ~title:[%i"Logout"] ~ok_label:[%i"Logout"]
      dialog_content
      (fun () ->
         Lwt.async @@ fun () ->
         Learnocaml_local_storage.clear ();
         delete_cookie "token";
         reload ();
         Lwt.return_unit)
  in
  let show_token_button_state = button_state () in
  List.iter (fun (text, icon, state, f) ->
      button ~container:El.sync_buttons ~theme:"white" ~group:sync_button_group ?state:state ~icon text f)
    [
      [%i"Show token"], "token", Some show_token_button_state, (fun () ->
          show_token_dialog (get_stored_token ());
          Lwt.return_unit);
      [%i"Sync workspace"], "sync", None, (fun () ->
          catch_with_alert @@ fun () ->
          sync () >>= fun _ -> Lwt.return_unit);
      [%i"Export to file"], "download", None, download_save;
      [%i"Import"], "upload", None, import_save;
      [%i"Download all source files"], "download", None, download_all;
      [%i"Logout"], "logout", None,
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
  Server_caller.request (Learnocaml_api.Version ()) >>=
    (function
     | Ok _ ->
        init_sync_token sync_button_group >|= init_tabs >>= fun tabs ->
        can_show_token () >>= fun show_token ->
        (if not show_token then
           Server_caller.request (Learnocaml_api.Is_account (get_stored_token ())) >|=
             (function
              | Ok true -> init_op ()
              | _ -> show_upgrade_button ()) >|= fun () ->
           disable_button show_token_button_state
         else if get_opt config##.enablePasswd then
           Lwt.return @@ show_upgrade_button ()
         else
           Lwt.return_unit) >>= fun () ->
        Lwt.return tabs
     | Error _ -> Lwt.return (init_tabs None)) >>= fun tabs ->
  try
    let activity = arg "activity" in
    let (_, select) = List.assoc activity tabs in
    select ()
  with Not_found ->
    no_tab_selected ();
    Lwt.return ()
