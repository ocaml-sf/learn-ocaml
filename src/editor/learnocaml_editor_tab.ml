(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_utils
open Lwt
open Learnocaml_data
open Learnocaml_common
open Editor
open Editor_lib   
module H = Tyxml_js.Html5


let fetch_editor_index ()=
  let index=
    Learnocaml_local_storage.(retrieve editor_index ) in

  let json =
    Json_repr_browser.Json_encoding.construct
       (SMap.enc editor_state_enc) index in
    try Lwt.return (Json_repr_browser.Json_encoding.destruct
                      (SMap.enc editor_state_enc) json) with exn ->
    let msg =
      Format.asprintf "bad structure for %s@.%a"
        "index"
        (fun ppf -> Json_encoding.print_error ppf) exn in
    Lwt.fail (Server_caller.Cannot_fetch msg);;


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
       show_load "learnocaml-main-loading" [ abort_message ] ;
       Manip.SetCss.opacity abort_message (Some "1") ;
     end ;
  true) ;;
 
             
let rec editor_tab token _ _ () =


    Lwt_js.sleep 0.5 >>= fun () ->
    let _ =match Learnocaml_local_storage.(retrieve editor_index) with
    | exception Not_found ->
       Learnocaml_local_storage.(store editor_index) SMap.empty
    | _ -> ()
    in
    let content_div = find_component "learnocaml-main-content" in
    fetch_editor_index () >>=
    fun index ->
    let format_exercise_list contents =
      let open Tyxml_js.Html5 in
      let open Exercise.Meta in 
      SMap.fold
        (fun exercise_id editor_sate acc ->
          div ~a:[a_id "toolbar"; a_class ["button"]] [
              (
                   let button = button ~a:[a_id exercise_id]
                                  [img ~src:"icons/icon_cleanup_dark.svg"
                                     ~alt:"" () ; pcdata "" ] in
                   Manip.Ev.onclick button
                     (delete_button_handler exercise_id) ;button
               );
              (
                let button = button ~a:[a_id exercise_id]
                               [img ~src:"icons/icon_download_dark.svg"
                                  ~alt:"" () ; pcdata "" ] in
                Manip.Ev.onclick button
                  (fun _ ->
                    let name = exercise_id ^ ".json" in
                    let content =
                      SMap.find
                        exercise_id
                        Learnocaml_local_storage.(retrieve editor_index)
                    in
                    let json =
                      Json_repr_browser.Json_encoding.construct
                        Editor.editor_state_enc
                        content in
                       let contents =
                         (Js._JSON##stringify json) in
                       Learnocaml_common.fake_download ~name ~contents;
                       true) ;button
         )] ::
        a ~a:[ a_href ("editor.html#id="^exercise_id) ;
               a_class [ "exercise" ] ]
          [div ~a:[ a_class [ "descr" ] ] [
               h1 [ pcdata editor_sate.metadata.title ] ;
               p [ match editor_sate.metadata.short_description with
                   | None -> pcdata [%i"No description available."]
                       | Some text -> pcdata text ] ;
             ] ;
           div ~a:[a_class["stats"]] [
               div ~a:[ a_class [ "stars" ] ] [
                   let num = 5 * int_of_float (editor_sate.metadata.stars*. 2.) in
                   let num = max (min num 40) 0 in
                   let alt =
                     Format.asprintf "difficulty: %d / 40" num in
                   let src =
                     Format.asprintf "icons/stars_%02d.svg" num in
                   img ~alt ~src ()
                 ] ;
               div ~a:[ a_class [ "length" ] ] [
                   match editor_sate.metadata.kind with
                   | Project -> pcdata "editor project"
                   | Problem -> pcdata "editor problem"
                   | Exercise ->
                      pcdata "editor exercise" ] ;
                 ];
          ] ::
          acc) index contents
    in
    let open Tyxml_js.Html5 in
    let open Learnocaml_exercise in
    let open Exercise.Meta in
    let restore_bar = a ~a:[ a_onclick (fun _ ->
         let _ = begin
             Learnocaml_common.fake_upload () >>= fun (_, contents) ->
             let save_file =
               Json_repr_browser.Json_encoding.destruct 
                 editor_state_enc
                 (Js._JSON##(parse contents)) in
             let messages = Tyxml_js.Html5.ul [] in
             
             let id = match save_file.metadata.id with
                 None -> ""
               | Some id -> id
             in
             if idUnique id &&
                  titleUnique save_file.metadata.title then
               begin
                 let old_index=
                   Learnocaml_local_storage.(retrieve editor_index) in
                 let new_index=SMap.add id save_file old_index in
                 Learnocaml_local_storage.(store
                                             (editor_index) new_index);
                Dom_html.window##.location##reload;
               
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
                 show_load "learnocaml-main-loading" [ abort_message ] ;
                 Manip.SetCss.opacity abort_message (Some "1");
               end;
             Lwt.return ();
           end in ();
            true); a_class [ "exercise"] ]
                     [ div ~a:[ a_class [ "descr" ] ] [
                           h1 [ pcdata [%i"Import an exercise"] ];
                           p [pcdata [%i"Import a new exercise \
                                         from a json file"]]]]
    in
    let c= List.rev
             (format_exercise_list 
                ( [a ~a:[
                       a_href (Printf.sprintf "new_exercise.html?token=%s" (Token.to_string token));
                       a_class [ "exercise" ] ] [
                       div ~a:[ a_class [ "descr" ] ] [
                           h1 [ pcdata [%i"New exercise"] ];
                           p [pcdata [%i"Create \
                                         a new exercise"]]]];
                   restore_bar]) )
    in
    let list_div =
      Tyxml_js.Html5.(div ~a:
                        [ Tyxml_js.Html5.a_id "learnocaml-main-exercise-list" ])
        c in
    Manip.appendChild content_div list_div ;
    hide_loading ~id:"learnocaml-main-loading" () ;
    Lwt.return list_div;;
