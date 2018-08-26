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
open Lwt.Infix

let find_div_or_append_to_body id =
  match Manip.by_id id with
  | Some div -> div
  | None ->
      let div = Tyxml_js.Html.(div ~a:[ a_id id ]) [] in
      Manip.(appendChild Elt.body) div;
      div

let find_component id =
  match Js_utils.Manip.by_id id with
  | Some div -> div
  | None -> failwith ("Cannot find id " ^ id)

let fake_download ~name ~contents =
  (* TODO: add some primitives to jsoo and clean this up  *)
  let blob : (Js.js_string Js.t Js.js_array Js.t -> File.blob Js.t) Js.constr =
    Js.Unsafe.global ##. _Blob in
  let blob = new%js blob (Js.array [| contents |]) in
  let url =
    Js.Unsafe.meth_call (Js.Unsafe.global##._URL) "createObjectURL" [| Js.Unsafe.inject blob |] in
  let link = Dom_html.createA Dom_html.document in
  link##.href := url ;
  Js.Unsafe.set link (Js.string "download") (Js.string name) ;
  ignore (Dom_html.document##.body##(appendChild ((link :> Dom.node Js.t)))) ;
  ignore (Js.Unsafe.meth_call link "click" [||]) ;
  ignore (Dom_html.document##.body##(removeChild ((link :> Dom.node Js.t))))

let fake_upload () =
  let input_files_load =
    Dom_html.createInput ~_type: (Js.string "file") Dom_html.document in
  let result_t, result_wakener = Lwt.wait () in
  let fail () =
    Lwt.wakeup_exn result_wakener
      (Failure "file loading not implemented for this browser") ;
    Js._true in
  input_files_load##.onchange := Dom.handler (fun ev ->
      Js.Opt.case (ev##.target) fail @@ fun target ->
      Js.Opt.case (Dom_html.CoerceTo.input target) fail @@ fun input ->
      Js.Optdef.case (input##.files) fail @@ fun files ->
      Js.Opt.case (files##(item (0))) fail @@ fun file ->
      let name = Js.to_string file##.name in
      let fileReader = new%js File.fileReader in
      fileReader##.onload := Dom.handler (fun ev ->
          Js.Opt.case (ev##.target) fail @@ fun target ->
          Js.Opt.case (File.CoerceTo.string (target##.result)) fail @@ fun result ->
          Lwt.wakeup result_wakener (name, result) ;
          Js._true) ;
      fileReader##(readAsText file) ;
      Js._true) ;
  ignore (Js.Unsafe.meth_call input_files_load "click" [||]) ;
  result_t

let fatal message =
  let id = "ocp-fatal-layer" in
  let div = match Manip.by_id id with
    | Some div -> div
    | None ->
        let sty =
          "display: flex;\
           flex-direction: column;\
           position: absolute;\
           top: 0; left: 0; bottom: 0; right: 0;\
           background: rgba(0,0,0,0.8);\
           color: white;\
           z-index: 22222;" in
        let div = Tyxml_js.Html.(div ~a:[ a_id id ; a_style sty ]) [] in
        Manip.(appendChild Elt.body) div;
        div in
  Manip.replaceChildren div
    Tyxml_js.Html.
      [ div ~a: [ a_style "flex: 1" ] [] ;
        div ~a: [ a_style "border: 3px white double;\
                           font-family: 'Inconsolata', monospace;\
                           flex: 0 0 auto;\
                           background: black;\
                           margin: auto;"]
          [ h3 ~a: [ a_style "margin: 0;\
                              padding: 10px;\
                              text-align: center;" ]
              [ pcdata [%i"INTERNAL ERROR"] ] ;
            pre ~a: [ a_style "margin: 0;\
                               border-top: 1px white solid;\
                               padding: 20px;" ]
              [ pcdata (String.trim message) ] ] ;
        div ~a: [ a_style "flex: 1" ] [] ]

let alert ?(title=[%i"ERROR"]) message =
  let id = "ocp-alert-layer" in
  let div = match Manip.by_id id with
    | Some div -> div
    | None ->
        let sty =
          "display: flex;\
           flex-direction: column;\
           position: absolute;\
           top: 0; left: 0; bottom: 0; right: 0;\
           background: rgba(0,0,0,0.8);\
           color: white;\
           z-index: 22221;" in
        let div = Tyxml_js.Html.(div ~a:[ a_id id ; a_style sty ]) [] in
        Manip.(appendChild Elt.body) div;
        div in
  let module H = Tyxml_js.Html in
  Manip.replaceChildren div [
    H.div ~a: [ H.a_style "flex: 1" ] [] ;
    H.div ~a: [ H.a_style "border: 3px white double;\
                           font-family: 'Inconsolata', monospace;\
                           flex: 0 0 auto;\
                           background: black;\
                           margin: auto;"]
      [ H.h3 ~a: [ H.a_style "margin: 0;\
                              padding: 10px;\
                              text-align: center;" ]
          [ H.pcdata title ] ;
        H.pre ~a: [ H.a_style "margin: 0;\
                               border-top: 1px white solid;\
                               padding: 20px;" ]
          [ H.pcdata (String.trim message) ];
        H.button ~a: [H.a_style "display: block;\
                                 margin: 10px auto;\
                                 padding: 5px 10px;\
                                 border: none;\
                                 background-color: white;\
                                 color: black;\
                                 text-align: center;";
                      H.a_onclick (fun _ ->
                          Manip.removeChild Manip.Elt.body div;
                          false)]
          [ H.pcdata "OK" ]
      ] ;
    H.div ~a: [ H.a_style "flex: 1" ] [];
  ]

let default_exn_printer = function
  | Failure msg -> msg
  | e -> Printexc.to_string e

let catch_with_alert ?(printer=default_exn_printer) f =
  Lwt.catch f @@ fun exn -> alert (printer exn); Lwt.return_unit

let hide_loading ?(id = "ocp-loading-layer") () =
  let elt = find_div_or_append_to_body id in
  Manip.(removeClass elt "initial") ;
  Manip.(removeClass elt "loading") ;
  Manip.(addClass elt "loaded")

let show_loading ?(id = "ocp-loading-layer") contents =
  let elt = find_div_or_append_to_body id in
  Manip.(addClass elt "loading-layer") ;
  Manip.(removeClass elt "loaded") ;
  Manip.(addClass elt "loading") ;
  let chamo_src =
    "icons/tryocaml_loading_" ^ string_of_int (Random.int 8 + 1) ^ ".gif" in
  Manip.replaceChildren elt
    Tyxml_js.Html.[
      div ~a: [ a_id "chamo" ] [ img ~alt: "loading" ~src: chamo_src () ] ;
      div ~a: [ a_class [ "messages" ] ] contents
    ]

let set_assoc name value =
  let rec set acc = function
    | [] -> List.rev ((name, value) :: acc)
    | (n, _) :: args when n = name ->
        List.rev_append ((name, value) :: acc) args
    | arg :: args -> set (arg :: acc) args in
  set []

let delete_assoc name =
  List.filter (fun (n, _) -> n <> name)

let arg, set_arg, delete_arg =
  let args = ref (Js_utils.parse_fragment ()) in
  let delete_arg name =
    args := delete_assoc name !args ;
    Js_utils.set_fragment !args in
  let set_arg name value =
    args := set_assoc name value !args ;
    Js_utils.set_fragment !args  in
  let arg name =
    List.assoc name !args in
  arg, set_arg, delete_arg

type button_group =
  (< disabled : bool Js.t Js.prop > Js.t * bool ref) list ref
  * Lwt_mutex.t
  * int ref

let button_group () : button_group =
  (ref [], Lwt_mutex.create (), ref 0)

type button_state =
  bool ref
  * (button_group * < disabled : bool Js.t Js.prop > Js.t) option ref

let button_state () : button_state =
  (ref false, ref None)

let disable_button_group (buttons, _, cpt) =
  incr cpt ;
  if !cpt = 1 then
    List.iter
      (fun (button, _) ->
         button##.disabled := Js.bool true)
      !buttons

let enable_button_group (buttons, _, cpt) =
  decr cpt ;
  if !cpt = 0 then
    List.iter
      (fun (button, state) ->
         if not !state then
           button##.disabled := Js.bool false)
      !buttons

let disable_button (disabled, self) =
  match !self with
  | None ->
      disabled := true
  | Some (_, button) ->
      disabled := true ;
      button##.disabled := Js.bool true

let enable_button (disabled, self) =
  match !self with
  | None ->
      disabled := false
  | Some ((_, _, cpt), button) ->
      disabled := false ;
      if !cpt = 0 then
        button##.disabled := Js.bool false

let button_group_disabled (_, _, cpt) =
  !cpt > 0

let disabling_button_group group cb =
  disable_button_group group ;
  Lwt_js.yield () >>= fun () ->
  Lwt.catch cb
    (function
      | Lwt.Canceled -> Lwt.return ()
      | exn -> Lwt.fail exn) >>= fun res ->
  enable_button_group group ;
  Lwt_js.yield () >>= fun () ->
  Lwt.return res

let disable_with_button_group component (buttons, _, _) =
  buttons :=
    ((component :> < disabled : bool Js.t Js.prop > Js.t), ref false)
    :: !buttons

let button ~container ~theme ?group ?state ~icon lbl cb =
  let (others, mutex, cnt) as group =
    match group with
    | None -> button_group ()
    | Some group -> group in
  let button =
    Tyxml_js.Html.(button [
        img ~alt:(lbl ^ " icon") ~src:("icons/icon_" ^ icon ^ "_" ^ theme ^ ".svg") () ;
        pcdata " " ;
        span ~a:[ a_class [ "label" ] ] [ pcdata lbl ]
      ]) in
  Manip.Ev.onclick button
    (fun _ ->
       begin Lwt.async @@ fun () ->
         Lwt_mutex.with_lock mutex @@ fun () ->
         disabling_button_group group cb
       end ;
       true) ;
  let dom_button =
    (Tyxml_js.To_dom.of_button button
     :> < disabled : bool Js.t Js.prop > Js.t) in
  let self_disabled =
    match state with
    | None -> ref false
    | Some (disabled, self) ->
        self := Some (group, dom_button) ;
        disabled in
  others := (dom_button, self_disabled) :: !others ;
  if !self_disabled || !cnt > 0 then
    dom_button##.disabled := Js.bool true ;
  Manip.appendChild container button

let dropdown ~id ~title items =
    let toggle _ =
      let menu = find_component id in
      let disp =
        match Manip.Css.display menu with
        | "block" -> "none"
        | _ ->
            Lwt_js_events.async (fun () ->
                Lwt_js_events.click window >|= fun _ ->
                Manip.SetCss.display menu "none"
              );
            "block"
      in
      Manip.SetCss.display menu disp;
      false
    in
    let module H = Tyxml_js.Html in
    H.div ~a: [H.a_class ["dropdown_btn"]] [
      H.button ~a: [H.a_onclick toggle]
        (title @ [H.pcdata " \xe2\x96\xbe" (* U+25BE *)]);
      H.div ~a: [H.a_id id; H.a_class ["dropdown_content"]] items
    ]

let gettimeofday () =
  let now = new%js Js.date_now in
  floor ((now ## getTime) *. 1000.) +. float (now ## getTimezoneOffset)

let render_rich_text ?on_runnable_clicked text =
  let open Learnocaml_data.Tutorial in
  let rec render acc text =
    match text with
    | [] -> List.rev acc
    | Text text :: rest ->
        render
          (Tyxml_js.Html.pcdata text :: acc)
          rest
    | Code { code ; runnable } :: rest ->
        let elt = Tyxml_js.Html.code [ Tyxml_js.Html.pcdata code ] in
        (match runnable, on_runnable_clicked with
         | true, Some cb ->
             Manip.addClass elt "runnable" ;
             Manip.Ev.onclick elt (fun _ -> cb code ; true)
         | _ -> ()) ;
        render (elt :: acc) rest ;
    | Emph text :: rest ->
        render
          (Tyxml_js.Html.em (render [] text) :: acc)
          rest
    | Image _ :: _ -> assert false
    | Math code :: rest ->
        render
          (Tyxml_js.Html.pcdata ("`" ^ code ^ "`") :: acc)
          rest in
  (render [] text
   :> [< Html_types.phrasing > `Code `Em `PCDATA ] Tyxml_js.Html.elt list)

let extract_text_from_rich_text text =
  let open Learnocaml_data.Tutorial in
  let rec render acc text =
    match text with
    | [] -> String.concat " " (List.rev acc)
    | Text text :: rest ->
        render (text :: acc) rest
    | Code { code } :: rest ->
        render (("[" ^ code ^ "]") :: acc) rest
    | Emph text :: rest ->
        render (("*" ^ render [] text ^ "*") :: acc) rest
    | Image { alt } :: rest ->
        render (("(" ^ alt ^ ")") :: acc) rest
    | Math code :: rest ->
        render (("$" ^ code ^ "$") :: acc) rest in
  render [] text

let set_state_from_save_file ?token save =
  let open Learnocaml_data.Save in
  let open Learnocaml_local_storage in
  match token with None -> () | Some t -> store sync_token t;
  store nickname save.nickname;
  store all_exercise_states save.all_exercise_states;
  store all_toplevel_histories save.all_toplevel_histories;
  store all_exercise_toplevel_histories save.all_exercise_toplevel_histories

let get_state_as_save_file () =
  let open Learnocaml_data.Save in
  let open Learnocaml_local_storage in
  {
    nickname = retrieve nickname;
    all_exercise_states = retrieve all_exercise_states;
    all_toplevel_histories = retrieve all_toplevel_histories;
    all_exercise_toplevel_histories = retrieve all_exercise_toplevel_histories;
  }

let sync token =
  let save_file = get_state_as_save_file () in
  Server_caller.request (Learnocaml_api.Update_save (token, save_file))
  >>= function
  | Ok save -> set_state_from_save_file ~token save; Lwt.return save
  | Error (`Not_found _) ->
      Server_caller.request_exn
        (Learnocaml_api.Create_token (Some token)) >>= fun _token ->
      assert (_token = token);
      Server_caller.request_exn
        (Learnocaml_api.Update_save (token, save_file)) >>= fun save ->
      set_state_from_save_file ~token save;
      Lwt.return save
  | Error e -> Lwt.fail_with (Server_caller.string_of_error e)
