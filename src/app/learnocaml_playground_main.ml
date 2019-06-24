(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_utils
open Lwt.Infix
open Learnocaml_common
open Learnocaml_data

module H = Tyxml_js.Html

let init_tabs, select_tab =
  let names = [ "toplevel" ; "editor" ] in
  let current = ref "toplevel" in
  let select_tab name =
    set_arg "tab" name ;
    Manip.removeClass
      (find_component ("learnocaml-exo-button-" ^ !current))
      "front-tab" ;
    Manip.removeClass
      (find_component ("learnocaml-exo-tab-" ^ !current))
      "front-tab" ;
    Manip.enable
      (find_component ("learnocaml-exo-button-" ^ !current)) ;
    Manip.addClass
      (find_component ("learnocaml-exo-button-" ^ name))
      "front-tab" ;
    Manip.addClass
      (find_component ("learnocaml-exo-tab-" ^ name))
      "front-tab" ;
    Manip.disable
      (find_component ("learnocaml-exo-button-" ^ name)) ;
    current := name in
  let init_tabs () =
    current := begin try
        let requested = arg "tab" in
        if List.mem requested names then requested else "toplevel"
      with Not_found -> "toplevel"
    end ;
    List.iter
      (fun name ->
         Manip.removeClass
           (find_component ("learnocaml-exo-button-" ^ name))
           "front-tab" ;
         Manip.removeClass
           (find_component ("learnocaml-exo-tab-" ^ name))
           "front-tab" ;
         Manip.Ev.onclick
           (find_component ("learnocaml-exo-button-" ^ name))
           (fun _ -> select_tab name ; true))
      names ;
    select_tab !current in
  init_tabs, select_tab

let () =
  run_async_with_log @@ fun () ->
  set_string_translations_exercises ();
  Learnocaml_local_storage.init () ;
  (* ---- launch everything --------------------------------------------- *)
  let toplevel_buttons_group = button_group () in
  disable_button_group toplevel_buttons_group (* enabled after init *) ;
  let toplevel_toolbar = find_component "learnocaml-exo-toplevel-toolbar" in
  let editor_toolbar = find_component "learnocaml-exo-editor-toolbar" in
  let nickname_div = find_component "learnocaml-nickname" in
  (match Learnocaml_local_storage.(retrieve nickname) with
   | nickname -> Manip.setInnerText nickname_div nickname
   | exception Not_found -> ());
  let toplevel_button =
    button ~container: toplevel_toolbar ~theme: "dark" ~group:toplevel_buttons_group ?state:None in
  let editor_button = button ~container: editor_toolbar ~theme: "light" in
  let id = match Url.Current.path with
    | "" :: "playground" :: p | "playground" :: p ->
        String.concat "/" (List.map Url.urldecode (List.filter ((<>) "") p))
    | _ -> arg "id"
  in
  Dom_html.document##.title :=
    Js.string (id ^ " - " ^ "Learn OCaml" ^" v."^ Learnocaml_api.version);
  let exercise_fetch =
    retrieve (Learnocaml_api.Playground id)
  in
  let after_init top =
    exercise_fetch >>= fun playground ->
    Learnocaml_toplevel.load ~print_outcome:true top
      ~message: [%i"loading the prelude..."]
      playground.Playground.prelude
    >>= fun r1 ->
    if not r1 then failwith [%i"error in prelude"] ;
    Learnocaml_toplevel.set_checking_environment top >>= fun () ->
    Lwt.return () in
  let toplevel_launch =
    toplevel_launch
      after_init
      select_tab
      toplevel_buttons_group
      id
  in
  init_tabs () ;
  toplevel_launch >>= fun top ->
  exercise_fetch >>= fun playground ->
  let solution =
    try Some (Learnocaml_local_storage.(retrieve (exercise_state id))).Answer.solution with
    | Not_found -> None in
  (* ---- toplevel pane ------------------------------------------------- *)
  init_toplevel_pane toplevel_launch top toplevel_buttons_group toplevel_button ;
  (* ---- editor pane --------------------------------------------------- *)
  let editor_pane = find_component "learnocaml-exo-editor-pane" in
  let editor = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div editor_pane) in
  let ace = Ocaml_mode.get_editor editor in
  Ace.set_contents ace ~reset_undo:true
    (match solution with
     | Some solution -> solution
     | None -> playground.Playground.template) ;
  Ace.set_font_size ace 18;
  begin editor_button
      ~icon: "cleanup" [%i"Reset"] @@ fun () ->
    confirm ~title:[%i"START FROM SCRATCH"]
      [H.pcdata [%i"This will discard all your edits. Are you sure?"]]
      (fun () ->
         Ace.set_contents ace playground.Playground.template);
    Lwt.return ()
  end ;
  begin editor_button
      ~icon: "download" [%i"Download"] @@ fun () ->
    let name = id ^ ".ml" in
    let contents = Js.string (Ace.get_contents ace) in
    Learnocaml_common.fake_download ~name ~contents ;
    Lwt.return ()
  end ;
  begin editor_button
      ~group: toplevel_buttons_group
      ~icon: "run" [%i"Eval code"] @@ fun () ->
    Learnocaml_toplevel.execute_phrase top (Ace.get_contents ace) >>= fun _ ->
    select_tab "toplevel";
    Lwt.return_unit
  end ;
  let typecheck set_class =
    Learnocaml_toplevel.check top (Ace.get_contents ace) >>= fun res ->
    let error, warnings =
      match res with
      | Toploop_results.Ok ((), warnings) -> None, warnings
      | Toploop_results.Error (err, warnings) -> Some err, warnings in
    let transl_loc { Toploop_results.loc_start ; loc_end } =
      { Ocaml_mode.loc_start ; loc_end } in
    let error = match error with
      | None -> None
      | Some { Toploop_results.locs ; msg ; if_highlight } ->
          Some { Ocaml_mode.locs = List.map transl_loc locs ;
                 msg = (if if_highlight <> "" then if_highlight else msg) } in
    let warnings =
      List.map
        (fun { Toploop_results.locs ; msg ; if_highlight } ->
           { Ocaml_mode.loc = transl_loc (List.hd locs) ;
             msg = (if if_highlight <> "" then if_highlight else msg) })
        warnings in
    Ocaml_mode.report_error ~set_class editor error warnings  >>= fun () ->
    Ace.focus ace ;
    Lwt.return () in
  (* ---- main toolbar -------------------------------------------------- *)
  let exo_toolbar = find_component "learnocaml-exo-toolbar" in
  let toolbar_button = button ~container: exo_toolbar ~theme: "light" in
  begin toolbar_button
      ~icon: "list" [%i"Playground"] @@ fun () ->
    Dom_html.window##.location##assign
      (Js.string "/index.html#activity=playground") ;
    Lwt.return ()
  end ;
  Window.onunload (fun _ev -> local_save ace id; true);
  (* ---- return -------------------------------------------------------- *)
  toplevel_launch >>= fun _ ->
  typecheck false >>= fun () ->
  hide_loading ~id:"learnocaml-exo-loading" () ;
  Lwt.return ()
