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

open Js_of_ocaml
open Js_of_ocaml_tyxml
open Js_utils
open Lwt.Infix
open Learnocaml_common
open Learnocaml_data

module H = Tyxml_js.Html

module Ids = struct
  let editor_pane = "learnocaml-exo-tab-editor"
  let toplevel_pane = "learnocaml-exo-tab-toplevel"
end

let tabs = [ Ids.editor_pane; Ids.toplevel_pane ]

let select_tab id =
  Manip.focus (find_component id)

let display_list ?(sep=Tyxml_js.Html5.txt ", ") l =
  let open Tyxml_js.Html5 in
  let rec gen acc = function
    | [] -> [ txt "" ]
    | a :: [] -> a :: acc
    | a :: ((_ :: _) as rem) ->
        gen (sep :: (a  :: acc)) rem
  in
  gen [] l |> List.rev

let set_string_translations () =
  let translations = [
    "txt_preparing", [%i"Preparing the environment"];
    (* "learnocaml-exo-button-editor", [%i"Editor"];
     * "learnocaml-exo-button-toplevel", [%i"Toplevel"]; *)
    "learnocaml-exo-editor-pane", [%i"Editor"];
  ] in
  List.iter
    (fun (id, text) ->
       Manip.setInnerHtml (find_component id) text)
    translations

let local_save ace id =
  let key = Learnocaml_local_storage.exercise_state id in
  let ans =
    try Learnocaml_local_storage.retrieve key with Not_found ->
      Answer.{solution = ""; mtime = 0.; report = None; grade = None}
  in
  Learnocaml_local_storage.store key
    { ans with Answer.solution = Ace.get_contents ace;
               mtime = gettimeofday () }


let editor_placeholder_text =
  [%i "(* This is an OCaml editor. Enter your program here and send it to the \
       toplevel \n\
      \   using the \"Eval code\" button. *)\n\n"]

let () =
  log "GO";
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
  log "TRANSL";
  set_string_translations ();
  log "INIT STORAGE";
  Learnocaml_local_storage.init () ;
  (* ---- launch everything --------------------------------------------- *)
  let toplevel_buttons_group = button_group () in
  disable_button_group toplevel_buttons_group (* enabled after init *) ;
  let toplevel_toolbar = find_component "learnocaml-exo-toplevel-toolbar" in
  let editor_toolbar = find_component "learnocaml-exo-editor-toolbar" in
  let toplevel_button = button ~container: toplevel_toolbar ~theme: "dark" in
  let editor_button = button ~container: editor_toolbar ~theme: "dark" in
  let id = match Url.Current.path with
    | "" :: "exercises" :: p | "exercises" :: p ->
        String.concat "/" (List.map Url.urldecode (List.filter ((<>) "") p))
    | _ -> try arg "id" with Not_found -> "sandbox"
  in
  Dom_html.document##.title :=
    Js.string ("TryOCaml");
  let after_init top =
    Learnocaml_toplevel.set_checking_environment top
  in
  let timeout_prompt =
    Learnocaml_toplevel.make_timeout_popup
      ~on_show: (fun () -> select_tab Ids.toplevel_pane)
      () in
  let flood_prompt =
    Learnocaml_toplevel.make_flood_popup
      ~on_show: (fun () -> select_tab Ids.toplevel_pane)
      () in
  let history =
    let storage_key =
      Learnocaml_local_storage.exercise_toplevel_history id in
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
  let toplevel_launch =
    create_toplevel
      ~after_init ~timeout_prompt ~flood_prompt
      ~on_disable_input: (fun _ -> disable_button_group toplevel_buttons_group)
      ~on_enable_input: (fun _ -> enable_button_group toplevel_buttons_group)
      ~container:(find_component "learnocaml-exo-toplevel-pane")
      ~history () in
  log "init_tabs";
  log "toplevel launch";
  toplevel_launch >>= fun top ->
  let solution =
    match Learnocaml_local_storage.(retrieve (exercise_state id)) with
    | { Answer.solution ; _ } -> Some solution
    | exception Not_found -> None in
  (* ---- toplevel pane ------------------------------------------------- *)
  log "button";
  begin toplevel_button
      ~group: toplevel_buttons_group
      ~icon: "cleanup" [%i"Clear"] @@ fun () ->
    Learnocaml_toplevel.clear top ;
    Lwt.return ()
  end ;
  begin toplevel_button
      ~icon: "reload" [%i"Reset"] @@ fun () ->
    toplevel_launch >>= fun top ->
    disabling_button_group toplevel_buttons_group (fun () -> Learnocaml_toplevel.reset top)
  end ;
  begin toplevel_button
      ~group: toplevel_buttons_group
      ~icon: "run" [%i"Eval phrase"] @@ fun () ->
    Learnocaml_toplevel.execute top ;
    Lwt.return ()
  end ;
  (* ---- editor pane --------------------------------------------------- *)
  log "editor";
  let editor_pane = find_component "learnocaml-exo-editor-pane" in
  let editor = Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div editor_pane) in
  let ace = Ocaml_mode.get_editor editor in
  Ace.set_contents ace ~reset_undo:true
    (match solution with
     | Some solution -> solution
     | None -> editor_placeholder_text) ;
  Ace.set_font_size ace 18;
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
  begin editor_button
      ~icon: "cleanup" [%i"Clear"] @@ fun () ->
    confirm ~title:[%i"CLEAR FILE"]
      [H.txt [%i"This will discard all your edits. Are you sure?"]]
      (fun () -> Ace.set_contents ace "");
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
      ~icon: "typecheck" [%i"Check"] @@ fun () ->
    typecheck true
  end;
  begin editor_button
      ~group: toplevel_buttons_group
      ~icon: "run" [%i"Eval code"] @@ fun () ->
    Learnocaml_toplevel.execute_phrase top (Ace.get_contents ace) >>= fun _ ->
    select_tab Ids.toplevel_pane;
    Lwt.return_unit
  end ;
  Window.onunload (fun _ev -> local_save ace id; true);
  (* ---- return -------------------------------------------------------- *)
  log "RUN";
  toplevel_launch >>= fun _ ->
  typecheck false >>= fun () ->
  hide_loading ~id:"learnocaml-exo-loading" () ;
  Lwt.return ()
