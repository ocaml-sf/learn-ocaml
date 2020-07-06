(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2020 Alban Gruin
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_utils
open Learnocaml_common

let id s = s, find_component s

let login_overlay_id, login_overlay = id "login-overlay"
let login_new_id, login_new = id "login-new"
let login_returning_id, login_returning = id "login-returning"
let button_yes_id, button_yes = id "first-connection-yes"
let button_no_id, button_no = id "first-connection-no"

let set_string_translations =
  List.iter
    (fun (id, text) ->
      Manip.setInnerHtml (find_component id) text)

let init_dialogs () =
  hide login_returning;
  Manip.SetCss.display login_overlay "block";
  Manip.Ev.onclick button_yes (fun _ ->
      hide login_new;
      Manip.SetCss.display login_returning "block";
      true);
  Manip.Ev.onclick button_no (fun _ ->
      Dom_html.window##.location##assign (Js.string "/");
      true)

let () =
  (match Js_utils.get_lang () with Some l -> Ocplib_i18n.set_lang l | None -> ());
  init_dialogs ();
  set_string_translations [
      "txt_first_connection_dialog", [%i"First connection"];
      "txt_first_connection_question", [%i"Do you have a Learn OCaml account?"];
      "txt_button_yes", [%i"Yes"];
      "txt_button_no", [%i"No"];
      "txt_returning_token", [%i"Enter your token"];
      "txt_returning_token_label", [%i"Token"];
      "txt_button_connect", [%i"Connect"]
    ]
