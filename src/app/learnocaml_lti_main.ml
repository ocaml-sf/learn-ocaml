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

let setup_csrf_token () =
  let csrf_input = Dom_html.getElementById "login-csrf-input" in
  match get_cookie "csrf" with
  | Some csrf -> csrf_input##setAttribute (Js.string "value") (Js.string csrf)
  | None -> ()

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
    ];
  setup_csrf_token ()
