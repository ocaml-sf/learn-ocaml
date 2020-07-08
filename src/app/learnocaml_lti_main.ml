(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2020 Alban Gruin
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_utils
open Lwt
open Learnocaml_data
open Learnocaml_common

module H = Tyxml_js.Html5

let id s = s, find_component s

let login_overlay_id, login_overlay = id "login-overlay"
let login_question_id, login_question = id "login-question"
let login_new_id, login_new = id "login-new"
let login_returning_id, login_returning = id "login-returning"
let button_yes_id, button_yes = id "first-connection-yes"
let button_no_id, button_no = id "first-connection-no"

let login_nickname_input_id, login_nickname_input = id "login-nickname-input"
let login_secret_input_id, login_secret_input = id "login-secret-input"
let login_new_button_id, login_new_button = id "login-new-button"

let login_csrf_input_id, login_csrf_input = id "login-csrf-input"
let login_id_input_id, login_id_input = id "login-id-input"
let login_hmac_input_id, login_hmac_input = id "login-hmac-input"

let set_string_translations =
  List.iter
    (fun (id, text) ->
      Manip.setInnerHtml (find_component id) text)

let send_sync_request token =
  let parameters =
    [("token", [Token.to_string token]);
     ("csrf", [Manip.value login_csrf_input]);
     ("user-id", [Manip.value login_id_input]);
     ("hmac", [Manip.value login_hmac_input])]
    |> Uri.encoded_of_query |> Js.string |> Js.some in
  let request = Js_of_ocaml.XmlHttpRequest.create () in
  request##(_open (Js.string "POST") (Js.string "/launch/login") (Js._false));
  request##(setRequestHeader (Js.string "Content-type")
              (Js.string "application/x-www-form-urlencoded"));
  request##(send parameters);
  if request##.status = 200 then
    Ok ()
  else
    Error ()

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
  let close_button = H.button ~a: [
                         H.a_onclick (fun _ ->
                             send_sync_request token;
                             Dom_html.window##.location##assign (Js.string "/");
                             false
                           )
                       ] [ H.pcdata [%i"OK"] ] in
  let buttons = Some([close_button]) in
  ext_alert ~title:[%i"Your Learn-OCaml token"] ?buttons [
    H.p [H.pcdata [%i"Your token is displayed below. It identifies you and \
                      allows to share your workspace between devices."]];
    H.p [H.pcdata [%i"Please write it down."]];
    H.div ~a:[H.a_style "text-align: center;"] [token_disp_div token];
  ]

let create_token () =
  let nickname = String.trim (Manip.value login_nickname_input) in
  if Token.check nickname || String.length nickname < 2 then
    (Manip.SetCss.borderColor login_nickname_input "#f44";
     Lwt.return_none)
  else
    let secret = Sha.sha512 (String.trim (Manip.value login_secret_input)) in
    retrieve (Learnocaml_api.Nonce ())
    >>= fun nonce ->
    let secret = Sha.sha512 (nonce ^ secret) in
    (Learnocaml_local_storage.(store nickname) nickname;
     retrieve
       (Learnocaml_api.Create_token (secret, None, Some nickname))
     >>= fun token ->
     Learnocaml_local_storage.(store sync_token) token;
     show_token_dialog token;
     Lwt.return_some (token, nickname))

let init_dialogs () =
  hide login_new;
  hide login_returning;
  Manip.SetCss.display login_overlay "block";
  Manip.Ev.onclick button_yes (fun _ ->
      hide login_question;
      Manip.SetCss.display login_returning "block";
      true);
  Manip.Ev.onclick button_no (fun _ ->
      hide login_question;
      Manip.SetCss.display login_new "block";
      true);
  Manip.Ev.onclick login_new_button (fun _ ->
      Lwt.async (fun _ ->
          create_token () >>= function
          | Some (token, _nickname) ->
             Lwt.return (show_token_dialog token)
          | None -> Lwt.return_unit);
      true)

let try_stored_token () =
  try
    send_sync_request Learnocaml_local_storage.(retrieve sync_token)
  with Not_found ->
    Error ()

let () =
  (match Js_utils.get_lang () with Some l -> Ocplib_i18n.set_lang l | None -> ());
  match try_stored_token () with
  | Ok () -> Dom_html.window##.location##assign (Js.string "/")
  | Error () ->
     init_dialogs ();
     set_string_translations [
         "txt_dialog", [%i"First connection"];
         "txt_question", [%i"Do you have a Learn OCaml account?"];
         "txt_button_yes", [%i"Yes"];
         "txt_button_no", [%i"No"];
         "txt_first_connection", [%i"First connection"];
         "txt_first_connection_dialog", [%i"Choose a nickname"];
         "txt_first_connection_secret", [%i"Enter the secret"];
         "txt_login_new", [%i"Create new token"];
         "txt_returning_token", [%i"Enter your token"];
         "txt_returning_token_label", [%i"Token"];
         "txt_button_connect", [%i"Connect"]
       ]
