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

let check_email_ml email =
  let regexp = Str.regexp Learnocaml_data.email_regexp_ml in
  Str.string_match regexp email 0

let id s = s, find_component s

let login_overlay_id, login_overlay = id "login-overlay"
let login_new_id, login_new = id "login-new"
let login_returning_id, login_returning = id "login-returning"

let reg_input_email_id, reg_input_email = id "register-email-input"
let reg_input_nick_id, reg_input_nick = id "register-nick-input"
let reg_input_password_id, reg_input_password = id "register-password-input"
let input_secret_id, input_secret = id "register-secret-input"
let input_consent_id, input_consent = id "first-connection-consent"
let login_new_button_id, login_new_button = id "login-new-button"

let login_email_input_id, login_email_input = id "login-email-input"
let login_password_input_id, login_password_input = id "login-password-input"
let login_csrf_input_id, login_csrf_input = id "login-csrf-input"
let login_id_input_id, login_id_input = id "login-id-input"
let login_hmac_input_id, login_hmac_input = id "login-hmac-input"
let login_connect_button_id, login_connect_button = id "login-connect-button"

let login_direct_button_id, login_direct_button = id "login-direct-login"

let set_string_translations =
  List.iter
    (fun (id, text) ->
      Manip.setInnerHtml (find_component id) text)

let send_sync_request () =
  let parameters =
    [("email", [Manip.value reg_input_email]);
     ("passwd", [Manip.value reg_input_password]);
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

let create_token () =
  let email = Manip.value reg_input_email and
      password = Manip.value reg_input_password and
      consent = Manip.checked input_consent and
      consent_label = find_component "txt_first_connection_consent" in
  (* 5 for a character, @, character, dot, character. *)
  let email_criteria = not (check_email_ml email) and
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
     Lwt.return_some (token, nickname))

let init_dialogs () =
  Manip.SetCss.display login_overlay "block";
  Manip.Ev.onclick login_new_button (fun _ ->
      Lwt.async (fun _ ->
          create_token () >>= function
          | Some (_token, _nickname) ->
             send_sync_request ();
             Dom_html.window##.location##assign (Js.string "/");
             Lwt.return ()
          | None -> Lwt.return_unit);
      true)

let () =
  (match Js_utils.get_lang () with Some l -> Ocplib_i18n.set_lang l | None -> ());
  init_dialogs ();
  set_string_translations [
      "txt_first_connection", [%i"First connection"];
      "txt_first_connection_email", [%i"E-mail address"];
      "txt_first_connection_nickname", [%i"Nickname"];
      "txt_first_connection_password", [%i"Password"];
      "txt_first_connection_secret", [%i"Enter the secret"];
      "txt_secret_label", [%i"The secret is the passphrase provided by \
                            your teacher to sign-up."];
      "txt_first_connection_consent", [%i"By submitting this form, I accept that the \
                                          information entered will be used in the \
                                          context of the Learn-OCaml plateform."];
      "txt_login_new", [%i"Create new token"];
      "txt_returning", [%i"Returning user"];
      "txt_returning_email", [%i"E-mail address"];
      "txt_returning_password", [%i"Password"];
      "txt_login_returning", [%i"Connect"];
      "txt_login_forgotten", [%i"Forgot your password?"];
      "txt_direct_login", [%i"Direct login"];
      "txt_button_direct_login", [%i"Direct login"];
    ]
