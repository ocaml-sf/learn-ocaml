(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2020 Alban Gruin
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_utils
open Lwt
open Learnocaml_common
open Learnocaml_api

module El = struct
  let id s = s, find_component s
  module Login_overlay = struct
    let login_overlay_id, login_overlay = id "login-overlay"
    let login_new_id, login_new = id "login-new"

    let upgrade_email_id, upgrade_email = id "upgrade-email-input"
    let upgrade_password_id, upgrade_password = id "upgrade-password-input"
    let upgrade_confirmation_id, upgrade_confirmation = id "upgrade-confirmation-input"
    let upgrade_button_id, upgrade_button = id "upgrade-button"
  end
end

let check_email_js email =
  let re = Regexp.regexp Learnocaml_data.email_regexp_js in
  Learnocaml_data.email_check_length email
  && match Regexp.string_match re email 0 with
     | Some _ -> true
     | None -> false

let init_token_dialog () =
  let open El.Login_overlay in
  Manip.SetCss.display login_overlay "block";
  let got_token = match Lwt.task () with
    |(_,got_tok) -> got_tok in
  let create_token () =
    let email = Manip.value upgrade_email and
        password = Manip.value upgrade_password and
        password_confirmation = Manip.value upgrade_confirmation in
    let email_criteria = not (check_email_js email) and
        passwd_crit1 = not (Learnocaml_data.passwd_check_length password) and
        passwd_crit2 = not (Learnocaml_data.passwd_check_strength password) and
        passwd_crit3 = not (password = password_confirmation) in
    Manip.SetCss.borderColor upgrade_email "";
    Manip.SetCss.borderColor upgrade_password "";
    Manip.SetCss.borderColor upgrade_confirmation "";
    if email_criteria || passwd_crit1 || passwd_crit2 || passwd_crit3 then
      begin
        if email_criteria then
          Manip.SetCss.borderColor upgrade_email "#f44";
        if passwd_crit1 || passwd_crit2 then
          Manip.SetCss.borderColor upgrade_password "#f44";
        if passwd_crit3 then
          Manip.SetCss.borderColor upgrade_confirmation "#f44";
        if email_criteria then begin
            cb_alert ~title:[%i"ERROR"]
              [%i"The entered e-mail was invalid."]
              (fun () -> Manip.focus upgrade_email)
          end
        else if passwd_crit1 then begin
            cb_alert ~title:[%i"ERROR"]
              [%i"Password must be at least 8 characters long"]
              (fun () -> Manip.focus upgrade_password)
          end
        else if passwd_crit2 then begin
            cb_alert ~title:[%i"ERROR"]
              [%i"Password must contain at least one digit, \
                  one lower and upper letter, \
                  and one non-alphanumeric char."]
              (fun () -> Manip.focus upgrade_password)
          end
        else if passwd_crit3 then begin
            cb_alert ~title:[%i"ERROR"]
              [%i"The password and its confirmation are not the same"]
              (fun () -> Manip.focus upgrade_confirmation)
          end;
        Lwt.return_none
      end
    else
      let token = Learnocaml_data.Token.to_string (Learnocaml_local_storage.(retrieve sync_token)) in
      retrieve (Learnocaml_api.Upgrade
                  ("email="^email^"&passwd="^password^"&token="^token))
                  (*body exemple ->
                  email=&passwd=&confirmation=&csrf=Bfkxd/2TjpMAkq4bFGIs1hp9oxeBTZIKioMlQMUDlpk=&token=ZGB-GDD-SNB-41M*)
      >>= fun _ -> cb_alert ~title:[%i"VALIDATION REQUIRED"]
                     [%i"A confirmation e-mail has been sent to your address."]
                     Js_utils.reload;
                   Lwt.return_none
  in
  let handler f t = fun _ ->
    Lwt.async (fun () ->
        f () >|= function
        | Some token -> Lwt.wakeup got_token token
        | None -> ());
    t
  in
  Manip.Ev.onclick upgrade_button (handler create_token false)

let set_string_translations =
  List.iter
    (fun (id, text) ->
      Manip.setInnerHtml (find_component id) text)

let () =
  (match Js_utils.get_lang () with Some l -> Ocplib_i18n.set_lang l | None -> ());
  try
    Manip.SetCss.display (find_component "login-overlay") "block";
    set_string_translations [
        "txt_password_length", [%i"Password must be at least 8 characters long"];
        "txt_password_strength", [%i"Password must contain at least one digit, \
                                     one lower and upper letter, \
                                     and one non-alphanumeric char."];
        "txt_upgrade", [%i"Setup a password"];
        "txt_upgrade_email", [%i"E-mail address"];
        "txt_upgrade_password", [%i"Password"];
        "txt_upgrade_password_confirmation", [%i"Confirm password"];
        "txt_do_upgrade", [%i"Upgrade"];
        "txt_info", [%i"An e-mail will be sent to your address to confirm it."];
      ];
    init_token_dialog ()
  with Not_found ->
    Learnocaml_common.alert ~title:[%i"NO TOKEN"] [%i"You are not logged in"]
