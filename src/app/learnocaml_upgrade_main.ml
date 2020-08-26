(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2020 Alban Gruin
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_utils
open Learnocaml_common

let set_string_translations =
  List.iter
    (fun (id, text) ->
      Manip.setInnerHtml (find_component id) text)

let () =
  (match Js_utils.get_lang () with Some l -> Ocplib_i18n.set_lang l | None -> ());
  try
    Manip.SetCss.display (find_component "login-overlay") "block";
    set_string_translations [
        "txt_upgrade", [%i"Upgrade account"];
        "txt_upgrade_email", [%i"E-mail address"];
        "txt_upgrade_password", [%i"Password"];
        "txt_do_upgrade", [%i"Upgrade"];
        "txt_info", [%i"An e-mail will be sent to your address to confirm it."];
      ]
  with Not_found ->
    Learnocaml_common.alert ~title:[%i"NO TOKEN"] [%i"You are not logged in"]
