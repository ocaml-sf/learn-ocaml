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
  Manip.SetCss.display (find_component "login-overlay") "block";
  set_string_translations [
      "txt_password_length", [%i"Password must be at least 8 characters long"];
      "txt_password_strength", [%i"Password must contain at least one digit, \
                                   one lower and upper letter, \
                                   and one non-alphanumeric char."];
      "txt_passwd_reset", [%i"Reset password"];
      "txt_new_passwd", [%i"New password"];
      "txt_submit", [%i"Submit"]
    ]
