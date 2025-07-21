(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2020 Alban Gruin
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_utils
open Js_of_ocaml
open Lwt
open Learnocaml_common

module H = Js_of_ocaml_tyxml.Tyxml_js.Html5

let id s = s, find_component s

(* XXX there is dead code among these variables *)
let login_overlay_id, login_overlay = id "login-overlay"

let login_direct_button_id, login_direct_button = id "login-direct-login"

let login_token_button_id, login_token_button = id "login-token-button"

let set_string_translations =
  List.iter
    (fun (id, text) ->
      Manip.setInnerHtml (find_component id) text)

let init_dialogs () =
  Manip.SetCss.display login_overlay "block"

let () =
  (match Js_utils.get_lang () with Some l -> Ocplib_i18n.set_lang l | None -> ());
  init_dialogs ();
  set_string_translations [
      "txt_direct_login_nickname", [%i"Choose a nickname"];
      "txt_direct_login", [%i"Direct login"];
      "txt_indirect_label", [%i"Or to be able to login independently of Moodle, \
                                you might want to setup a password below \
                                (or upgrade your account later)"];
      "txt_button_direct_login", [%i"Direct login"];
      "txt_token_returning", [%i"Connect"];
      "txt_returning_with_token", [%i"Reuse an account with a legacy token"];
      "txt_returning_token", [%i"Token"];
    ]
