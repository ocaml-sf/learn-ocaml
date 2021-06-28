(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2020 Alban Gruin
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Learnocaml_common

let rec drop_2_trailing = function
  | [] | [_] | [_; _] -> []
  | x :: l -> x :: drop_2_trailing l

(* Replace location: from [http://localhost:8080/confirm/...handle...]
   to [http://localhost:8080] *)
let redirect () =
  let open Js_of_ocaml__Url in
  match Url.Current.get () with
  | Some (Http http_url) ->
     let new_url = {http_url with hu_path = drop_2_trailing http_url.hu_path} in
     Url.Current.set (Http new_url)
  | Some (Https http_url) ->
     let new_url = {http_url with hu_path = drop_2_trailing http_url.hu_path} in
     Url.Current.set (Https new_url)
  | Some _ | None -> ()

let () =
  (match Js_utils.get_lang () with Some l -> Ocplib_i18n.set_lang l | None -> ());
  let message =
    [%i"Your e-mail address has been confirmed. You can now login."] in
  cb_alert ~title:[%i"EMAIL CONFIRMED"] message redirect
