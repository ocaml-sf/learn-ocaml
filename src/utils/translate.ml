open Js_utils
open Learnocaml_common

let set_lang () =
  match Js.Optdef.to_option (Dom_html.window##.navigator##.language) with
  | Some l -> Ocplib_i18n.set_lang (Js.to_string l)
  | None ->
     (match Js.Optdef.to_option (Dom_html.window##.navigator##.userLanguage) with
      | Some l -> Ocplib_i18n.set_lang (Js.to_string l)
      | None -> ())

let set_string_translations translations =
  List.iter
    (fun (id, text) ->
       Manip.setInnerHtml (find_component id) text)
    translations

let set_title_translations translations =
  List.iter
  (fun (id, text) -> Manip.setTitle (find_component id) text)
  translations
