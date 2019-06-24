open Js_of_ocaml
open Dom_html
open Js_utils
open Learnocaml_common
open Learnocaml_exercise_state
open Learnocaml_index
open Editor_lib

module StringMap = Map.Make (String)

(* Internationalization *)
let () = Translate.set_lang ()
let () =
  let translations = [
    "txt_new_exo", [%i"New exercise"];
    "txt_id", [%i"Unique identifier:<br>"];
    "txt_title", [%i"Title (unique too):<br>"];
    "txt_descr", [%i"Description of the exercise:<br>"];
    "txt_diff", [%i"Difficulty level:<br>"];
    "cancel", [%i"Cancel"];
    "save", [%i"Save"];
  ] in
  Translate.set_string_translations translations



let getString = function
  | None -> failwith "incorrect_input"
  | Some input -> Js.to_string input##.value
let getStringOpt = function
  | None -> None
  | Some input -> Some (Js.to_string input##.value)
let getFloatOpt = function
  | None -> None
  | Some input -> float_of_string_opt (Js.to_string input##.value)


let previous_id = match (arg "id") with
  | exception Not_found -> ""
  | s -> s
let save = getElementById "save"
let identifier = getElementById_coerce "identifier" CoerceTo.input
let title = getElementById_coerce "title" CoerceTo.input
let description = getElementById_coerce "description" CoerceTo.textarea
let difficulty = getElementById_coerce "difficulty" CoerceTo.select
let incipit = ""
let checkbox = {undesirable = false; imperative = false}
let solution, question, template, test,
    previous_title, previous_diff, prelude, prepare =
  match Learnocaml_local_storage.(retrieve (editor_state previous_id)) with
  | exception Not_found ->
     "", "", "", {testml = ""; testhaut = IntMap.empty}, "", 0., "", ""
  | {Learnocaml_exercise_state.metadata;
     solution; question; template; test; mtime; prelude; prepare} ->
     solution, question, template, test,
     metadata.titre, metadata.diff, prelude, prepare


let () = match identifier with
  | None -> ()
  | Some input -> input##.value := Js.string previous_id
let () = match title with
  | None -> ()
  | Some input -> input##.value := Js.string previous_title
let previous_descr =
  let exos = Learnocaml_local_storage.(retrieve (index_state "index")).exos in
  let exo =
    match (StringMap.find_opt previous_id exos) with
    | None -> {exercise_kind = Learnocaml_exercise; exercise_title = "";
               exercise_short_description = None; exercise_stars = 0.}
    | Some s -> s
  in exo.exercise_short_description
let () = match previous_descr with
  | Some d -> setInnerHtml (getElementById "description") d
  | None -> ()
let () = match difficulty with
  | None -> ()
  | Some select -> select##.value := Js.string (string_of_float previous_diff)


let resultOptionToBool = function
  | None -> false
  | Some _ -> true
let isIdCorrect s =
  resultOptionToBool (Regexp.string_match (Regexp.regexp "^[a-z0-9_-]+$") s 0)
let isTitleCorrect s =
  (resultOptionToBool (Regexp.string_match (Regexp.regexp "^[^ \t]") s 0)) &&
    (resultOptionToBool (Regexp.string_match (Regexp.regexp ".*[^ \t]$") s 0))


let store id titre description diff =
  let metadata = {id; titre; description; diff} in
  if previous_id <> "" then
    Learnocaml_local_storage.(delete (editor_state previous_id));
  Learnocaml_local_storage.(store (editor_state id))
    {Learnocaml_exercise_state.metadata; solution; incipit; question; template;
     test; prelude; prepare; checkbox; mtime = gettimeofday ()};
  store_in_index metadata;;


let id_error = getElementById "id_error"
let title_error = getElementById "title_error"


let () = save##.onclick := handler (fun _ ->
  let id = getString identifier
  and titre = getString title
  and description = getString description
  and diff = match getFloatOpt difficulty with
    | None -> 0.
    | Some x -> x in
  let id_correct = isIdCorrect id
  and id_unique = idUnique id
  and title_correct = isTitleCorrect titre
  and title_unique = titleUnique titre in
  (if not id_correct then
    setInnerHtml id_error [%i"Incorrect identifier: an identifier \
                              can't be empty, \
                              and only lower case letters, numerals, dashes \
                              and underscores are allowed"]
  else if not id_unique then
    setInnerHtml id_error [%i"This identifier is already used, \
                              please choose another one"]
  else
    setInnerHtml id_error "");
  (if not title_correct then
    setInnerHtml title_error [%i"Incorrect title: a title can't be empty, \
                                 or begin or end with a space or a tab"]
  else if not title_unique then
     setInnerHtml title_error
       [%i"This title is already used, please choose another one"]
  else
    setInnerHtml title_error "");
  if id_correct && title_correct && id_unique && title_unique then
    begin
      store id titre description diff;
      Dom_html.window##.location##assign
        (Js.string ("editor.html#id=" ^ id ^ "&action=open"));
    end;
  Js._true
)
