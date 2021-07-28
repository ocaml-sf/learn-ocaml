(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml
open Js_utils
open Lwt
open Learnocaml_data
open Learnocaml_common
module H = Tyxml_js.Html5
module React = Lwt_react

let find_tab name = find_component ("learnocaml-exo-tab-" ^ name)

let tab_select_signal, init_tab, select_tab =
  let init_tab, select_tab = mk_tab_handlers "details" ["list"; "answer"] in
  let tab_select_signal, tab_select_signal_set = React.S.create "details" in
  let select_tab str = tab_select_signal_set str; select_tab str in
  (tab_select_signal, init_tab, select_tab)

let update_answer_tab, clear_answer_tab = ace_display (find_tab "answer")

let selected_class_signal, set_selected_class = React.S.create None

let selected_repr_signal, set_selected_repr = React.S.create None

let open_tok tok =
  let _win = window_open ("/student-view.html?token=" ^ tok) "_blank" in
  false

let list_of_tok =
  List.map
  @@ fun x ->
  let tok = Token.to_string x in
  H.a ~a:[H.a_onclick (fun _ -> open_tok tok)] [H.txt (tok ^ " ")]

let rec render_tree =
  let open Asak.Wtree in
  function
  | Leaf xs ->
      [ H.p
          ~a:
            [ H.a_onclick (fun _ ->
                  set_selected_class (Some xs);
                  true ) ]
          [ H.txt
              ("Leaf with " ^ string_of_int (List.length xs) ^ " student(s)")
          ] ]
  | Node (f, l, r) ->
      [ H.p [H.txt ("Node " ^ string_of_int f)]
      ; H.ul [H.li (render_tree l); H.li (render_tree r)] ]

let weight_of_tree t = Asak.Wtree.fold_tree (fun _ -> ( + )) t

let render_trees xs =
  let aux (i, acc) t =
    let str =
      "Class n°" ^ string_of_int i ^ " ("
      ^ string_of_int (weight_of_tree List.length t)
      ^ " students)"
    in
    (i + 1, H.li (H.txt str :: render_tree t) :: acc)
  in
  List.rev @@ snd @@ List.fold_left aux (1, []) xs

let render_classes xs =
  let aux (grade, values) acc =
    let str = string_of_int grade ^ "pts :" in
    H.p [H.txt str] :: H.ul (render_trees values) :: acc
  in
  List.fold_right aux xs []

let sum_with f = List.fold_left (fun acc x -> acc + f x) 0

let exercises_tab part =
  let open Partition in
  let not_graded =
    string_of_int (List.length part.not_graded) ^ " codes were not graded: "
  in
  let bad_type =
    string_of_int (List.length part.bad_type) ^ " codes had the wrong type: "
  in
  let total_sum =
    let s =
      sum_with
        (fun (_, x) -> sum_with (fun x -> weight_of_tree List.length x) x)
        part.partition_by_grade
    in
    string_of_int s ^ " codes implemented the function with the right type."
  in
  H.p (H.txt not_graded :: list_of_tok part.not_graded)
  :: H.p (H.txt bad_type :: list_of_tok part.bad_type)
  :: H.p [H.txt total_sum]
  :: render_classes part.partition_by_grade

let _class_selection_updater =
  let previous = ref None in
  let of_repr repr = [H.code [H.txt repr]] in
  let onclick p tok repr =
    H.a_onclick
    @@ fun _ ->
    ( match !previous with
    | None -> ()
    | Some prev -> Manip.replaceChildren prev [] );
    previous := Some p;
    Manip.replaceChildren p (of_repr repr);
    set_selected_repr (Some (tok, repr));
    true
  in
  let to_li tok repr p =
    let strtok = Token.to_string tok in
    H.li
      ~a:[onclick p tok repr; H.a_ondblclick (fun _ -> open_tok strtok)]
      [H.txt strtok; p]
  in
  let mkfirst (tok, repr) =
    let p = H.p (of_repr repr) in
    previous := Some p;
    to_li tok repr p
  in
  let mkelem (tok, repr) = to_li tok repr @@ H.p [] in
  selected_class_signal
  |> React.S.map
     @@ fun id ->
     match id with
     | None -> ()
     | Some xs ->
         set_selected_repr (Some (List.hd xs));
         Manip.replaceChildren (find_tab "details")
           [H.ul @@ (mkfirst (List.hd xs) :: List.map mkelem (List.tl xs))]

let main () =
  Learnocaml_local_storage.init ();
  ( match Js_utils.get_lang () with
  | Some l -> Ocplib_i18n.set_lang l
  | None -> () );
  set_string_translations_view ();
  let teacher_token = Learnocaml_local_storage.(retrieve sync_token) in
  if not (Token.is_teacher teacher_token) then
    (* No security here: it's client-side, and we don't check that the token is
       registered server-side *)
    failwith "The page you are trying to access is for teachers only";
  let exercise_id =
    try List.assoc "id" Url.Current.arguments with Not_found ->
      failwith "Exercise id is missing"
  in
  let fun_id =
    try List.assoc "function" Url.Current.arguments with Not_found ->
      failwith "function name is missing"
  in
  let prof =
    try int_of_string @@ List.assoc "prof" Url.Current.arguments with
    | Not_found | Failure _ -> failwith "prof is missing or malformed"
  in
  let update_repr_code = function
    | None -> true
    | Some (tok, _) ->
        Lwt.async (fun () ->
            retrieve (Learnocaml_api.Fetch_save tok)
            >|= fun save ->
            match SMap.find_opt exercise_id save.Save.all_exercise_states with
            | None -> ()
            | Some x -> update_answer_tab x.Answer.solution );
        true
  in
  let _repr_selection_updater =
    selected_repr_signal
    |> React.S.map
       @@ fun id ->
       let tab = React.S.value tab_select_signal in
       if tab = "answer" then update_repr_code id else true
  in
  retrieve
    (Learnocaml_api.Partition (teacher_token, exercise_id, fun_id, prof))
  >>= fun part ->
  hide_loading ~id:"learnocaml-exo-loading" ();
  Manip.replaceChildren (find_tab "list") (exercises_tab part);
  init_tab ();
  Manip.Ev.onclick (find_component "learnocaml-exo-button-answer") (fun _ ->
      select_tab "answer";
      update_repr_code (React.S.value selected_repr_signal) );
  Lwt.return_unit

let () = run_async_with_log main
