(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2023 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml
open Js_of_ocaml_tyxml
open Js_utils
open Lwt
open Learnocaml_data
open Learnocaml_common

module H = Tyxml_js.Html5
module React = Lwt_react

let find_tab name = (find_component ("learnocaml-exo-tab-" ^ name))

let tab_select_signal, init_tab, select_tab =
  let init_tab, select_tab = mk_tab_handlers "details" ["list"; "answer"] in
  let tab_select_signal, tab_select_signal_set =
    React.S.create "details" in
  let select_tab str = tab_select_signal_set str; select_tab str in
  tab_select_signal, init_tab, select_tab

let update_answer_tab, clear_answer_tab = ace_display (find_tab "answer")

let selected_class_signal, set_selected_class = React.S.create None
let selected_repr_signal, set_selected_repr   = React.S.create None

let students_map = ref Token.Map.empty
let anon_id_map = ref Token.Map.empty
let partition = ref None

let open_tok tok =
  let _win = window_open ("/student-view.html?token="^tok) "_blank" in
  false

let rec render_tree =
  let open Asak.Wtree in
  function
  | Leaf xs ->
     [H.p ~a:[ H.a_onclick (fun _ ->
                   set_selected_class (Some xs); true)]
        [H.txt ("Leaf with " ^ string_of_int (List.length xs) ^ " student(s)")]]
  | Node (f,l,r) ->
     [
       H.p [ H.txt ("Node " ^ string_of_int f) ]
     ; H.ul [
            H.li (render_tree l)
          ; H.li (render_tree r)
          ]
     ]

let weight_of_tree t = Asak.Wtree.fold_tree (fun _ -> ( + )) t

let render_trees xs =
  let aux (i,acc) t =
    let str =
      "Class n°" ^ string_of_int i
      ^ " (" ^ string_of_int (weight_of_tree List.length t) ^ " students)" in
    i+1,
    H.li
      ( H.txt str
      :: render_tree t)
    :: acc
  in
  List.rev @@ snd @@ List.fold_left aux (1,[]) xs

let render_classes xs =
  let aux (grade,values) acc =
    let str = string_of_int grade ^ "pts :" in
    H.p [H.txt str] ::
      H.ul (render_trees values) ::
        acc
  in List.fold_right aux xs []

let sum_with f = List.fold_left (fun acc x -> acc + f x) 0

let students_of_tokens tokens =
  List.map (fun t -> Token.Map.find t !students_map) tokens

(* implements the following, minor feature:
   give a unique number foreach student involved in the partition *)
let generate_anon_from_part () =
  let open Partition in
  let part = Option.get !partition in
  let num = ref 0 in
  let count tokens =
    let new_map =
      List.fold_left (fun res st -> incr num; Token.Map.add st !num res)
        !anon_id_map tokens in
    anon_id_map := new_map in
  count part.not_graded;
  count part.bad_type;
  let rec wcount = function
    | Asak.Wtree.Node (_i, wa, wb) -> wcount wa; wcount wb
    | Asak.Wtree.Leaf list_pair_tok -> count (List.map fst list_pair_tok) in
  part.partition_by_grade |> List.iter @@ fun (_grade, list) ->
    list |> List.iter @@ wcount

let anon_id_of_student st =
  string_of_int @@ Token.Map.find st.Student.token !anon_id_map

let nickname_of_student st =
  Option.value st.Student.nickname ~default:("Nick" ^ " N/A")

let list_of_students_details () =
  let open Partition in
  let part = Option.get !partition in
  let not_graded_students = students_of_tokens part.not_graded in
  let bad_type_students = students_of_tokens part.bad_type in
  let create_div students_list =
    students_list |>
      List.concat_map @@
        fun st ->
        let anon = anon_id_of_student st in
        let nick = nickname_of_student st in
        let tok = Token.to_string st.Student.token in
        [H.span ~a:[
             H.a_ondblclick (fun _ -> open_tok tok);
             H.a_class ["student"];
             H.a_user_data "anon" ("Student " ^ anon);
             H.a_user_data "nickname" nick;
             H.a_user_data "token" tok;
           ] [];
         H.txt " "] in
  (create_div not_graded_students,
   create_div bad_type_students)

let exercises_tab () =
  let part = Option.get !partition in
  let open Partition in
  let not_graded =
    string_of_int (List.length part.not_graded)
    ^ " codes were not graded: " in
  let bad_type =
       string_of_int (List.length part.bad_type)
    ^ " codes had the wrong type: " in
  let total_sum =
    let s =
      sum_with
        (fun (_,x) ->
          sum_with (fun x -> weight_of_tree List.length x)
            x
        )
        part.partition_by_grade in
    string_of_int s
    ^ " codes implemented the function with the right type." in
  let (not_graded_students, bad_type_students) =
    list_of_students_details () in
  H.p (H.txt not_graded :: not_graded_students)
  :: H.p (H.txt bad_type :: bad_type_students)
  :: H.p [H.txt total_sum]
  :: render_classes part.partition_by_grade

let replace_with_students xs =
  List.map (fun (tok, repr) -> (Token.Map.find tok !students_map, repr)) xs

let _class_selection_updater =
  let previous = ref None in
  let of_repr repr = [H.code [H.txt repr]] in
  let onclick p token repr =
     H.a_onclick @@
       fun _ ->
       (match !previous with
        | None -> ()
        | Some prev -> Manip.replaceChildren prev []);
       previous := Some p;
       Manip.replaceChildren p (of_repr repr);
       set_selected_repr (Some (token, repr));
       true in
  let to_li st repr p =
    let anon = anon_id_of_student st in
    let nick = nickname_of_student st in
    let tok = Token.to_string st.Student.token in
    H.li
      [H.span ~a:[ onclick p st.Student.token repr;
                   H.a_ondblclick (fun _ -> open_tok tok);
                   H.a_class ["student"];
                   H.a_user_data "anon" ("Student " ^ anon);
                   H.a_user_data "nickname" nick;
                   H.a_user_data "token" tok;
         ] [];
       p] in
  let mkfirst (students,repr) =
    let p =  H.p (of_repr repr) in
    previous := Some p;
    to_li students repr p in
  let mkelem (student,repr) =
    to_li student repr @@ H.p []
  in
  selected_class_signal |> React.S.map @@ fun id ->
  match id with
  | None -> ()
  | Some xs ->
     set_selected_repr (Some (List.hd xs));
     let xs = replace_with_students xs in
     Manip.replaceChildren (find_tab "details")
       [H.ul @@ mkfirst (List.hd xs) :: List.map mkelem (List.tl xs)]

let set_classes selected =
  Manip.removeClass (find_tab "list") ("token-id");
  Manip.removeClass (find_tab "list") ("anon-id");
  Manip.removeClass (find_tab "list") ("nickname-id");
  Manip.removeClass (find_tab "details") ("token-id");
  Manip.removeClass (find_tab "details") ("anon-id");
  Manip.removeClass (find_tab "details") ("nickname-id");
  Manip.addClass (find_tab "list") (selected ^ "-id");
  Manip.addClass (find_tab "details") (selected ^ "-id")

let main () =
  Learnocaml_local_storage.init ();
  (match Js_utils.get_lang() with Some l -> Ocplib_i18n.set_lang l | None -> ());
  set_string_translations_view ();
  let session = Learnocaml_local_storage.(retrieve sync_session) in
  let is_teacher = Learnocaml_local_storage.(retrieve is_teacher) in
  if not (is_teacher) then
    (* No security here: it's client-side, and we don't check that the token is
       registered server-side *)
    failwith "The page you are trying to access is for teachers only";
  let exercise_id =
    try (List.assoc "id" Url.Current.arguments)
    with Not_found -> failwith "Exercise id is missing" in
  let fun_id =
    try (List.assoc "function" Url.Current.arguments)
    with Not_found -> failwith "function name is missing" in
  let prof =
    try (int_of_string @@ List.assoc "prof" Url.Current.arguments)
    with Not_found | Failure _ -> failwith "prof is missing or malformed" in

  let update_repr_code = function
    | None -> true
    | Some (tok,_) ->
       Lwt.async (fun () ->
           retrieve (Learnocaml_api.Fetch_save session)
           >|= fun save ->
           match SMap.find_opt exercise_id save.Save.all_exercise_states with
           | None -> ()
           | Some x ->
              update_answer_tab x.Answer.solution);
       true in

  let _repr_selection_updater =
    selected_repr_signal |> React.S.map @@ fun id ->
      let tab = React.S.value tab_select_signal in
      if tab = "answer"
      then update_repr_code id
      else true in

  let fetch_students =
    retrieve (Learnocaml_api.Students_list session)
    >|= fun students ->
    let map =
      List.fold_left (fun res st -> Token.Map.add st.Student.token st res)
        Token.Map.empty students in
    students_map := map in

  let fetch_part =
    retrieve (Learnocaml_api.Partition (session, exercise_id, fun_id, prof))
    >|= fun part ->
    partition := Some part in

  let select = find_component "learnocaml-select-student-info" in

  let update_pii_selected () = set_classes (Manip.value select) in

  Lwt.join [fetch_students; fetch_part] >>= fun () ->
  generate_anon_from_part ();
  hide_loading ~id:"learnocaml-exo-loading" ();
  Manip.replaceChildren (find_tab "list") (exercises_tab ());
  update_pii_selected ();
  init_tab ();
  Manip.Ev.onchange_select select (fun _ -> update_pii_selected (); true);
  Manip.Ev.onclick (find_component "learnocaml-exo-button-answer")
    (fun _ -> select_tab "answer"; update_repr_code (React.S.value selected_repr_signal));
  Lwt.return_unit

let () = run_async_with_log  main
