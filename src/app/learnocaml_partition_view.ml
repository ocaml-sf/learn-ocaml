(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
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
     ;  H.ul [
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

let students_partition students part =
  let students_map =
      List.fold_left (fun res st -> Token.Map.add st.Student.token st res)
        Token.Map.empty students in
  List.map (fun t -> Token.Map.find t students_map) part

let list_of_students_details students part =
  let open Student in
  let open Partition in
  let bad_type_students = students_partition students part.bad_type in
  let not_graded_students = students_partition students part.not_graded in
  let rec create_div students_list id = match students_list with 
    | [] -> []
    | t::q -> let tok = Token.to_string t.token in
              let nick = Option.value t.nickname ~default:"Student" in
              H.a ~a:[
                  H.a_ondblclick (fun _ -> open_tok tok);
                  H.a_class ["student"];
                  H.a_user_data "anon" ("Student " ^ string_of_int id ^ " ");
                  H.a_user_data "token" (tok ^ " ");
                  H.a_user_data "nickname" (nick ^ " ");
                  (* i added the spaces here in the attribute,
                     for now i don't see how to do otherwise*)
                ] []
              :: (create_div q (id + 1))
  in (create_div not_graded_students 1,
      create_div bad_type_students (1 + List.length not_graded_students))

let exercises_tab students part =
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
  let (not_graded_students,bad_type_students) = list_of_students_details students part in
  H.p (H.txt not_graded :: not_graded_students)
  :: H.p ( H.txt bad_type :: bad_type_students)
  :: H.p [H.txt total_sum]
  :: render_classes part.partition_by_grade

let replace_with_students xs students =
  let students_map =
      List.fold_left (fun res st -> Token.Map.add st.Student.token st res)
        Token.Map.empty students in
  List.map (fun (tok,repr) -> (Token.Map.find tok students_map,repr)) xs

let _class_selection_updater =
  let previous = ref None in
  let of_repr repr = [H.code [H.txt repr]] in
  let onclick p tok repr =
     H.a_onclick @@
       fun _ ->
       (match !previous with
        | None -> ()
        | Some prev -> Manip.replaceChildren prev []);
       previous := Some p;
       Manip.replaceChildren p (of_repr repr);
       set_selected_repr (Some (tok,repr));
       true in
  let to_li student repr p =
    let tok = student.Student.token in
    let nick = Option.value student.Student.nickname ~default:"Student" in
    let strtok = Token.to_string tok in
    H.li
      ~a:[ onclick p tok repr ;
           H.a_ondblclick (fun _ -> open_tok strtok);
           H.a_class ["student"];
           H.a_user_data "anon" ("Student");
           H.a_user_data "token" (strtok);
           H.a_user_data "nickname" (nick);
      ]
      [H.txt ""; p] in
  let mkfirst (students,repr) =
    let p =  H.p (of_repr repr) in
    previous := Some p;
    to_li students repr p in
  let mkelem (student,repr) =
    to_li student repr @@ H.p []
  in
  selected_class_signal |> React.S.map @@ fun id ->
  match id with
  | None -> Lwt.return_unit
  | Some xs ->
     set_selected_repr (Some (List.hd xs));
     let teacher_token = Learnocaml_local_storage.(retrieve sync_token) in
     retrieve (Learnocaml_api.Students_list teacher_token)
     >>= fun students ->
     let xs = replace_with_students xs students in
     Manip.replaceChildren (find_tab "details")
       [H.ul @@ mkfirst (List.hd xs) :: List.map mkelem (List.tl xs)];
     Lwt.return_unit

let set_classes selected =
  Manip.removeClass (find_tab "list") ("token-id");
  Manip.removeClass (find_tab "list") ("anon-id");
  Manip.removeClass (find_tab "list") ("nickname-id");
  Manip.removeClass (find_tab "details") ("token-id");
  Manip.removeClass (find_tab "details") ("anon-id");
  Manip.removeClass (find_tab "details") ("nickname-id");
  Manip.addClass (find_tab "list") (selected ^ "-id");
  Manip.addClass (find_tab "details") (selected ^ "-id"); true

let main () =
  Learnocaml_local_storage.init ();
  (match Js_utils.get_lang() with Some l -> Ocplib_i18n.set_lang l | None -> ());
  set_string_translations_view ();
  let teacher_token = Learnocaml_local_storage.(retrieve sync_token) in
  if not (Token.is_teacher teacher_token) then
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
           retrieve (Learnocaml_api.Fetch_save tok)
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

  let fetch_students = retrieve (Learnocaml_api.Students_list teacher_token) in
  let fetch_part =
    retrieve (Learnocaml_api.Partition (teacher_token, exercise_id, fun_id, prof))
  in
  Lwt.both fetch_students fetch_part >>= fun (students, part) -> 
  hide_loading ~id:"learnocaml-exo-loading" ();
  Manip.replaceChildren (find_tab "list") (exercises_tab students part);
  init_tab ();
  Manip.Ev.onclick (find_component "learnocaml-exo-button-answer")
    (fun _ -> select_tab "answer"; update_repr_code (React.S.value selected_repr_signal));
  Manip.Ev.onchange_select (find_component "learnocaml-select-student-info")
    (fun _ -> find_component "learnocaml-select-student-info" |> Manip.value |> set_classes);
  Lwt.return_unit

let () = run_async_with_log  main
