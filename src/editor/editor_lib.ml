open Learnocaml_data
open Learnocaml_common
open Learnocaml_index
open Lwt.Infix
open Js_utils
open Tyxml_js.Html5
open Dom_html
open Editor
open Exercise.Meta
(* Internationalization *)

let get_editor_state id=
    (SMap.find id Learnocaml_local_storage.(retrieve editor_index))
let update_index editor_state =
    let old_index=
      Learnocaml_local_storage.(retrieve editor_index)
    in
    let new_index =
      SMap.add editor_state.exercise.id editor_state old_index
    in
    Learnocaml_local_storage.(store editor_index) new_index

let get_titre id =
  (get_editor_state id).metadata.title
let get_description id =
  match (get_editor_state id).metadata.short_description with
  | None -> ""
  | Some d -> d
let get_diff id =
  (get_editor_state id).metadata.stars
let get_solution id =
  (get_editor_state id).exercise.solution
let get_question id =
  (get_editor_state id).exercise.descr
let get_template id =
    (get_editor_state id).exercise.template
let get_testml id =
    (get_editor_state id).exercise.test
let get_prelude id =
    (get_editor_state id).exercise.prelude
let get_prepare id =
    (get_editor_state id).exercise.prepare


let remove_exo exercise_id =
  let old_index= Learnocaml_local_storage.(retrieve editor_index) in
  let new_index= SMap.remove exercise_id old_index in
  Learnocaml_local_storage.(store editor_index) new_index

let titleUnique title =
  let exos=Learnocaml_local_storage.(retrieve editor_index) in
  match SMap.find_first_opt
          (fun key -> (SMap.find key exos).metadata.title = title)
          exos with
  | None -> true
  | _ -> false

let idUnique id =
  match SMap.find id Learnocaml_local_storage.(retrieve editor_index ) with
  | exception Not_found -> true
  | _ -> false

let new_state (metadata:Exercise.Meta.t) =
  let id= match metadata.id with
      None -> ""
    | Some i -> i
  in
  let exercise = {id;prelude="";
                  template="";descr="";prepare="";
                  test="";solution="";max_score=0}
  in
  {exercise;metadata}

let setInnerHtml elt s =
  elt##.innerHTML := Js.string s

let show_load id contents =
  let elt = match Manip.by_id id with
      None -> failwith "Element not found"
    | Some e -> e
  in
  Manip.(addClass elt "loading-layer") ;
  Manip.(removeClass elt "loaded") ;
  Manip.(addClass elt "loading") ;
  let chamo_src =
    "icons/tryocaml_loading_" ^ string_of_int (Random.int 8 + 1) ^ ".gif" in
  Manip.replaceChildren elt
    Tyxml_js.Html.[
      div ~a: [ a_id "chamo" ] [ img ~alt: "loading" ~src: chamo_src () ] ;
      div ~a: [ a_class [ "messages" ] ] contents
  ]


let test_lib_prepare =
  {|module Wrapper (Introspection : Introspection_intf.INTROSPECTION) =
   struct
   module Mock = struct
   let results = ref None
   let set_progress _ = ()
   let timeout = None
   module Introspection = Introspection
   end
   module Test_lib = Test_lib.Make(Mock)
   module Report = Learnocaml_report;;
   open Mock
   let code_ast = (failwith "WIP" : Parsetree.structure);;
   |}

let rec num_occs s i c num_acc =
  if i > String.length s then num_acc
  else match String.index_from_opt s (i + 1) c with
       | None -> num_acc
       | Some i -> num_occs s i c (num_acc + 1)
let offset_test_lib_prepare =
  num_occs test_lib_prepare (-1) '\n' 0

let with_test_lib_prepare string = test_lib_prepare ^ string ^ " end";;
let typecheck set_class ace_t editor_t top prelprep ?(mock = false) ?onpasterr string =
  let offset_prelprep = num_occs prelprep (-1) '\n' 0 in
  let code = prelprep ^ if mock then with_test_lib_prepare string
                        else string
  and grading = mock in
  Learnocaml_toplevel.check ~grading top code >>= fun res ->
  let error, warnings =
    match res with
    | Toploop_results.Ok ((), warnings) -> None, warnings
    | Toploop_results.Error (err, warnings) -> Some err, warnings in
  let shift_loc (l, c) =
    (l - (if mock then offset_test_lib_prepare else 0)
     - offset_prelprep, c) in
  let transl_loc { Toploop_results.loc_start ; loc_end } =
    { Ocaml_mode.loc_start = shift_loc loc_start ;
      Ocaml_mode.loc_end = shift_loc loc_end } in
  let error = match error with
    | None -> None
    | Some { Toploop_results.locs ; msg ; if_highlight } ->
       Some { Ocaml_mode.locs = List.map transl_loc locs ;
              msg = (if if_highlight <> "" then if_highlight else msg) } in
  let warnings =
    List.map
      (fun { Toploop_results.locs ; msg ; if_highlight } ->
        { Ocaml_mode.loc = transl_loc (List.hd locs) ;
          msg = (if if_highlight <> "" then if_highlight else msg) })
      warnings in
  let pasterr =
    match error with
    | None -> false
    | Some {Ocaml_mode.locs; _} ->
       List.exists (fun { Ocaml_mode.loc_start = (m, _);
                          Ocaml_mode.loc_end = (n, _) } -> (m <= 0 || n <= 0))
         locs in
  match onpasterr, pasterr with
  | Some onpasterr, true -> onpasterr ()
  | None, _ | _, false ->
     Ocaml_mode.report_error ~set_class editor_t error warnings >>= fun () ->
     Ace.focus ace_t;
     Lwt.return ()

(* ---------- Functions for generate test -> Compile ---------- *)

(* Unused:

let rec redondance liste = match liste with
  | [] -> []
  | e :: s -> e :: redondance (List.filter ((<>) e) s)
 *)

let init = "let () =
            set_result @@
            ast_sanity_check code_ast @@ fun () ->\n"

let section name report = {|Section ([ Text "Fonction:" ; Code "|}
                          ^ name ^ {|" ], |} ^ report ^ " );\n"


(*_____________________Functions for the Generate button_____________________*)

(* Remove duplicates; keep the latest occurence *)
let rec undup_assoc = function
  | [] -> []
  | (f, ty) :: l -> if List.mem_assoc f l then undup_assoc l
                    else (f, ty) :: undup_assoc l

(* Minor bug: if the file contains 2 definitions of a same identifier
   and only one of them is a function, this function will be selected
   even if it is defined before the non-function expression. *)
let extract_functions s =
  (* Remove module/module_types as their signature could contain val items *)
  let s = Regexp.(global_replace (regexp "module type\\\s\\w+\\s=\\ssig\\s[^]+?\\send\\s*") s "") in
  let s = Regexp.(global_replace (regexp "module type\\s\\w+\\s=\\s\\w+\\s*") s "") in
  let s = Regexp.(global_replace (regexp "module\\s\\w+\\s:\\ssig\\s[^]+?\\send\\s*") s "") in
  let s = Regexp.(global_replace (regexp "module\\s\\w+\\s:\\s\\w+\\s*") s "") in
  let rec process i acci =
    match Regexp.(search (regexp "val\\s(\\S+)\\s:\\s([^:]+?)\\s+=\\s+<fun>") s i) with
    | None -> List.rev acci
    | Some (i, result) ->
       match Regexp.(matched_group result 1, matched_group result 2) with
       | Some func, Some ty -> process (i + 1) ((func, ty) :: acci)
       | _ -> process (i + 1) acci
  in undup_assoc (process 0 [])

let find_all r s =
  let rec process i acci =
    match Regexp.(search r s i) with
    | None -> List.rev acci
    | Some (i, result) ->
       match Regexp.(matched_group result 0) with
       | Some s -> process (i + 1) (s :: acci)
       | _ -> Dom_html.window##alert (Js.string "Error in editor_lib.ml: this should not occur");
              List.rev acci
  in process 0 []

let replace_all map s =
  List.fold_left (fun res (e, by) ->
      Regexp.(global_replace (regexp_string e) res by))
    s map

let base = ["int"; "bool"; "char"; "string"]
let gen1 i = List.nth base (i mod 4)
let gen2 i =
  let q, r = (i / 4) mod 3, i mod 4 in
  if q + 1 = r then List.nth base 0
  else List.nth base (q + 1)

let monomorph_generator l =
  let f ty =
    let vars =
      List.sort_uniq compare
      @@ find_all (Regexp.regexp "'[A-Za-z](?:\\w[A-Za-z0-9_']*)?") ty
    in
    if vars = [] then [(10, ty)]
    else let t1 = replace_all (List.mapi (fun i e -> (e, gen1 i)) vars) ty
         and t2 = replace_all (List.mapi (fun i e -> (e, gen2 i)) vars) ty
         in [(5, t1); (5, t2)]
  in
  List.map (fun (func, ty) -> (func, f ty)) l
  |> List.map  (fun (name, list_mono)  ->
         List.fold_left  (fun acc (gen, ty) ->
             TestAgainstSol
               {name; ty; suite = "[]"; gen;
                tester = ""; sampler = ""}::acc)
           []
           list_mono)
  |> List.flatten

(* ____Functions for generate template______________________________________ *)

(* TODO: Refactor and delete concatenation that involves type (char list) *)

let string_of_char ch = String.make 1 ch

let rec concatenation listech = match listech with
  | [] -> ""
  | c :: l -> (string_of_char c) ^ (concatenation l)

let rec decompositionSol str n =
  if str = "" then []
  else if n + 1 = String.length str then [(str.[n])]
  else (str.[n])::(decompositionSol str (n+1))

let failchar =
  decompositionSol {|
                    "Change this string for you code."

                    |} 0

let tail l = match l with
  | [] -> []
  | e :: l -> l

let rec commentaire listech cpt = match listech with
  | [] -> []
  | '*'::')'::l -> if cpt = 0 then l else commentaire l (cpt - 1)
  | '('::'*'::l -> commentaire l (cpt + 1)
  | c::l -> commentaire l cpt

let rec premierLet listech = match listech with
  | [] -> []
  | '('::'*'::l -> premierLet (commentaire l 0)
  | c::'l'::'e'::'t'::' '::l ->
     if c = '\n' || c = ' ' then ('l'::'e'::'t'::' '::l) else premierLet l
  | 'l'::'e'::'t'::' '::l -> 'l'::'e'::'t'::' '::l
  | ' '::l -> premierLet l
  | '\n'::l -> premierLet l
  | _ -> []

let rec validationLet listech = match listech with
  | [] -> false
  | ' '::l -> validationLet l
  | '\n'::l -> validationLet l
  | '('::l -> validationLet l
  | 'l'::'e'::'t'::l -> false
  | _ -> true

let rec rechercheEgal listech = match listech with
  | [] -> 0
  | '='::l -> 1
  | ' '::'l'::'e'::'t'::' '::l -> 2
  | '\n'::'l'::'e'::'t'::' '::l -> 2
  | c::l -> rechercheEgal l

let rec rechercheLet listech b = match listech with
  | [] -> []
  | '('::'*'::l -> rechercheLet (commentaire l 0) b
  | ';'::';'::l -> rechercheLet l true
  | '='::l -> rechercheLet l (validationLet l)
  | _::'t'::'h'::'e'::'n'::_::l -> rechercheLet l (validationLet l)
  | _::'e'::'l'::'s'::'e'::_::l -> rechercheLet l (validationLet l)
  | _::'i'::'n'::_::l -> rechercheLet l (validationLet l)
  | '-'::'>'::l -> rechercheLet l (validationLet l)
  | 'l'::'e'::'t'::' '::l ->
     if b && (rechercheEgal l) = 1 then 'l'::'e'::'t'::' '::l
     else if (rechercheEgal l) = 0 then rechercheLet l false
     else rechercheLet l true
  | c::suite -> rechercheLet suite b

let rec decomposition2 listech = match listech with
  | [] -> []
  | '='::l -> ['=']
  | c::l -> c :: (decomposition2 l)

let decompoFirst listech = match listech with
  | []-> []
  | _ -> (decomposition2 listech) @ failchar

let rec genLet listech =
  let liste = rechercheLet listech true in
  match liste with
  | [] -> []
  | _ -> (decomposition2 liste) @ failchar @ (genLet (tail liste))

let genTemplate chaine =
  if chaine = "" then ""
  else concatenation (genLet (decompositionSol chaine 0))

(* ---- create an exo ------------------------------------------------------- *)
let exo_creator proper_id =
  let exercise = (get_editor_state proper_id).exercise in
  let read_field field =
      match field with
      | "id"-> Some exercise.id
      | "prelude.ml" -> Some exercise.prelude
      | "template.ml" -> Some exercise.template
      | "descr.md" -> Some exercise.descr
      | "prepare.ml" -> Some exercise.prepare
      | "test.ml" -> Some exercise.test
      | "solution.ml" -> Some exercise.solution
      |  "max_score" -> Some (string_of_int exercise.max_score)
      | _ -> None
  in
  Learnocaml_exercise.read
    ~read_field
    ~id:proper_id
    ~decipher:false
    ()

let get_answer top =
  Learnocaml_toplevel.execute_test top

(* TODO look for the record type of res to make the message more understandable *)
let typecheck_dialog_box div_id res =
  let result,ok =
    let open Toploop_results in
    match res with
    | Ok _ ->  [%i"Your question does typecheck. "],true
    | Error (err,_) ->
       [%i"Your question does not typecheck. "]
       ^ err.msg ,false in
  if ok then
    begin
      let messages = Tyxml_js.Html5.ul [] in
      let _checked, check_message =
        let t, _u = Lwt.task () in
        let btn_ok = Tyxml_js.Html5.(button [ pcdata [%i"OK"] ]) in
        Manip.Ev.onclick btn_ok ( fun _ ->
                                  hide_loading ~id:div_id () ; true) ;
        let div =
          Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                            [ pcdata result ;
                              btn_ok ;
          ]) in
        Manip.SetCss.opacity div (Some "0") ;
        t, div in
      Manip.replaceChildren messages
        Tyxml_js.Html5.[ li [ pcdata "" ] ] ;
      show_load div_id [ check_message ] ;
      Manip.SetCss.opacity check_message (Some "1");
      Lwt.return ();
    end
  else
    begin
      hide_loading ~id:div_id ();
      Dom_html.window##alert (Js.string result);
      Lwt.return ();
    end


let put_exercise_id id (old_state:editor_state)  =
  {
    exercise = {old_state.exercise with id} ;
    metadata = {old_state.metadata with id =Some id}
  }
 ;;   

module Editor_io = struct

  let download_file name contents =
    let url =
      Js.Unsafe.meth_call (Js.Unsafe.global##._URL) "createObjectURL" [| Js.Unsafe.inject contents |] in
    let link = Dom_html.createA Dom_html.document in
    link##.href := url ;
    Js.Unsafe.set link (Js.string "download") (Js.string name) ;
    ignore (Dom_html.document##.body##(appendChild ((link :> Dom.node Js.t)))) ;
    ignore (Js.Unsafe.meth_call link "click" [||]) ;
    ignore (Dom_html.document##.body##(removeChild ((link :> Dom.node Js.t))))
        
  let download id =
    let name = id ^ ".zip" in
    let json = (get_editor_state id) 
               |>  Json_repr_browser.Json_encoding.construct
                     Editor.editor_state_enc
    in
    let contents =  Js._JSON##(stringify json) in
    let editor_download = Js.Unsafe.eval_string "editor_download" in
    let callback = download_file name in 
    let _ =
      Js.Unsafe.fun_call editor_download
      [|Js.Unsafe.inject contents;
        Js.Unsafe.inject (Js.wrap_callback callback) |] in ()

  let download_all () =
    let name = "exercises.zip" in
    let editor_index= Learnocaml_local_storage.(retrieve editor_index) in
    let json =  Json_repr_browser.Json_encoding.construct
                  (SMap.enc editor_state_enc)
                  editor_index
    in
    let exercises =  Js._JSON##(stringify json) in
    let index = SMap.fold
                  (fun k editor_state acc ->
                    (k, Some editor_state.metadata ) :: acc) editor_index []
    in
    let index = Learnocaml_data.Exercise.Index.Exercises index
                |> Json_repr_browser.Json_encoding.construct
                     Exercise.Index.enc
    in
    let index = Js._JSON##(stringify index) in
    let editor_download = Js.Unsafe.eval_string "editor_download_all" in
    let callback = download_file name in 
    let _ =
      Js.Unsafe.fun_call editor_download
        [|Js.Unsafe.inject exercises;
          Js.Unsafe.inject index;
        Js.Unsafe.inject (Js.wrap_callback callback) |] in ()
                                                   
  let upload_file () =
    let input_files_load =
      Dom_html.createInput ~_type: (Js.string "file") Dom_html.document in
    let result_t, result_wakener = Lwt.wait () in
    let fail () =
      Lwt.wakeup_exn result_wakener
        (Failure "file loading not implemented for this browser") ;
      Js._true
    in
    input_files_load##.onchange :=
      Dom.handler
        (fun ev ->
          Js.Opt.case (ev##.target) fail @@
            fun target ->
            Js.Opt.case (Dom_html.CoerceTo.input target) fail @@
              fun input ->
              Js.Optdef.case (input##.files) fail @@
                fun files ->
                Js.Opt.case (files##(item (0))) fail @@
                  fun file ->
                  Lwt.wakeup result_wakener file; Js._true);
    ignore (Js.Unsafe.meth_call input_files_load "click" [||]) ;
    result_t

  let upload_new_exercise id to_save =
    let to_save = put_exercise_id id to_save
    in
    let open Exercise.Meta in
    let result= idUnique id && titleUnique to_save.metadata.title in
    if result then
      update_index to_save;
   result        
              
  let upload () =
    run_async_with_log
      (fun () ->
        upload_file () >>=
          fun file ->
          let f = Js.Unsafe.eval_string "editor_import" in 
          let callback =
            (fun text ->
              SMap.iter
                (fun id editor_state ->
                  if not (upload_new_exercise id editor_state)  then
                    Learnocaml_common.alert [%i"Identifier and/or title not unique\n"])
                (Json_repr_browser.Json_encoding.destruct 
                   (SMap.enc editor_state_enc)
                   (Js._JSON##(parse text)));
              Dom_html.window##.location##reload)
          in
          Js.Unsafe.fun_call f [| Js.Unsafe.inject file ;
                            Js.Unsafe.inject callback|])          
                                                             
end
