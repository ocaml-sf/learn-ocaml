open Learnocaml_exercise_state
open Learnocaml_common
open Learnocaml_index
open Lwt.Infix
open Js_utils
open Tyxml_js.Html5
open Dom_html

module StringMap = Map.Make(String);;


(* Internationalization *)
let () = Translate.set_lang ()


let get_titre id =
  Learnocaml_local_storage.(retrieve (editor_state id)).metadata.titre

let get_description id =
  Learnocaml_local_storage.(retrieve (editor_state id)).metadata.description

let get_diff id =
  Learnocaml_local_storage.(retrieve (editor_state id)).metadata.diff
let get_solution id =
  Learnocaml_local_storage.(retrieve (editor_state id)).solution
let get_question id =
  Learnocaml_local_storage.(retrieve (editor_state id)).question
let get_template id =
  Learnocaml_local_storage.(retrieve (editor_state id)).template
let get_testml id =
  Learnocaml_local_storage.(retrieve (editor_state id)).test.testml
let get_testhaut id =
  Learnocaml_local_storage.(retrieve (editor_state id)).test.testhaut
let get_prelude id =
  Learnocaml_local_storage.(retrieve (editor_state id)).prelude
let get_prepare id =
  Learnocaml_local_storage.(retrieve (editor_state id)).prepare
let get_imperative id =
  Learnocaml_local_storage.(retrieve (editor_state id)).checkbox.imperative
let get_undesirable id =
  Learnocaml_local_storage.(retrieve (editor_state id)).checkbox.undesirable

let get_a_question id idQuestion = let string_map = get_testhaut id in
  StringMap.(find idQuestion string_map)

(* TODO refactor the following getters with get_a_question *)

let get_ty id idQuestion = let test_list = get_testhaut id in
  match StringMap.(find idQuestion test_list) with
  | TestAgainstSol a -> a.ty
  | TestAgainstSpec a -> a.ty
  | TestSuite a -> a.ty

let get_name_question id idQuestion= let test_list = get_testhaut id in
  match StringMap.(find idQuestion test_list) with
  | TestAgainstSol a -> a.name
  | TestAgainstSpec a -> a.name
  | TestSuite a -> a.name
                    
let get_type_question id idQuestion =
  let test_list = get_testhaut id in
  match StringMap.(find idQuestion test_list) with
  | TestAgainstSol _ -> Solution
  | TestAgainstSpec _ -> Spec
  | TestSuite _ -> Suite

let get_extra_alea id idQuestion =  let test_list = get_testhaut id in
  match StringMap.(find idQuestion test_list) with
  | TestAgainstSol a -> a.gen
  | TestAgainstSpec a -> a.gen
  | _ -> failwith "?"
                    
let get_input id idQuestion =
  let test_list = get_testhaut id in
  match StringMap.(find idQuestion test_list) with
  | TestAgainstSol a -> a.suite
  | TestAgainstSpec a -> a.suite
  | TestSuite a -> a.suite

let get_spec id idQuestion = let test_list = get_testhaut id in
  match StringMap.(find idQuestion test_list) with
  | TestAgainstSpec a -> a.spec
  | _ -> failwith ""

let get_buffer id =
  Learnocaml_local_storage.(retrieve (editor_state id)).incipit

let compute_question_id test_haut =
  let key_list = List.map (fun (a,b) -> int_of_string a)
                   (StringMap.bindings test_haut) in
  let mi neighbor_color =
    let rec aux c n=match c with
      | [] -> n
      | x :: l -> if x <> n then aux l n else aux neighbor_color (n + 1)
    in aux neighbor_color 1
  in string_of_int (mi key_list)

let save_testhaut testhaut id =
  match Learnocaml_local_storage.(retrieve (editor_state id)) with
    {metadata;incipit;prepare;solution;question;
     template;test;prelude;checkbox;mtime} ->
    let mtime = gettimeofday () in
    let test = {testml = test.testml; testhaut} in
    let new_exo = {metadata;incipit;prepare;solution;question;
                   template;test;prelude;checkbox;mtime} in
    Learnocaml_local_storage.(store (editor_state id)) new_exo


let fetch_test_index id =
  let index = get_testhaut id in
  let json =
    Json_repr_browser.Json_encoding.construct
      testhaut_enc index in
  try Lwt.return (Json_repr_browser.Json_encoding.destruct testhaut_enc json)
  with exn ->
    Lwt.fail (failwith "")

let testhaut_iframe = Dom_html.createIframe Dom_html.document
let iframe_tyxml = Tyxml_js.Of_dom.of_iFrame testhaut_iframe

let find_div id =
  match Manip.by_id id with
    | Some div -> div
    | None -> let window=Dom_html.window in
              let window=window##.parent in
              let document=window##.document in
              Tyxml_js.Of_dom.of_element (Js.Opt.case
                   (document##getElementById (Js.string id))
                   (fun () -> raise Not_found)
                   (fun node -> node))


let remove_exo exercise_id =
  let exos =
    Learnocaml_local_storage.(retrieve (index_state "index")).exos in
  let exos = StringMap.remove exercise_id exos in
  let index = {Learnocaml_exercise_state.exos; mtime = gettimeofday ()} in
  Learnocaml_local_storage.(store (index_state "index")) index;
  Learnocaml_local_storage.(delete (editor_state exercise_id))

let titleUnique titre =
  let exos=
    match Learnocaml_local_storage.(retrieve (index_state "index")) with
    | {Learnocaml_exercise_state.exos; mtime} -> exos in
  match StringMap.find_first_opt
          (fun key -> (StringMap.find key exos).exercise_title = titre)
          exos with
    | None -> true
    | _ -> false

let idUnique id =
  match Learnocaml_local_storage.(retrieve (editor_state id)) with
  | exception Not_found -> true
  | _ -> false

let store_in_index metadata =
  let exercise_title = metadata.titre in
  let exercise_stars = metadata.diff in
  let exercise_kind = Learnocaml_exercise in
  let exercise_short_description = Some metadata.description in
  let exo = {exercise_kind; exercise_stars; exercise_title;
             exercise_short_description} in
  match Learnocaml_local_storage.(retrieve (index_state "index")) with
  | {Learnocaml_exercise_state.exos; mtime} ->
      let former_exos = exos in
      let exos = StringMap.add metadata.id exo former_exos in
      let index = {Learnocaml_exercise_state.exos; mtime = gettimeofday ()} in
      Learnocaml_local_storage.(store (index_state "index")) index


let setInnerHtml elt s =
  elt##.innerHTML := Js.string s

let hide_load id =
  let elt_lml=match find_div id with
    | exception Not_found ->
       let div = Tyxml_js.Html.(div ~a:[ a_id id ]) [] in
       let window=Dom_html.window in
       let window=window##.parent in
       let document=window##.document in
       Manip.(appendChild (Tyxml_js.Of_dom.of_body document##.body) ) div;
       div
    | div -> div
  in
  Manip.(removeClass elt_lml "initial") ;
  Manip.(removeClass elt_lml "loading") ;
  Manip.(addClass elt_lml "loaded")

let show_load id contents =
  let elt =  match find_div id with
    | exception Not_found ->
      let div = Tyxml_js.Html.(div ~a:[ a_id id ]) [] in
      let window=Dom_html.window in
      let window=window##.parent in
      let document=window##.document in
      Manip.(appendChild (Tyxml_js.Of_dom.of_body document##.body) ) div;
      div
    | div -> div in
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

let _ = testhaut_iframe##.width := Js.string "100%"
let _ = testhaut_iframe##.height := Js.string "100%"
let _ = Manip.SetCss.opacity iframe_tyxml (Some "1")
let recovering_callback = ref (fun () -> ())


let checkbox_creator string cas id =
  let chk = input ~a:[ a_id string; a_input_type `Checkbox] () in
  let checked =
    match cas with
    | 0 -> (get_imperative id)
    | _ -> (get_undesirable id) in
  let dom_chk = Tyxml_js.To_dom.of_input chk in
  dom_chk##.checked := Js.bool checked;
  dom_chk##.onclick := handler (fun _ ->
      let a = Learnocaml_local_storage.(retrieve (editor_state id)) in
      let checkbox =
      if cas = 0 then
        {imperative = Js.to_bool dom_chk##.checked;
         undesirable = a.checkbox.undesirable}
      else
        {imperative = a.checkbox.imperative;
         undesirable = Js.to_bool dom_chk##.checked} in
      let new_e = {metadata = a.metadata; incipit = a.incipit;
                   prepare = a.prepare; prelude = a.prelude;
                   checkbox;test = a.test; template = a.template;
                   solution = a.solution; mtime = a.mtime;
                   question = a.question} in
      Learnocaml_local_storage.(store (editor_state id) new_e); Js._true);
  Tyxml_js.Of_dom.of_input dom_chk


let with_test_lib_prepare string =
  "module Dummy_Functor (Introspection :\n                        Introspection_intf.INTROSPECTION) = struct\n  module Dummy_Params = struct\n    let results = ref None\n    let set_progress _ = ()\n    let timeout = None\n    module Introspection = Introspection            \n  end\n  module Test_lib = Test_lib.Make(Dummy_Params)\n  module Report = Learnocaml_report;;\n  let code_ast = (failwith \"WIP\" : Parsetree.structure);;\n\n "
  ^ string ^ " end";;

let typecheck_spec_aux set_class ace_t editor_t top string=
  Learnocaml_toplevel.check top
      (with_test_lib_prepare string) >>= fun res ->
    let error, warnings =
      match res with
      | Toploop_results.Ok ((), warnings) -> None, warnings
      | Toploop_results.Error (err, warnings) -> Some err, warnings in
    let transl_loc { Toploop_results.loc_start ; loc_end } =
      { Ocaml_mode.loc_start ; loc_end } in
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
    Ocaml_mode.report_error ~set_class editor_t error warnings  >>= fun () ->
    Ace.focus ace_t ;
    Lwt.return () ;;

let typecheck_spec set_class ace_t editor_t top =
  typecheck_spec_aux set_class ace_t editor_t top (Ace.get_contents ace_t)

let rec testhaut_init content_div id =
  let elt = find_div "learnocaml-loading" in
    fetch_test_index id >>= fun index ->
  let format_question_list all_question_states =
    let  format_contents acc contents =
          StringMap.fold
            (fun question_id quest acc ->
               let name,ty=match quest with
                 | TestAgainstSol a -> a.name,a.ty
                 | TestAgainstSpec a -> a.name,a.ty
                 | TestSuite a -> a.name,a.ty in
                 div ~a:[a_id "toolbar"; a_class ["button"]] [
              (div ~a:[a_id "button_delete"] [
                  let button = button ~a:[a_id question_id]
                                 [img ~src:"icons/icon_cleanup_dark.svg"
                                    ~alt:"" () ; pcdata ""] in
                  Manip.Ev.onclick button
                    (fun _ ->
                       begin
                         let messages = Tyxml_js.Html5.ul [] in
                         let aborted, abort_message =
                           let t, u = Lwt.task () in
                           let btn_no =
                             Tyxml_js.Html5.(button [ pcdata [%i"No"] ]) in
                           Manip.Ev.onclick btn_no ( fun _ ->
                               hide_load "learnocaml-main-loading" ;
                               true) ;
                           let btn_yes =
                             Tyxml_js.Html5.(button [ pcdata [%i"Yes"] ]) in
                           Manip.Ev.onclick btn_yes (fun _ ->
                               let rmv = get_testhaut id in
                               let testhaut =
                                 StringMap.remove question_id rmv in
                               save_testhaut testhaut id ;
                               hide_load "learnocaml-main-loading";
                               Manip.removeChildren content_div;
                               let _ = testhaut_init content_div id in
                               () ; true) ;
                           let div =
                             Tyxml_js.Html5.(div ~a: [ a_class [ "dialog" ] ]
                             [ pcdata [%i"Are you sure you want to delete \
                                          this question?\n"] ;
                               btn_yes ;
                               pcdata " " ;
                               btn_no ]) in
                           Manip.SetCss.opacity div (Some "0") ;
                           t, div in
                         Manip.replaceChildren messages
                           Tyxml_js.Html5.[ li [ pcdata "" ] ] ;
                         show_load "learnocaml-main-loading" [ abort_message ] ;
                         Manip.SetCss.opacity abort_message (Some "1") ;
                       end ;
                       true) ; button
              ] );
                    (div ~a:[a_id "up"] [
                     let buttonUp = button ~a:[]
                                      [img ~src:"icons/icon_down_dark.svg"
                                         ~alt:"" () ; pcdata "" ] in
                     Manip.Ev.onclick buttonUp
                       (fun _ ->
                         begin
                           let qid = question_id in
                           let testhaut = get_testhaut id in
                            let question = StringMap.find qid testhaut in
                            let suivant = string_of_int
                                            ((int_of_string qid) + 1) in
                            let testhaut =
                              match StringMap.find suivant testhaut with
                              | exception Not_found -> testhaut
                              | qsuivante ->
                                 let map =
                                   StringMap.add qid qsuivante testhaut in
                                 StringMap.add suivant question map in
                            save_testhaut testhaut id;
                            Manip.removeChildren content_div;
                            let _ = testhaut_init content_div id in ()
                          end;
                         true) ;
                     buttonUp;
            ]);
                  (div ~a:[a_id "down"] [
                  let buttonDown = button ~a:[]
                                     [img ~src:"icons/icon_up_dark.svg"
                                        ~alt:"" () ; pcdata "" ] in
                     Manip.Ev.onclick buttonDown
                       (fun _ ->
                         begin
                           let qid = question_id in
                           let testhaut = get_testhaut id in
                           let question = StringMap.find qid testhaut in
                           let intp = (int_of_string qid) -1 in
                           let prec = string_of_int
                                        (if intp = 0 then 1 else intp ) in
                           let testhaut =
                             match StringMap.find prec testhaut with
                              | exception Not_found -> testhaut
                              | qprec ->
                                 let map = StringMap.add qid qprec testhaut in
                                 StringMap.add prec question map in
                            save_testhaut testhaut id;
                            Manip.removeChildren content_div;
                            let _ = testhaut_init content_div id in ()
                          end;
                         true) ;
                     buttonDown;
                  ]);
                  (div ~a:[a_id "duplicate"] [
                  let buttonDuplicate = button ~a:[]
                                          [img ~src:"icons/icon_list_dark.svg"
                                             ~alt:"" (); pcdata "" ] in
                       Manip.Ev.onclick buttonDuplicate
                         (fun _ ->
                           begin
                             let testhaut = get_testhaut id in
                             let question =
                               StringMap.find question_id testhaut in
                             let qid = compute_question_id testhaut in
                             let testhaut =
                               StringMap.add qid question testhaut in
                             save_testhaut testhaut id;
                             Manip.removeChildren content_div;
                             let _ = testhaut_init content_div id in ()
                           end; true);
                       buttonDuplicate;
                  ]) ]
                  ::  a ~a:[ a_onclick (fun _ ->
                  Manip.(addClass elt "loading-layer") ;
                  Manip.(removeClass elt "loaded") ;
                  Manip.(addClass elt "loading") ;
                  Manip.replaceChildren elt [iframe_tyxml] ;
                  testhaut_iframe##.src :=
                    Js.string ("test.html#id=" ^ id ^ "&questionid=" ^
                               question_id ^ "&action=open") ;
                  true) ;
                  a_class [ "exercise" ] ] [
                div ~a:[ a_class [ "descr" ] ] [
                  h1 [ pcdata name ] ;
                  p [   pcdata ty ] ;
                ]
              ] ::
              acc)
            contents acc in
    List.rev (format_contents  ([a ~a:[ a_class ["patterns"]] [
        h1 [ pcdata [%i"Code quality and forbidden patterns"] ];
        div ~a:[ a_class [ "quality" ] ] [
          p [ pcdata [%i"Forbid undesirable code patterns"] ];
          checkbox_creator "quality_box" 1 id ;];
        div ~a:[a_class [ "imperative" ] ] [
          p [ pcdata [%i"Forbid imperative features"] ];
          checkbox_creator "imperative_box" 0 id
        ];]] @
       [a ~a:[ a_onclick
       (fun _ ->
         Manip.(addClass elt "loading-layer") ;
         Manip.(removeClass elt "loaded") ;
         Manip.(addClass elt "loading") ;
         Manip.replaceChildren elt [iframe_tyxml]  ;
         testhaut_iframe##.src:=Js.string ("test.html#id="^id^"&action=open");
         true);
      Tyxml_js.Html5.a_class [ "exercise" ] ] [
         Tyxml_js.Html5.div ~a:[ Tyxml_js.Html5.a_class [ "descr" ] ] [
             Tyxml_js.Html.h1 [ Tyxml_js.Html5.pcdata [%i"New question"] ];
             Tyxml_js.Html5.p [Tyxml_js.Html5.pcdata
                                 [%i"Create a new question"]];
           ];
       ]]) index) in
  let list_div =
    Tyxml_js.Html5.(div ~a:
      [Tyxml_js.Html5.a_id "learnocaml-main-exercise-list" ])
      (format_question_list index) in
  Dom.appendChild (Tyxml_js.To_dom.of_div content_div)
    (Tyxml_js.To_dom.of_div list_div ) ;

  Lwt.return_unit;;


(* ---------- Functions for generate test -> Compile ---------- *)

(* Unused:

let rec redondance liste = match liste with
  | [] -> []
  | e :: s -> e :: redondance (List.filter ((<>) e) s)
 *)

let init = "let () =
            set_result @@
            ast_sanity_check code_ast @@ fun () ->\n"

let section name report = "Section
  ([ Text \"Fonction:\" ; Code \""^name^"\" ], " ^ report ^ " );\n"


(*_____________________Functions for the Generate button_____________________*)

(* we get "val f : int -> int -> int = <fun>" *)

let string_of_char ch = String.make 1 ch

let rec concatenation listech = match listech with
  | [] -> ""
  | c :: l -> (string_of_char c) ^ (concatenation l)


let rec get_equal listeChar = match listeChar with
  | [] -> []
  | '=' :: l -> []
  | ch :: tail -> ch :: (get_equal tail)

let rec get_val listeChar = match listeChar with
  | [] -> []
  | 'v' :: 'a' :: 'l' :: tail -> get_equal tail
  | ch :: suite -> get_val suite

let rec get_next_val listeChar = match listeChar with
  | [] -> []
  | 'v' :: 'a' :: 'l' :: tail -> tail
  | ch :: suite -> get_next_val suite

let rec get_all_val listeChar listeRes = match listeChar with
  | [] -> listeRes
  | _ -> if (get_val listeChar) <> []
         then (get_all_val (get_next_val listeChar)
                 ((get_val listeChar) :: listeRes))
         else listeRes

let rec get_only_fct listeChar listeFinale = match listeChar with
  | [] -> listeFinale
  | 'v'::'a'::'l'::suite -> listeFinale @ (isFct suite ['v';'a';'l'])
  | ch::suite -> get_only_fct suite listeFinale
and isFct listeChar listeAux = match listeChar with
  | [] -> []
  | 'v'::'a'::'l'::suite -> isFct suite ['v';'a';'l']
  | '<'::'f'::'u'::'n'::'>'::suite ->
     get_only_fct suite (listeAux @ ['<';'f';'u';'n';'>'])
  | ch::suite -> isFct suite (listeAux @ [ch])

let rec get_type_of_fct listeChar b = match listeChar with
  | [] -> []
  | ':'::tail -> get_type_of_fct tail true
  | ch::tail -> if b then (ch::(get_type_of_fct tail b))
                else (get_type_of_fct tail b)


let rec get_nom listeChar nom = match listeChar with
  | [] -> nom
  | ' '::suite -> get_nom suite nom
  | ch::' '::suite -> if ch <> ' ' then nom @ [ch] else get_nom suite nom
  | ch::suite -> get_nom suite (nom@[ch])

let rec get_questions listeChar name_and_type = match listeChar with
  | [] -> name_and_type
  | liste::suite -> get_questions suite name_and_type @
                    [(concatenation (get_nom liste []),
                      concatenation (get_type_of_fct liste false))]


let first (a,b,c) = a
let second (a,b,c) = b
let third (a,b,c) = c

(* TODO: Refactor this implementation to avoid "char list"! *)
let maj_mono val_next_mono = match val_next_mono with
  | 'i'::'n'::'t'::[]->'b'::'o'::'o'::'l'::[]
  | 'b'::'o'::'o'::'l'::[]->'c'::'h'::'a'::'r'::[]
  | 'c'::'h'::'a'::'r'::[] -> 's'::'t'::'r'::'i'::'n'::'g'::[]
  | 's'::'t'::'r'::'i'::'n'::'g'::[] -> 'i'::'n'::'t'::[]
  | _ -> failwith "error monomorphic type"

(** Update the list of couples and then return this list
  * and the monomorphic type that must be used *)
let rec get_association listeCouple elt listeCouple2 val_next_mono =
  match listeCouple with
  | [] -> ((elt,maj_mono val_next_mono) ::
             listeCouple2,maj_mono val_next_mono, true)
  | (poly, mono) :: tail ->
     if poly = elt then (listeCouple2,mono,false)
     else (get_association tail elt listeCouple2 val_next_mono)

let getC listeChar =
  let rec before listeChar = match listeChar with
  | [] -> []
  | ' '::suite -> []
  | ch::suite -> ch:: (before suite)
  and after listeChar = match listeChar with
    | [] -> []
    | ' '::suite -> suite
    | ch::suite -> after suite in
  (before listeChar, after listeChar)
(** Replace the 'a, 'b, ... by int || char || ... *)
let rec polymorph_detector_aux listeType listeCouple val_next_mono =
  match listeType with
  | [] -> []
  | '\''::suite -> let ch,tail = getC suite in
                   let v = (get_association
                              listeCouple ch listeCouple val_next_mono) in
                   let res = if third v then polymorph_detector_aux tail (first v) (second v)
                             else polymorph_detector_aux tail (first v) (val_next_mono)
                   in if res = [] then second v else second v @ ' ' :: res
  | ch::tail -> ch::(polymorph_detector_aux tail listeCouple val_next_mono)

let rec decompositionSol str n =
  if str = "" then []
  else if n + 1 = String.length str then [(str.[n])]
  else (str.[n])::(decompositionSol str (n+1))

let extra_alea_poly = 5
and extra_alea_mono = 10

(* TODO: Refactor this implementation to avoid "char list"! *)
(** @param listeChar a list of couples of char lists *)
let rec polymorph_detector listeChar = match listeChar with
  | []-> []
  | (listeNom,listeType)::tail ->
     if String.contains listeType '\'' then
       (listeNom, extra_alea_poly,
        concatenation
          (polymorph_detector_aux (decompositionSol listeType 0)
             [] (['s';'t';'r';'i';'n';'g']))) ::
         (listeNom, extra_alea_poly,
          concatenation
            (polymorph_detector_aux (decompositionSol listeType 0)
               [] (['i';'n';'t']))) ::
           polymorph_detector tail
     else
       (listeNom, extra_alea_mono,
        concatenation
            (polymorph_detector_aux (decompositionSol listeType 0)
               [] (['s';'t';'r';'i';'n';'g']))) :: (* should be simplified *)
         polymorph_detector tail

(* Test to experiment the code’s semantics:

let s =
  (decompositionSol
    "'aa -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h" 0)
in polymorph_detector_aux s [] (['s';'t';'r';'i';'n';'g']);;

polymorph_detector
  [(), "'aa -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h"];;
 *)

(* ____Functions for generate template______________________________________ *)

let failchar =
  decompositionSol {|
  "Remplacez cette chaine par votre code."

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

let rec genTemplate chaine =
  if chaine = "" then ""
  else concatenation (genLet (decompositionSol chaine 0))


(*_________ Check _________________________________________*)

let typecheck set_class ace editor top =
    Learnocaml_toplevel.check top (Ace.get_contents ace) >>= fun res ->
    let error, warnings =
      match res with
      | Toploop_results.Ok ((), warnings) -> None, warnings
      | Toploop_results.Error (err, warnings) -> Some err, warnings in
    let transl_loc { Toploop_results.loc_start ; loc_end } =
      { Ocaml_mode.loc_start ; loc_end } in
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
    Ocaml_mode.report_error ~set_class editor error warnings  >>= fun () ->
    Ace.focus ace ;
    Lwt.return ()

(* ---- create an exo ------------------------------------------------------- *)

let exo_creator proper_id =
  let titre = get_titre proper_id in
  let question = get_question proper_id in
  let question = Omd.to_html (Omd.of_string question) in
  let open Learnocaml_exercise in
  let exo1 = set id proper_id empty in
  let exo2 = set title titre exo1 in
  let exo3 = set max_score 80 exo2 in
  let exo4 = set prepare (get_prepare proper_id) exo3 in
  let exo5 = set prelude (get_prelude proper_id) exo4 in
  let exo6 = set solution (get_solution proper_id) exo5 in
  let exo7 = set test (get_testml proper_id) exo6 in
  let exo8 = set template (get_template proper_id) exo7 in
  set descr question exo8

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
      let checked, check_message =
        let t, u = Lwt.task () in
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
      show_loading ~id:div_id [ check_message ] ;
      Manip.SetCss.opacity check_message (Some "1");
       Lwt.return ();
    end
  else
    begin
      hide_loading ~id:div_id (); 
      Dom_html.window##alert (Js.string result);
       Lwt.return ();
    end;;

(* keep sync with test-spec *)
let test_prel = "open Test_lib\nopen Learnocaml_report;;\n"
