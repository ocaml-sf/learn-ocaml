(** This ocaml file is associated with test.html.
  * This one is called on an iframe.
  * This is why there are functions like close_frame *)

open Js_of_ocaml
open Js_utils
open Lwt.Infix
open Dom_html
open Learnocaml_common
open Editor_lib
open Learnocaml_exercise_state
open Tyxml_js.Html5

let () =
  show_loading ~id:"check-answer"
    Tyxml_js.Html5.[ ul [ li [ pcdata [%i"Loading"] ] ] ];
  
  Translate.set_lang ();
  
  let translations = [
    "check", [%i"Check"];
    "cancel", [%i"Cancel"];
    "save", [%i"Save"];
    "txt_name", [%i"Function name: "];
    "txt_ty", [%i"Type: "];
    "txt_sol", [%i"Solution"];
    "txt_spec", [%i"Specification"];
    "txt_suite", [%i"Tests suite"];
    "txt_input_sol", [%i"Arguments:<br>\
                         <span class=\"popuptext\" id=\"popup-sol\"/>"];
    "txt_gen_sol", [%i"Number of generated tests:<br>"];
    "txt_datalist_sol", [%i"Tester:<br>"];
    "txt_sampler_sol", [%i"Sampler:<br>"];
    "txt_input_spec", [%i"Arguments:<br>\
                          <span class=\"popuptext\" id=\"popup-spec-arg\"/>"];
    "txt_gen_spec", [%i"Number of generated tests:<br>"];
    "txt_datalist_spec", [%i"Tester:<br>"];
    "txt_sampler_spec", [%i"Sampler:<br>"];
    "txt_spec_specification", [%i"Specification:<br>\
                               <span class=\"popuptext\" id=\"popup-spec\"/>"];
    "txt_suite_input", [%i"Arguments and results:<br>\
                           <span class=\"popuptext\" id=\"popup-suite\"/>"];
    "txt_datalist_suite", [%i"Tester:<br>"];
  ] in
  Translate.set_string_translations translations

let txt_popup_arg = "[!! 1; !! 2]" ^
                      [%i" for two tests for a function with int -> 'a profile"] ^
                        "<br>[true @: 4 @:!! \"titi\"]" ^
                          [%i" for one test for a function<br>\
                              with bool -> int -> string -> 'a profile"]
let txt_popup_suite = "[false @:!! false ==> false;<br>
                       false @:!! true ==> true;<br>
                       true @:!! false ==> true;<br>
                       true @:!! true ==> false]<br>" ^
                        [%i"This is the syntax for the exclusive or.<br>\
                            It is the same syntax as the arguments',<br>\
                            but you also have to provide the return value \
                            after ==>."]
let txt_popup_spec = "fun f args ret -> let x0 = \
               apply (fun n u -> n) args in ~~ (x0 < ret)<br>" ^
                       [%i"- f is the function<br>"] ^
                         [%i"- args are its arguments<br>"] ^
                           [%i"- ret is the return value of the function \
                               when applied with args<br>"] ^
                             [%i"In this example, the return value should be \
                                 greater than the first argument."]

let init_tabs, select_tab =
  let names = [ "solution"; "spec"; "suite" ] in
  let current = ref "suite" in
  let select_tab name =
    set_arg "tab" name ;
    Manip.removeClass
      (find_component ("learnocaml-tab-" ^ !current))
      "front-tab" ;
    Manip.addClass
      (find_component ("learnocaml-tab-" ^ name))
      "front-tab" ;
    current := name in
  let init_tabs () =
    current := begin try
        let requested = arg "tab" in
        if List.mem requested names then requested else "suite"
      with Not_found -> "suite"
    end ;
    List.iter
      (fun name ->
         Manip.removeClass
           (find_component ("learnocaml-tab-" ^ name))
           "front-tab" )
      names ;
    select_tab !current in
  init_tabs, select_tab

let id = arg "id"

let name = match getElementById_coerce "name" CoerceTo.input with
   | None -> failwith "unknown element name"
   | Some s -> s
let ty = match getElementById_coerce "ty" CoerceTo.input with
   | None -> failwith "unknown element ty"
   | Some s -> s

(* radio button *)
let solution = match getElementById_coerce "solution" CoerceTo.input with
  | None -> failwith ""
  | Some s -> s
let spec = match getElementById_coerce "spec" CoerceTo.input with
  | None -> failwith ""
  |Some s -> s
let suite = match getElementById_coerce "suite" CoerceTo.input with
  | None -> failwith ""
  | Some s -> s


let samplerSol = match getElementById_coerce "sol-sampler" CoerceTo.input with
  | None -> failwith "unknown element sampler sol"
  | Some s -> s
let samplerSpec = match getElementById_coerce "spec-sampler" CoerceTo.input with
  | None -> failwith "unknown element sampler spec"
  | Some s -> s

let extraAleaSol =match getElementById_coerce "sol-gen" CoerceTo.input with
  | None -> failwith "unknown element extraAleaSol"
  | Some s -> s
let extraAleaSpec = match getElementById_coerce "spec-gen" CoerceTo.input with
  | None -> failwith "unknown element extraAleaSpec"
  | Some s -> s

let datalistSol = match getElementById_coerce "sol-datalist" CoerceTo.input with
  | None -> failwith "unknown element datalistSol"
  | Some s -> s
let datalistSpec =
  match getElementById_coerce "spec-datalist" CoerceTo.input with
  | None -> failwith "unknown element datalistSpec"
  | Some s -> s
let datalistSuite =
  match getElementById_coerce "suite-datalist" CoerceTo.input with
  | None -> failwith "unknown element datalistSuite"
  | Some s -> s;;

let txtPopupSol = getElementById "txt_input_sol";;
let txtPopupSpecArg = getElementById "txt_input_spec";;
let txtPopupSuite = getElementById "txt_suite_input";;
let txtPopupSpec = getElementById "txt_spec_specification";;

let save = getElementById "save";;

let setInnerHtml elt s =
  elt##.innerHTML := Js.string s


let input_solution_editor = find_component "learnocaml-tab-solution-input";;
let editor_input_solution = Ocaml_mode.create_ocaml_editor
    (Tyxml_js.To_dom.of_div input_solution_editor);;
let ace_input_sol = Ocaml_mode.get_editor editor_input_solution;;
let _ = Ace.set_contents ace_input_sol "[]";
        Ace.set_font_size ace_input_sol 18;;

let input_spec_editor = find_component "learnocaml-tab-spec-input"
let editor_input_spec =
  Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div input_spec_editor)
let ace_input_spec = Ocaml_mode.get_editor editor_input_spec
let _ = Ace.set_contents ace_input_spec "[]";
        Ace.set_font_size ace_input_spec 18;;

let spec_spec_editor = find_component "learnocaml-tab-spec-spec"
let editor_spec_spec =
  Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div spec_spec_editor)
let ace_spec_spec = Ocaml_mode.get_editor editor_spec_spec
let _ =  Ace.set_contents ace_spec_spec "fun f args ret ->\n...";
         Ace.set_font_size ace_spec_spec 18;;

let input_suite_editor = find_component "learnocaml-tab-suite-input"
let editor_input_suite =
  Ocaml_mode.create_ocaml_editor (Tyxml_js.To_dom.of_div input_suite_editor)
let ace_input_suite = Ocaml_mode.get_editor editor_input_suite
let _ = Ace.set_contents ace_input_suite "[]";
        Ace.set_font_size ace_input_suite 18;;

(* -------------------------------------------------------------------- *)

let save_suite () =
  let name = Js.to_string name##.value in
  let ty = Js.to_string ty##.value in
  let input = Ace.get_contents ace_input_suite in
  let datalist = Js.to_string datalistSuite##.value in
  if input = "" then
    TestSuite {name; ty; suite = "[]"; tester = datalist}
  else
    TestSuite {name; ty; suite = input; tester = datalist}

let save_solution () =
  let name = Js.to_string name##.value in
  let ty = Js.to_string ty##.value in
  let input = Ace.get_contents ace_input_sol in
  let extra_alea = int_of_string (Js.to_string extraAleaSol##.value) in
  let datalist = Js.to_string datalistSol##.value in
  let sampler = Js.to_string samplerSol##.value in
  if input = "" then
    TestAgainstSol {name; ty; suite = "[]"; gen = extra_alea;
                    tester = datalist; sampler}
  else
    TestAgainstSol {name; ty; suite = input; gen = extra_alea;
                    tester = datalist; sampler}

let save_spec () =
  let name = Js.to_string name##.value in
  let ty = Js.to_string ty##.value in
  let input = Ace.get_contents ace_input_spec in
  let specification = Ace.get_contents ace_spec_spec in
  let extra_alea = int_of_string (Js.to_string extraAleaSpec##.value) in
  let datalist = Js.to_string datalistSpec##.value in
  let sampler = Js.to_string samplerSpec##.value in
  if input = "" then
    TestAgainstSpec {name; ty; suite = "[]"; spec = specification;
                     gen = extra_alea; tester = datalist; sampler}
  else
    TestAgainstSpec {name; ty; suite = input; spec = specification;
                     gen = extra_alea; tester = datalist; sampler}

(* ---- restore fields if they are not empty ------------------------------- *)

let testhaut = get_testhaut id
    
let () = match arg "questionid" with
  | exception Not_found -> select_tab "suite"; suite##.checked := Js.bool true
  | qid ->
     try let qid = int_of_string qid in
      begin
      let name_elt = name in
      let ty_elt = ty in
      let suite_elt = suite in
      let spec_elt = spec in
          match IntMap.find qid testhaut with
             | TestSuite {name;ty;suite;tester} ->
                begin
                  Ace.set_contents ace_input_suite suite;
                  name_elt##.value:=Js.string name;
                  suite_elt##.checked := Js.bool true;
                  ty_elt##.value:=Js.string ty;
                  datalistSuite##.value:= Js.string tester;
                  select_tab "suite"
                end;
             | TestAgainstSpec {name;ty;gen;tester;sampler;suite;spec} ->
                begin
                  Ace.set_contents ace_input_spec suite;
                  Ace.set_contents ace_spec_spec spec;
                  name_elt##.value:=Js.string name;
                  spec_elt##.checked := Js.bool true;
                  ty_elt##.value:=Js.string ty;
                  extraAleaSpec##.value:= Js.string (string_of_int gen);
                  datalistSpec##.value:= Js.string tester;
                  samplerSpec##.value:= Js.string sampler;
                  select_tab "spec"
                end;
             | TestAgainstSol {name;ty;gen;tester;sampler;suite} ->
                begin
                  Ace.set_contents ace_input_sol suite;
                  name_elt##.value:=Js.string name;
                  solution##.checked := Js.bool true;
                  ty_elt##.value:=Js.string ty;
                  extraAleaSol##.value:= Js.string (string_of_int gen);
                  datalistSol##.value:=Js.string tester;
                  samplerSol##.value:=Js.string sampler;
                  select_tab "solution"
                end
      end with
     | Failure _ ->
        (* TODO: error message? *)
        select_tab "suite"; suite##.checked := Js.bool true

let () = solution##.onclick :=
           handler (fun _ -> select_tab "solution"; Js._true);;
let () = spec##.onclick := handler
      (fun _ -> select_tab "spec";
        if Ace.get_contents ace_spec_spec = "" then
          Ace.set_contents ace_spec_spec "fun f args ret ->\n...";
        Js._true);;
let () = suite##.onclick := handler (fun _ -> select_tab "suite"; Js._true);;

let show_sol = ref false;;
let () = let popup = find_component "popup-sol" in
         Manip.setInnerHtml popup txt_popup_arg;
         txtPopupSol##.onclick := handler
            (fun _ -> let popup = find_component "popup-sol" in
                      let test = find_component "learnocaml-test" in
                      if !show_sol then
                        begin
                          Manip.removeClass popup "show";
                          Manip.SetCss.opacity test (Some "1");
                          show_sol := false;
                        end
                      else
                        begin
                          Manip.addClass popup "show";
                          Manip.SetCss.opacity test (Some "0");
                          show_sol := true;
                        end;
                      Js._true);;

let show_spec_arg = ref false;;
let () = let popup = find_component "popup-spec-arg" in
         Manip.setInnerHtml popup txt_popup_arg;
         txtPopupSpecArg##.onclick := handler
            (fun _ -> let popup = find_component "popup-spec-arg" in
                      let test = find_component "learnocaml-test" in
                      if !show_spec_arg then
                        begin
                          Manip.removeClass popup "show";
                          Manip.SetCss.opacity test (Some "1");
                          show_spec_arg := false;
                        end
                      else
                        begin
                          Manip.addClass popup "show";
                          Manip.SetCss.opacity test (Some "0");
                          show_spec_arg := true;
                        end;
                      Js._true);;

let show_spec = ref false;;
let () = let popup = find_component "popup-spec" in
         Manip.setInnerHtml popup txt_popup_spec;
         txtPopupSpec##.onclick := handler
            (fun _ -> let popup = find_component "popup-spec" in
                      if !show_spec then
                        begin
                          Manip.removeClass popup "show";
                          show_spec := false;
                        end
                      else
                        begin
                          Manip.addClass popup "show";
                          show_spec := true;
                        end;
                      Js._true);;

let show_suite = ref false;;
let () = let popup = find_component "popup-suite" in
         Manip.setInnerHtml popup txt_popup_suite;
         txtPopupSuite##.onclick := handler
            (fun _ -> let popup = find_component "popup-suite" in
                      let test = find_component "learnocaml-test" in
                      if !show_suite then
                        begin
                          Manip.removeClass popup "show";
                          Manip.SetCss.opacity test (Some "1");
                          show_suite := false;
                        end
                      else
                        begin
                          Manip.addClass popup "show";
                          Manip.SetCss.opacity test (Some "0");
                          show_suite := true;
                        end;
                      Js._true);;

let transResultOption = function
  | None -> false
  | Some s -> true
let name_correct s = s <> ""
let type_correct s = s <> ""

let close_frame () =
  (* trick to get access to the container
  	 of the frame (learnocaml-loading) *)
  let window = Dom_html.window in
  let window = window##.parent in
  let document = window##.document in
  let div = Js.Opt.case
              (document##getElementById (Js.string "learnocaml-loading"))
      (fun () -> failwith "titi")
      (fun node -> node) in
  let exo_list =
    Js.Opt.case
      (document##getElementById (Js.string "learnocaml-exo-testhaut-pane"))
      (fun () -> failwith "toto")
      (fun pnode -> pnode) in
  let exo_list=Tyxml_js.Of_dom.of_element exo_list in
  Manip.removeChildren exo_list;

  let _ = testhaut_init exo_list id in ();
  div##setAttribute (Js.string "class") (Js.string "loading-layer loaded")

let toString = function
  | None -> failwith "incorrect_input"
  | Some input -> Js.to_string input##.value

let name_error = getElementById "name_error"
let type_error = getElementById "type_error"

(* ---- save button -------------------------------------------------------- *)
let question_id =  match arg "questionid" with
  | exception Not_found -> compute_question_id testhaut
  | qid -> int_of_string qid (* TODO: may raise error *)

let save_handler close = (fun _ ->
    let name = Js.to_string name##.value in
    let ty = Js.to_string ty##.value in
    let name_correct = name_correct name in
    let type_correct = type_correct ty in
    (if not name_correct then
       setInnerHtml name_error [%i"Incorrect name: a name can't be empty"]
     else
       setInnerHtml name_error "");
    (if not type_correct then
       setInnerHtml type_error [%i"Incorrect type: a type can't be empty"]
     else
       setInnerHtml type_error "");
    if name_correct && type_correct then (
      let question =
        match arg "tab" with
  	| "suite" -> save_suite ()
        | "solution" -> save_solution ()
        | "spec" -> save_spec ()
        | _ -> failwith "" in
      let testhaut = get_testhaut id in
      let testhaut = IntMap.add question_id question testhaut in
      save_testhaut testhaut id ;
      close ();
    );
    Js._true
  )
let _ = save##.onclick := handler (save_handler close_frame)

(* ---- Cancel button ------------------------------------------------------- *)
let cancel = getElementById "cancel"
let () = cancel##.onclick := handler (fun _ ->
  let _ = close_frame () in (); Js._true)

(* ----Check button ------------------------------------------------------------- *)
let check = getElementById "check" ;;
let container_div = find_component "check-answer";; 

let after_init top =
  begin (* TODO: try to simplify this small hack *)
    Lwt.return true
  end >>= fun r1 ->
  if not r1 then failwith [%i"unexpected error"];
  Learnocaml_toplevel_worker_caller.set_checking_environment top >>= fun _ ->
  Lwt.return_unit

let () =
  Lwt.async @@ fun () ->
  Learnocaml_toplevel_worker_caller.create ~after_init ()
  >>= fun top->
  hide_loading ~id:"check-answer" ();
  check##.onclick := handler (fun _ ->
      let _ = save_handler ( fun ()->() ) () in ();
      show_loading ~id:"check-answer"
        Tyxml_js.Html5.[ ul [ li [ pcdata [%i"Checking the question"] ] ] ] ;
      let str=with_test_lib_prepare
          (test_prel ^ (get_buffer id) ^ "\n" ^
             (Test_spec.question_typed
                (get_a_question id question_id) question_id)) in
      Lwt.async (fun () ->
          Learnocaml_toplevel_worker_caller.check top str >>= fun res ->
          typecheck_dialog_box "check-answer" res);
      Js._true);
  Lwt.return_unit
