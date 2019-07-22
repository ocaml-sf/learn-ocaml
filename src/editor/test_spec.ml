(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * The main authors of the editor part is the pfitaxel team see 
 * https://github.com/pfitaxel/learn-ocaml-editor for more information
 * 
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Editor_lib

let rec to_string_aux char_list =match char_list with
  | []-> ""
  | c::l -> (string_of_char c) ^ ( to_string_aux l)

(* FIXME: it seems "str" always starts (and sometimes ends) with a space.
   This should be fix so that the space comes from [to_ty] itself. *)
let to_ty str = "[%ty:" ^ str ^ "]"

let parse_type string =
  let char_list_ref = ref (List.rev (decompositionSol string 0)) in
  let para_cpt =ref 0 in
  let esp_cpt= ref 0 in
  (* reverse char_list before using it *)
  let rec last_arg char_list acc =
    match char_list with
      []->char_list_ref:=[];acc
    |elt :: l ->
        if elt = ')' then
          incr para_cpt;
        if elt ='(' then
          decr para_cpt;
        if elt='>' && !para_cpt=0 then
          match l with
            '-'::l2 -> char_list_ref:=l2;acc
          |_ -> failwith "toto"
        else
          begin
            if !esp_cpt=0 && elt=' ' then
              begin
                esp_cpt:=1;
                last_arg l ( elt::acc )
              end
            else
              begin
                if elt<>' ' then
                  begin
                    esp_cpt:=0;
                    last_arg l (elt::acc)
                  end
                else
                  last_arg l acc
              end
          end in
  let init_acc () =
    let arg1=last_arg (!char_list_ref ) [] in
    let arg2=last_arg (!char_list_ref)  [] in
    let ty1=to_ty (to_string_aux arg1) in
    let ty2=to_ty (to_string_aux arg2) in
    "last_ty "^ty2^" "^ty1 in
  let acc =ref (init_acc ()) in
  while !char_list_ref <>[] do
    let arg=last_arg (!char_list_ref) [] in
    let ty= to_ty (to_string_aux arg) in
    acc:="arg_ty "^ty^" ("^(!acc)^")" ;
  done;
  !acc;;

(* The tester arg could take into account exceptions/sorted lists/etc. *)
let question_typed question id_question =
  let open Learnocaml_data.Editor in
  let opt_string param = function
    | "" -> ""
    | v -> Format.sprintf " ~%s:(%s)" param v
  and sampler_args = function
    | "" -> ""
    | f -> Format.sprintf "fun () -> last ((%s) ())" f
  in
  match question with
  | TestAgainstSpec a ->
     (* FIXME *)
     "(* Question #" ^ " about " ^ a.name ^ " was not translated\n"
     ^ "(TestAgainstSpec not currently supported by the learn-ocaml runtime) *)"
  | TestSuite a ->
     let name, prot, tester, suite =
       a.name, parse_type a.ty, opt_string "test" a.tester, a.suite in
     Format.sprintf "let question%d =@.  \
                     let prot = %s in@.  \
                     test_function%s prot@.  \
                     (lookup_student (ty_of_prot prot) %s)@.  \
                     %s;;@."
       id_question prot tester name suite
  | TestAgainstSol a ->
     let name = a.name
     and prot = parse_type a.ty
     and gen = a.gen
     and sampler = opt_string "sampler" (sampler_args a.sampler)
     and tester = opt_string "test" a.tester
     and suite = a.suite
     in
     Format.sprintf "let question%d =@.  \
                     let prot = %s in@.  \
                     test_function_against_solution ~gen:(%d)%s%s prot@.  \
                     \"%s\"@.  \
                     %s;;@."
     id_question prot gen sampler tester name suite

(*****************)
(* compile stuff *)
(*****************)

open Learnocaml_data.Editor

let test_prel = "open Test_lib\nopen Learnocaml_report;;\n"


let quality_function = {|
let avoid_thentrue = let already = ref false in fun _ ->
  if !already then [] else begin
    already := true ;
    Learnocaml_report.[ Message ([ Text "* Do not write the following code patterns:";
                                   Code "[if ... then true else ...;
 if ... then false else ...;
 if ... then ... else true;
 if ... then ... else false]"; Text "
Preferably use Boolean operators (&&), (||), not."], Success ~-4) ]
  end

let check_thentrue e =
    Parsetree.(
      match e with
      | {pexp_desc = Pexp_ifthenelse (_, e1, (Some e2))} ->
         begin
           match e1 with
           | {pexp_desc = Pexp_construct ({Asttypes.txt = (Longident.Lident "false")}, None)}
           | {pexp_desc = Pexp_construct ({Asttypes.txt = (Longident.Lident "true")}, None)} ->
              avoid_thentrue e1
           | _ -> []
         end @ begin
           match e2 with
           | {pexp_desc = Pexp_construct ({Asttypes.txt = (Longident.Lident "false")}, None)}
           | {pexp_desc = Pexp_construct ({Asttypes.txt = (Longident.Lident "true")}, None)} ->
             avoid_thentrue e2
           | _ -> []
          end
      | _ -> [])

let avoid_list1app = let already = ref false in fun _ ->
  if !already then [] else begin
    already := true ;
    Learnocaml_report.[ Message ([ Text "* Do not write:";
                                   Code "[x] @ l";
                                   Text ". Preferably write:";
                                   Code "x :: l";
                                   Text "."], Success ~-4) ]
  end

let check_list1app e =
  Parsetree.(
    match e.pexp_desc with
    | Pexp_apply (app0, [(_, lst1); _]) ->
       (match app0.pexp_desc, lst1.pexp_desc with
        | Pexp_ident {Asttypes.txt = app0'},
          Pexp_construct ({Asttypes.txt = (Longident.Lident "::")}, Some lst1')
             when List.mem (Longident.flatten app0') [["List"; "append"]; ["@"]] ->
           (match lst1'.pexp_desc with
            | Pexp_tuple [_; nil0] ->
               (match nil0.pexp_desc with
                | Pexp_construct ({Asttypes.txt = (Longident.Lident "[]")}, None) ->
                   avoid_list1app e
                | _ -> [])
            | _ -> [])
        | _ -> [])
    | _ -> [])

let avoid_eqphy = let already = ref false in fun _ ->
  if !already then [] else begin
    already := true ;
    Learnocaml_report.[ Message ([ Text "*  Do not use physical equality";
                                   Code "(==)";
                                   Text ". Preferably use structural equality";
                                   Code "(=)";
                                   Text "."], Success ~-1) ]
  end

let avoid_neqphy = let already = ref false in fun _ ->
  if !already then [] else begin
    already := true ;
    Learnocaml_report.[ Message ([ Text "* Do not use physical inequality";
                                   Code "(!=)";
                                   Text ". Preferably use structural inequality";
                                   Code "(<>)";
                                   Text "."], Success ~-1) ]
  end

let check_eqphy e =
  Parsetree.(
    match e.pexp_desc with
    | Pexp_ident {Asttypes.txt = Longident.Lident "=="} -> avoid_eqphy e
    | _ -> [])

let check_neqphy e =
  Parsetree.(
    match e.pexp_desc with
    | Pexp_ident {Asttypes.txt = Longident.Lident "!="} -> avoid_neqphy e
    | _ -> [])
|}

let imperative_function = {|let ast_imperative_check ast =
  let chk_expr e =
    Parsetree.(
      match e with
      | {pexp_desc = Pexp_sequence _} -> forbid_syntax ";" e
      | {pexp_desc = Pexp_while _} -> forbid_syntax "while" e
      | {pexp_desc = Pexp_for _} -> forbid_syntax "for" e
      | {pexp_desc = Pexp_array _} -> forbid_syntax "array" e
      | _ -> [] ) in
  let imperative_report =
    ast_check_structure
      ~on_expression:chk_expr
      ast |> List.sort_uniq compare in
  if snd (Learnocaml_report.result imperative_report) then
    imperative_report
  else
    []|}


let ast_fonction quality imperative  =
  let fonction = if quality then
                   quality_function
                 else
                   "" in
  let fonction = if imperative then
                   fonction ^ imperative_function
                 else
                   fonction ^ "" in
  let fonction = fonction ^ "\n\nlet ast_quality ast =" in
  let fonction =
    if imperative then
      fonction ^ {|
                  let imperative_report =
                  let tempReport = ast_imperative_check ast in
                  if tempReport = [] then []
                  else (Message ([ Text "Imperative features have been detected:" ],
                  Success ~-4)) :: tempReport
                  |}
    else
      fonction ^ {|
                  let imperative_report = []
                  |} in
  let fonction =
    if quality then
      fonction ^ {|
                  and report =
                  let tempReport = ast_check_structure
                  ~on_expression:(check_thentrue @@@ check_list1app @@@
                  check_eqphy @@@ check_neqphy)
                  ast |> List.sort_uniq compare in
                  if tempReport = [] then []
                  else (Message ([Text "Unwanted code patterns have been detected:"],
                  Failure)) :: tempReport
                  |}
    else fonction ^ " and report = []" in
  let fonction = fonction ^ {|
                             in if imperative_report = [] && report = []
                             then [ Message ([ Text "OK (no prohibited construction detected)"], Success 0) ]
                             else imperative_report @ report;;

                             |} in
  fonction

let ast_code quality imperative =
let fonction =
    if quality || imperative then
      {|Section ([ Text "Code quality:" ], ast_quality code_ast);
       |}
    else
      "" in
  fonction

  let compile indexed_list =
    let tests = test_prel ^ (ast_fonction true true) in
    let tests = List.fold_left (fun acc (qid,quest) ->
          acc ^ (question_typed quest qid)^" \n")
         tests indexed_list in
    let tests = tests ^ init ^ "[\n" ^ ast_code true true in
    let tests =
      List.fold_left (fun acc (qid, quest) ->
          let name=match quest with
            | TestAgainstSol a->a.name
            | TestAgainstSpec a ->a.name
            | TestSuite a -> a.name in
          acc ^ (section name ("question" ^ string_of_int qid ) ))
         tests indexed_list in
    tests ^ " ]"
