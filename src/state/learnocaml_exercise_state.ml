(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2016 OCamlPro.
 *
 * Learn-OCaml is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Learn-OCaml is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. *)


open Json_encoding;;
open Learnocaml_index;;


type exercise_state =
  { solution : string ;
    grade : int option ;
    report : Learnocaml_report.t option ;
    mtime : float }

let exercise_state_enc =
  let grade_enc =
    conv
      (fun s -> s)
      (fun s ->
         if s < 0 || s > 100 then
           raise (Cannot_destruct ([], Failure "grade overflow"))
         else s)
      int in
  conv
    (fun { grade ; solution ; report ; mtime } ->
       (grade, solution, report, mtime))
    (fun (grade, solution, report, mtime) ->
       { grade ; solution ; report ; mtime })
    (obj4
       (opt "grade" grade_enc)
       (req "solution" string)
       (opt "report" Learnocaml_report.report_enc)
       (dft "mtime" float 0.));;


type index_state =
  {
    exos: exercise Map.Make (String).t ;
        mtime : float
  }
let index_state_enc = conv (fun {exos; mtime} -> (exos, mtime))
  (fun (exos, mtime) -> {exos; mtime})
  (obj2 (req "exercises" (map_enc exercise_enc)) (dft "mtime" float 0.));;
type type_question= Suite | Solution | Spec ;;

(* previous version of storing questions may be useful

type type_question= Suite | Solution | Spec ;;

type question_state =
  {name:string;
   ty :string;
   type_question : type_question;
   input :string;
   output:string;
   extra_alea:int;
   datalist:string;
  }

let question_state_enc =
  conv
    (fun {name; ty; type_question; input; output; extra_alea; datalist}->
       (name, ty, type_question, input, output, extra_alea, datalist)
    )
    (fun (name, ty, type_question, input, output, extra_alea, datalist)->
       {name; ty; type_question; input; output; extra_alea; datalist}
    )
    (obj7
       (req "name" string)
       (req "ty" string)
       (req "type_question"
         ( string_enum ["suite",Suite;"spec",Spec;"solution",Solution] ) )
       (req "input" string)
       (req "output" string)
       (req "extra_alea" int)
       (req "datalist" string)
    );;
*)

type test_qst_untyped =
  | TestAgainstSol of
      { name: string
      ; ty: string
      ; gen: int
      ; suite: string
      ; tester: string
      ; sampler : string }
  | TestAgainstSpec of
      { name: string
      ; ty: string
      ; gen: int
      ; suite: string
      ; spec : string
      ; tester: string
      ; sampler: string }
  | TestSuite of
      { name: string;
        ty: string;
        suite: string;
        tester :string };;
type a_sol =
  { name: string
  ; ty: string
  ; gen: int
  ; suite: string
  ; tester: string
  ; sampler : string }
let test_against_sol_enc =
  conv
    (fun {name; ty; gen; suite; tester; sampler} ->
       (name, ty, gen, suite, tester, sampler)
    )
    (fun (name, ty, gen, suite, tester, sampler) ->
       {name; ty; gen; suite; tester; sampler}
    )
    (obj6
       (req "name" string)
       (req "ty" string)
       (req "gen" int )
       (req "suite" string)
       (req "tester" string)
       (req "sampler" string)
    );;
type a_spec=
  { name: string
  ; ty: string
  ; gen: int
  ; suite: string
  ; spec : string
  ; tester: string
  ; sampler : string}
let test_against_spec_enc =
  conv
    (fun {name; ty; gen; suite;spec; tester; sampler}->
       (name, ty, gen, suite,spec, tester, sampler)
    )
    (fun (name, ty, gen, suite, spec,tester, sampler)->
       {name; ty; gen; suite; spec;tester; sampler}
    )
    (obj7
       (req "name" string)
       (req "ty" string)
       (req "gen" int )
       (req "suite" string)
       (req "spec" string)
       (req "tester" string)
       (req "sampler" string)
    );;
type suite=
  { name: string
  ; ty: string
  ; suite: string
  ; tester: string }

let test_suite_enc =
  conv
    (fun {name; ty; suite; tester}->
       (name, ty,  suite, tester)
    )
    (fun (name, ty, suite, tester)->
       {name; ty;  suite; tester}
    )
    (obj4
       (req "name" string)
       (req "ty" string)
       (req "suite" string)
       (req "tester" string)
    );;

let test_qst_untyped_enc =union [
    case
      test_against_sol_enc
      (function TestAgainstSol {name; ty; gen; suite; tester; sampler} ->
       Some {name; ty; gen; suite; tester; sampler} | _ -> None)
      (fun {name; ty; gen; suite; tester; sampler} ->
       TestAgainstSol {name; ty; gen; suite; tester; sampler});
    case
      test_against_spec_enc
      (function TestAgainstSpec {name; ty; gen; suite;spec; tester; sampler} ->
       Some {name; ty; gen; suite;spec; tester; sampler} | _ -> None)
      (fun {name; ty; gen; suite;spec; tester; sampler} ->
       TestAgainstSpec {name; ty; gen; suite;spec; tester; sampler} );
    case
      test_suite_enc
      (function TestSuite {name; ty; suite; tester} ->
       Some {name; ty; suite; tester} | _ -> None)
      (fun {name; ty; suite; tester}  ->TestSuite {name; ty; suite; tester} );
  ]
;;

module IntMap =
  Map.Make(struct type t = int let compare (x:t) y = compare x y end)

type test_state = { testml: string;
                    testhaut: test_qst_untyped IntMap.t }

(** Useful for serialization *)
let string_of_int_map iv =
  let module StringMap = Map.Make(String) in
  IntMap.fold (fun i v sv -> StringMap.add (string_of_int i) v sv) iv StringMap.empty

let int_of_string_map iv =
  let module StringMap = Map.Make(String) in
  StringMap.fold (fun s v iv -> IntMap.add (int_of_string s) v iv) iv IntMap.empty

let testhaut_enc = map_enc test_qst_untyped_enc

let test_state_enc =
  conv
    (fun { testml; testhaut } -> (testml, string_of_int_map testhaut))
    (fun (testml, testhautstr) ->
      let testhaut = int_of_string_map testhautstr in
      { testml; testhaut })
    (obj2
       (req "testml" string)
       (req "testhaut" testhaut_enc)
    )

type metadata =
  { id : string;
    titre : string;
    description : string;
    diff : float;
  }

let metadata_enc = conv
    (fun {id;titre;description;diff} -> (id,titre,description,diff))
    ( fun (id,titre,description,diff) -> {id;titre;description;diff})
    (obj4
       (req "id" string)
       (req "titre" string )
       (req "description" string)
       (req "diff" float )
    )
type checkbox=
  { imperative : bool;
    undesirable : bool}

let checkbox_enc = conv
    (fun {imperative;undesirable} -> (imperative,undesirable) )
    (fun (imperative,undesirable) -> {imperative;undesirable} )
    (obj2
       (req "imperative" bool )
       (req "undesirable" bool )
    )

type editor_state =
  { metadata : metadata;
    prepare : string;
    solution : string ;
    question : string ;
    template : string ;
    test : test_state ;
    prelude : string;
    incipit : string ;
    checkbox : checkbox;
    mtime : float }

let editor_state_enc =
  conv
    (fun {metadata; prepare; solution; question; template;
          test; prelude; incipit; checkbox; mtime } ->
      (metadata, prepare, solution, question, template,
       test, prelude, incipit, checkbox, mtime))
    (fun (metadata, prepare, solution, question, template,
          test, prelude, incipit, checkbox, mtime) ->
      {metadata; prepare; solution; question; template;
       test; prelude; incipit; checkbox; mtime})
    (obj10
       (req "metadata" metadata_enc)
       (req "prepare" string)
       (req "solution" string)
       (req "question" string)
       (req "template" string)
       (req "test" test_state_enc )
       (req "prelude" string)
       (req "incipit" string)
       (req "checkbox" checkbox_enc )
       (dft "mtime" float 0.))
