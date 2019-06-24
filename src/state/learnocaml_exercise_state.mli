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

type exercise_state =
  { solution : string ;
    grade : int (* in [0, 100] *) option ;
    report : Learnocaml_report.t option ;
    mtime : float }

val exercise_state_enc : exercise_state Json_encoding.encoding

type type_question = Suite | Solution | Spec ;;

type test_qst_untyped =
  | TestAgainstSol of
      { name: string
      ; ty: string 
      ; gen: int
      ; suite: string
      ; tester: string
      ; sampler: string }
  | TestAgainstSpec of
      { name: string
      ; ty: string
      ; gen: int
      ; suite: string
      ; spec : string
      ; tester: string
      ; sampler: string }
  | TestSuite of
      { name: string
      ; ty: string
      ; suite: string
      ; tester: string } ;;

module IntMap : Map.S with type key = int

type test_state = { testml: string;
                    testhaut: test_qst_untyped IntMap.t }

val string_of_int_map : 'a IntMap.t -> 'a Map.Make(String).t
val int_of_string_map : 'a Map.Make(String).t -> 'a IntMap.t

val testhaut_enc : test_qst_untyped Map.Make (String).t Json_encoding.encoding
    
type metadata =
  { id : string;
    titre : string;
    description : string;
    diff : float
  }

type checkbox =
  { imperative : bool;
    undesirable : bool}

type editor_state =
  { metadata : metadata;    
    prepare : string;
    solution : string;
    question : string;
    template : string;
    test : test_state;
    prelude : string;
    incipit : string ;
    checkbox : checkbox;
    mtime : float }

val editor_state_enc : editor_state Json_encoding.encoding
open Learnocaml_index
type index_state =
  {
     exos : Learnocaml_index.exercise Map.Make(String).t;
     mtime : float;
  }

val index_state_enc : index_state Json_encoding.encoding
