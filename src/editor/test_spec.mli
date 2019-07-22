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

(** Allows typing a question passing by a string
  * @param question_untyped id_question *)
val question_typed : Learnocaml_data.Editor.test_qst_untyped -> int -> string

val compile :  (int * Learnocaml_data.Editor.test_qst_untyped) list -> string
