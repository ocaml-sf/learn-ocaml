
(** Allows typing a question passing by a string
  * @param question_untyped id_question *)
val question_typed : Learnocaml_data.Editor.test_qst_untyped -> int -> string

val compile :  (int * Learnocaml_data.Editor.test_qst_untyped) list -> string
