(** Getters of an editor exercise
  * @param the id *) 
val get_titre : string -> string
val get_description : string -> string
val get_diff : string -> float
val get_solution : string -> string
val get_question : string -> string
val get_template : string -> string
val get_testml : string -> string
val get_testhaut : string -> Learnocaml_exercise_state.test_qst_untyped Map.Make(String).t
val get_prelude : string -> string
val get_prepare : string -> string
val get_imperative : string -> bool
val get_undesirable : string -> bool
val get_buffer : string -> string
(** Getters of a question of an editor exercise
 * @param exercise_id question_id *)
val get_a_question : string -> Map.Make(String).key ->Learnocaml_exercise_state.test_qst_untyped
val get_ty : string -> Map.Make(String).key -> string
val get_name_question : string -> Map.Make(String).key -> string
val get_type_question : string -> Map.Make(String).key -> Learnocaml_exercise_state.type_question
val get_extra_alea : string -> Map.Make(String).key -> int
val get_input : string -> Map.Make(String).key -> string
val get_spec : string -> Map.Make(String).key -> string

(** Question ids are integers stored in strings *)

(** Compute the smallest integer not used yet *)                             
val compute_question_id : 'a Map.Make(String).t -> string

(** Setter of testhaut
  * @param new_StringMap exercise_id *)  
val save_testhaut : Learnocaml_exercise_state.test_qst_untyped Map.Make(String).t -> string -> unit

val with_test_lib_prepare :string->string

(** Remove an exercise from the local storage *)
val remove_exo : Map.Make(String).key -> unit

(** @return a bool depending on whether the id is already used or not *)
val idUnique : string -> bool
(** @return a bool depending on whether the title is already used or not *)
val titleUnique : string -> bool

(** Store an exercise in the dynamic index of editor exercises *)
val store_in_index : Learnocaml_exercise_state.metadata -> unit

(** arguments Dom element , string *)
val setInnerHtml : < innerHTML : < set : Js.js_string Js.t -> unit; .. >
                   Js_of_ocaml.Js.gen_prop; .. > Js_of_ocaml.Js.t -> string -> unit

(** Trick to call the recovering function outside of it definition enveroniment *)
val recovering_callback : (unit -> unit) ref

(** Create the testhaut pane blindfolds *)
val testhaut_init : [< Html_types.div ] Tyxml_js.Html5.elt -> string -> unit Lwt.t

(** Fragment of a test.ml code
  * @see definition *)
val init : string

(** Create the code of a section
  * @param name_of_the_function associated_report *)
val section : string -> string -> string

val string_of_char : char -> string

(** @param content_of_the_toplevel [[]]
  * @return a list
  * The first value is the type of the first val, etc. *)
val get_all_val : char list -> char list list -> char list list

(** Remove atomic values from a list of types
  * @return a list of type of function (var function_name : type = <fun>)
  * @param content_of_the_toplevel [[]] result_list  (second parameter must be []) *)
val get_only_fct : char list -> char list -> char list

(** Associate each function with its type
  * @ return a list of couple (function_name, function_type)
  * @ param content_of_the_toplevel result_list (second param must be []) *)
val get_questions : char list list -> (string * string) list -> (string * string) list

(** Create the corresponding char list of a string (second parameter must be 0) *)
val decompositionSol : string -> int -> char list

(** Create a list of triples (key, alea, "monorphic type"):
    polymorph_detector [("f", "'a -> 'b"); ("p", "int -> int")] =
    [("f", 5, "int -> bool"); ("f", 5, "bool -> char"); ("p", 10, "int -> int")] *)
val polymorph_detector : ('a * string) list -> ('a * int * string) list

(** Create the template of the solution *)
val genTemplate : string -> string

(** Refacoring of typecheck functions *)
val typecheck : bool -> 'a Ace.editor -> Ocaml_mode.editor -> Learnocaml_toplevel.t -> unit Lwt.t
val typecheck_spec : bool -> 'a Ace.editor -> Ocaml_mode.editor -> Learnocaml_toplevel.t -> unit Lwt.t


(** Create an exercise with the data of the local storage
  * @param editor_exercise_id *)
val exo_creator : string -> Learnocaml_exercise.t

(** @return the output of toplevel buffer *) 
val get_answer : Learnocaml_toplevel.t -> string
val typecheck_dialog_box : string-> 'a Toploop_results.toplevel_result -> unit Lwt.t
val test_prel :string 

