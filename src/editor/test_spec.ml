(* Internationalization *)
let () = Translate.set_lang ()

module type TYPING = sig
  (** Should return a representation of a type from its string serialisation *)
  val ty_of : string -> 'a Ty.ty
end

(*
module Make(Test_lib : Test_lib.S) (Typing : TYPING) = struct

open Test_lib
open Learnocaml_report


(* sampler: (unit -> ('ar -> 'row, 'ar -> 'urow, 'ret) args) *)
(* keep in sync with learnocaml_exercise_state.ml *)
type test_qst_untyped =
  | TestAgainstSol of
      { name: string
      ; ty: string
      ; gen: int
      ; suite: string
      ; tester: string
      ; sampler: string}
  | TestAgainstSpec of
      { name: string
      ; ty: string
      ; gen: int
      ; suite: string
      ; spec : string
      ; tester: string
      ; sampler: string}
  | TestSuite of
      { name: string;
        ty: string;
        suite: string;
        tester : string}

type outcome =
  | Correct of string option
  | Wrong of string option

(* Old code
(* val get_test_qst : test_qst_untyped -> test_qst_typed *)

type test_qst_typed =
  | TestAgainstSol :
      { name: string
      ; prot: (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) prot
      ; tester: 'ret tester option
      ; sampler:(unit -> ('ar -> 'row, 'ar -> 'urow, 'ret) args) option
      ; gen: int
      ; suite: ('ar -> 'row, 'ar -> 'urow, 'ret) args list } -> test_qst_typed
  | TestAgainstSpec :
      { name: string
      ; prot: (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) prot
      ; tester: 'ret tester option
      ; sampler: (unit -> ('ar -> 'row, 'ar -> 'urow, 'ret) args) option
      ; gen: int
      ; suite: ('ar -> 'row, 'ar -> 'urow, 'ret) args list
      ; spec : ('ar -> 'row) -> ('ar -> 'row, 'ar -> 'urow, 'ret) args ->
               'ret -> outcome } -> test_qst_typed
  | TestSuite :
      { name: string
      ; prot: (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) prot
      ; tester: 'ret tester option
      ; suite: (('ar -> 'row, 'ar -> 'urow, 'ret) args *
                  (unit -> 'ret)) list } -> test_qst_typed

(** Notation for TestAgainstSpec *)
let (~~) b = if b then Correct None else Wrong None

This is now in test_lib:
(** Notations for TestSuite *)
let (==>) a b = (a, fun () -> b)
(* let (=>) a b = (a, fun () -> Lazy.force b) (* needs module Lazy *) *)
(** Notations for heterogeneous lists *)
let (@:) a l = arg a @@ l
let (!!) b = last b
let (@:!!) a b = a @: !! b
(* Homogeneous case, for testing purposes
let (@:) a l = a :: l
let (!!) b = b :: []
let (@:!!) a b = a @: !! b
*)
*)

(* TODO missing: nth_arg *)

(*
let example_constr_sol =
  TestAgainstSol
    { name = "opp";
      prot = (last_ty [%ty: int] [%ty: int] );
      gen = 0;
      suite = [!! 0; !! 1; !! 2; !! ~-1]
    }
*)

(*
let example_constr_spec =
  TestAgainstSpec
    { name = "idempotent";
      prot = (last_ty [%ty: (int)] [%ty: int]);
      gen = 0;
      suite = [!! 0; !! 1; !! 2];
      spec = fun f args ret -> (* ret = apply f args *)
      (* Function f should be idempotent *)
      ~~ (ret = apply f (!! ret))
    }

let example_constr_suite =
  TestSuite
    {
      name = "xor";
      prot = (arg_ty [%ty: bool] (last_ty [%ty: bool] [%ty: bool]));
      suite = [false @:!! false ==> false;
               false @:!! true ==> true;
               true @:!! false ==> true;
               true @:!! true ==> false]
    }
*)
end
 *)

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
  let open Learnocaml_exercise_state in
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
     "(* Question #" ^ string_of_int id_question ^ " about " ^ a.name ^ " was not translated\n"
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
