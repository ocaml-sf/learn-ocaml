(* Internationalization *)
let () = Translate.set_lang ()

module type TYPING = sig
  (** Should return a representation of a type from its string serialisation *)
  val ty_of : string -> 'a Ty.ty
end

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

(* TODO val get_test_qst : test_qst_untyped -> test_qst_typed *)

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


let local_dummy : 'a sampler = fun () -> failwith "dummy sampler"
(* à n'utiliser que si on passe l'argument ~gen:0 (pas d'alea) *)

let test_question (t : test_qst_typed) =
  match t with
  | TestAgainstSol t ->
     let tester = match t.tester with
       | None -> test
       | Some s -> s in
     if t.gen = 0 then
       begin
         test_function_against
         ~gen:t.gen ~sampler:local_dummy
         ~test:tester (* could take into account exceptions/sorted lists/etc. *)
         t.prot
         (lookup_student (ty_of_prot t.prot) t.name)
         (lookup_solution (ty_of_prot t.prot) t.name)
         t.suite
       end
     else
       begin
         match t.sampler with
         | None -> (test_function_against
                   ~gen:t.gen
                   ~test:tester
                   (* could take into account exceptions/sorted lists/etc. *)
                   t.prot
                   (lookup_student (ty_of_prot t.prot) t.name)
                   (lookup_solution (ty_of_prot t.prot) t.name)
                   t.suite)
         | Some s -> (test_function_against
                     ~gen:t.gen ~sampler:s
                     ~test:tester
                     (* could take into account exceptions/sorted lists/etc. *)
                     t.prot
                     (lookup_student (ty_of_prot t.prot) t.name)
                     (lookup_solution (ty_of_prot t.prot) t.name)
                     t.suite)
       end
  | TestAgainstSpec t ->
     let to_string ty v = Format.asprintf "%a" (typed_printer ty) v in
     let stud = lookup_student (ty_of_prot t.prot) t.name in
     test_value stud @@ fun uf ->
     (* no sampler for the moment *)
     let open Learnocaml_report in
     List.flatten @@ List.map (fun args ->
       let code =
         Format.asprintf "@[<hv 2>%s,%a@]" t.name (print t.prot) args in
       let ret_ty = get_ret_ty (ty_of_prot t.prot) args in
       Message ([ Text "Checking spec for" ; Code code ], Informative) ::
         let ret = apply uf args in
         let value = to_string ret_ty ret in
       let (text, note) = match t.spec uf args ret with
         | Correct None -> ("Correct spec", Success 1)
         | Correct (Some message) -> (message, Success 1)
         | Wrong None -> ("Wrong spec", Failure)
         | Wrong (Some message) -> (message, Failure) in
       [Message ([Text "Got value"; Code value; Text (": " ^ text)], note)])
     t.suite
  | TestSuite t ->
     let test = match t.tester with
       | None -> test
       | Some s -> s in
     test_function
       ~test:test (* could take into account exceptions/sorted lists/etc. *)
       t.prot
       (lookup_student (ty_of_prot t.prot) t.name)
       t.suite
end

open Editor_lib

let rec to_string_aux char_list =match char_list with
  | []-> ""
  | c::l -> (string_of_char c) ^ ( to_string_aux l)

let to_ty str = "(to_ty \""^str^"\" )"

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

let question_typed question id_question =
  let open Learnocaml_exercise_state in
  let name,ty,input,extra_alea,output,type_question,tester,sampler =
    match question with
    | TestAgainstSol a -> a.name, a.ty, a.suite, a.gen, "",
                          Solution, a.tester, a.sampler
    | TestAgainstSpec a -> a.name, a.ty, a.suite, a.gen, a.spec,
                           Spec, a.tester, a.sampler
    | TestSuite a -> a.name, a.ty, a.suite, 0, "", Suite, a.tester, "" in
  let tester = match tester with
    | "" -> "None"
    | s -> "Some ("^s^")" in
  let sampler = match sampler with
    | "" -> "None"
    | s -> "Some (fun () -> last ("^s^"()))" in
  let acc=(match type_question with
           | Suite -> "\nlet question"^id_question^" =  TestSuite {name=\""^name^"\"; prot="^(parse_type ty)^"; tester="^tester^"; suite="^input^"}"
           | Solution -> "\nlet question"^id_question^" = TestAgainstSol {name=\""^name^"\"; prot="^(parse_type ty)^"; tester="^tester^"; sampler="^sampler^"; gen="^(string_of_int extra_alea)^"; suite="^input^"}"
           | Spec -> "\nlet question"^id_question^" = TestAgainstSpec {name=\""^name^"\"; prot="^(parse_type ty)^"; tester="^tester^"; sampler="^sampler^"; gen="^(string_of_int extra_alea)^"; suite="^input^"; spec="^output^"}") in
  acc
;;

