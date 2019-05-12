open Test_lib
open Report
open Typed_ast_lib

module Mutation_test = Mutation_test.Make (Test_lib)
module Style_check = Style_checking.Make ()

(* Generating the Typed_ast *)
type typed_ast_result =
  | Ok of Typed_ast.structure
  | Error of exn
let typed_ast =
  try Ok (Typed_ast_lib.tast_of_parsetree_structure code_ast)
  with exn -> Error exn

(* Style checking *)
let checkers = Style_check.all_checkers ~max_if_cases: 3 ()
let style_report () =
  match typed_ast with
  | Ok tast -> Style_check.ast_style_check_structure checkers tast
  | _ -> []

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ Section
      ([ Text "Function:" ; Code "plus" ],
       test_function_2_against_solution
         [%ty : int -> int -> int ] "plus"
         [ (1, 1) ; (2, 2) ; (10, -10) ]) ;
    Section
      ([ Text "Function:" ; Code "minus" ],
       test_function_2_against_solution
         [%ty : int -> int -> int ] "minus"
         [ (1, 1) ; (4, -2) ; (0, 10) ]) ;
    Section
      ([ Text "Function:" ; Code "times" ],
       test_function_2_against_solution
         [%ty : int -> int -> int ] "times"
         [ (1, 3) ; (2, 4) ; (3, 0) ]) ;
    Section
      ([ Text "Function:" ; Code "divide" ],
       test_function_2_against_solution
         [%ty : int -> int -> int ] "divide"
         [ (12, 4) ; (12, 5) ; (3, 0) ]);

    Section
      ([ Text "Mutation tests:" ; Code "plus" ],
      Mutation_test.test_unit_tests_2
        [%ty: int -> int -> int]
        "plus"
        [(fun x y -> x);
         (fun x y -> y)]);
  ]
  @
  style_report ()
