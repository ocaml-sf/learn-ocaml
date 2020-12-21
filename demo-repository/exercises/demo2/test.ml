open Test_lib
open Report

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
         [ (12, 4) ; (12, 5) ; (3, 0) ]) ]
