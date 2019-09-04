open Test_lib
open Report

module Mutation_test = Mutation_test.Make (Test_lib)
open Mutation_test

let test_plus () =
  test_function_2_against_solution
    [%ty : int -> int -> int ] "plus"
    [ (1, 1) ; (2, 2) ; (10, -10) ]
  @
  test_unit_tests_2
    [%ty : int -> int -> int ] "plus"
    [ ("Subtracts instead of adding", 1, fun x y -> x - y) ]

let test_minus () =
  test_function_2_against_solution
    [%ty : int -> int -> int ] "minus"
    [ (1, 1) ; (4, -2) ; (0, 10) ]
  @
  test_unit_tests_2
    [%ty : int -> int -> int ] "minus"
    [ ("Adds instead of subtracting", 1, fun x y -> x + y) ]

let test_times () =
  test_function_2_against_solution
    [%ty : int -> int -> int ] "times"
    [ (1, 3) ; (2, 4) ; (3, 0) ]
  @
  test_unit_tests_2
    [%ty: int -> int -> int ] "times"
    [ ("Divides instead of multiplying", 1, fun x y -> x / y) ]

let test_divide () =
  test_function_2_against_solution
    [%ty : int -> int -> int ] "divide"
    [ (12, 4) ; (12, 5) ; (3, 0) ]
  @
  test_unit_tests_2
    [%ty : int -> int -> int ] "divide"
    [ ("Multiplies instead of dividing", 1, fun x y -> x * y) ]

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ Section
      ([ Text "Function:" ; Code "plus" ],
       test_plus ()) ;
    Section
      ([ Text "Function:" ; Code "minus" ],
       test_minus ()) ;
    Section
      ([ Text "Function:" ; Code "times" ],
       test_times ()) ;
    Section
      ([ Text "Function:" ; Code "divide" ],
       test_divide ()) ]
