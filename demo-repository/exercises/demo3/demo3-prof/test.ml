open Test_lib
open Report

module Mutation_test = Mutation_test.Make (Test_lib)
open Mutation_test

let test_plus () =
  test_function_2_against_solution
    [%ty : int -> int -> int ] "plus"
    [ (1, 1) ; (2, 2) ; (10, -10) ]
    (*  @
    test_unit_tests_2
      [%ty : int -> int -> int ] "plus"
      [ ("Subtracts instead of adding", 1, fun x y -> x - y) ] *)

 (* Au dessus (en commentaire) : test des tests de l'élève *)

let test_minus () =
  test_function_2_against_solution
    [%ty : int -> int -> int ] "minus"
    [ (1, 1) ; (4, -2) ; (0, 10) ]
    

let test_times () =
  test_function_2_against_solution
    [%ty : int -> int -> int ] "times"
    [ (1, 3) ; (2, 4) ; (3, 0) ]
    

let test_divide () =
  test_function_2_against_solution
    [%ty : int -> int -> int ] "divide"
    [ (12, 4) ; (12, 5) ; (3, 0) ]
    
   

let test_sum3 () =
  test_function_against_solution
    ~gen:2                  (* only 2 random test cases *)
    [%funty: int -> int -> int -> int] (* function type *)
    "sum3"                             (* function name *)
    (* list of additional, explicit test cases *)
    [ 10 @: 20 @:!! 30 ;
      -1 @: -2 @:!! -3 ]

let test_plus2 () =
  test_function_1_against_solution
    [%ty:  int -> int] (* function type *)
    "plus2"
    ~gen:2                  
    [ 12;6 ]


let test_sum_0_to_n () =
  test_function_1_against_solution
    [%ty : int -> int ]
    "sum_0_to_n" 
    ~gen:4
    ~test:(test_eq_exn
             (fun exn1 exn2 -> match exn1, exn2 with
                  Failure _, Failure _ -> true | _, _ -> false))
    [-4]

let test_fact () =
  Section ([ Text "Function" ; Code "fact"],
           test_function_1_against_solution
             [%ty : int -> int ]
             "fact"
             ~sampler:(fun () -> Random.int 11)
             ~gen:4
             []
          )





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
       test_divide ()) ;
    Section
      ([ Text "Function:" ; Code "sum3" ],
       test_sum3 ()) ;
    Section
      ([ Text "Function:" ; Code "plus2"],
       test_plus2 ());
    Section
      ([ Text "Function:" ; Code "sum_0_to_n"],
       test_sum_0_to_n ());
    
    test_fact ();
    
  ]
