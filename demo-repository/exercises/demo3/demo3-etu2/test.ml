open Test_lib
open Report

module Mutation_test = Mutation_test.Make (Test_lib)
open Mutation_test

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
