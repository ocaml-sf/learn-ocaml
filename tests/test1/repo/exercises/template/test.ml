open Test_lib
open Report

let exercise_1 =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "solution" ],
           test_function_1_against_solution
             [%ty: string -> string] "solution"
             [])

let exercise_2 =
  set_progress "Grading exercise 2." ;
  Section ([ Text "Exercise 2: " ; Code "solution" ],
           test_function_1_against_solution
             [%ty: int -> string] "solution"
             [])

let exercise_3 =
  set_progress "Grading exercise 3." ;
  Section ([ Text "Exercise 3: " ; Code "solution" ],
           test_function_1_against_solution
             [%ty: bool -> string] "solution"
             [])

let exercise_4 =
  set_progress "Grading exercise 4." ;
  Section ([ Text "Exercise 4: " ; Code "solution" ],
           test_function_1_against_solution
             [%ty: float -> string] "solution"
             [])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ; exercise_3 ; exercise_4 ]
