open Test_lib
open Report

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ Section
      ([ Text "Function:" ; Code "rev" ],
       test_function_1_against_solution ~gen:0
         [%ty : int list -> int list ] "rev"
         [ [3;5;9] ; [] ; [3;5;9;8;5];[9];[7;5] ] )];
