open Test_lib
open Report

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ Section
      ([ Text "Function:" ; Code "aff" ],
       let prot = [%funty: int -> int -> int -> int] in
       test_function_against_solution ~gen:(0) prot "aff"
         [2 @: (-1) @:!! 5;
          (-2) @: 1 @:!! 5])
  ]
