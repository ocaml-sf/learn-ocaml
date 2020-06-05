open Test_lib
open Report

module Style_check = Style_checking.Make ()

let forbidden_construct_str =
  "Unable to process your code for style checking because you have used " ^
  "a language construct that is not supported in our course."
let forbidden_construct_msg =
  Message ([Text forbidden_construct_str], Failure)

let test () =
  try
    let tast = Typed_ast_lib.tast_of_parsetree_structure code_ast in
    let checkers = Style_check.all_checkers () in
    Style_check.ast_style_check_structure checkers tast
  with exn -> [forbidden_construct_msg]

(* Generate the report to be shown to the student *)
let () =
  set_result @@
  ast_sanity_check code_ast @@ test
