open Test_lib
open Report

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ Section
      ([ Text "Function:" ; Code "foo" ], 
        find_binding code_ast "foo" (fun _ -> [Message ([Text "Found"], Success 1)]));
   ]
