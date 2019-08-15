open Test_lib
open Report

let sample_natural () = sample_int () + 5 (* sample_int generates -5 to 5 *)
let sample_natural_pair () = (sample_natural (), sample_natural ())

let forbidden_construct_str =
  "Unable to process your code for tail recursion checking because you " ^
  "have used a language construct that is not supported in our course."
let forbidden_construct_msg =
  Message ([Text forbidden_construct_str], Failure)

(* Scaling the grades in a report by a given (integer) factor *)
let rec scale factor report = List.map (scale_item factor) report
and scale_item factor item =
  match item with
  | Section (text, t) -> Section (text, scale factor t)
  | Message (text, Success n) -> Message (text, Success (factor * n))
  | _ -> item

let tailrec_info_msg name =
  Message (
      [Text "Checking that"; Code name; Text "is tail-recursive"],
      Informative
    )

let check_tailrec tast name =
  let open Typed_ast_lib in
  find_binding tast name @@ fun _ -> check_tailcalls ~points: 0

let nonrec_failure_msg name =
  Message (
      [Code name;
       Text "should not be recursive.";
       Text "Check that you are not using";
       Code "let rec"],
      Failure
    )

let nonrec_success_msg name =
  Message (
      [Text "Verified that"; Code name; Text "is not recursive"],
      Important
    )

let check_nonrec tast name =
  let open Typed_ast_lib in
  find_binding tast name @@
  fun rf _ _ ->
    match rf with
    | Asttypes.Nonrecursive -> [nonrec_success_msg name]
    | Asttypes.Recursive -> [nonrec_failure_msg name]

let test_fast_exp_tl_tailrec () =
  let open Typed_ast_lib in
  try
    let tast = tast_of_parsetree_structure code_ast in
    let aux_report = check_tailrec tast "fast_exp_aux" in
    let tl_report = check_nonrec tast "fast_exp_tl" in
    tailrec_info_msg "fast_exp_aux" :: (aux_report @ tl_report)
  with exn -> [forbidden_construct_msg]

let tailrec_failure_msg =
  Message (
      [Text "Your function was not tail recursive.";
       Text "Correctness tests are worth 0 points until this is fixed."],
      Failure
    )

let test_fast_exp () =
  let report =
    test_function_2_against_solution
      [%ty: int -> int -> int] "fast_exp"
      ~sampler: sample_natural_pair
      []
  in
  [Section ([Text "Question 1:"; Code "fast_exp"], report)]

let test_fast_exp_tl () =
  let tailrec_report = test_fast_exp_tl_tailrec () in
  let test_report =
    test_function_2_against_solution
      [%ty: int -> int -> int] "fast_exp_tl"
      ~sampler: sample_natural_pair
      []
  in
  let test_report =
    if snd (Report.result tailrec_report) then
      tailrec_failure_msg :: scale 0 test_report
    else
      test_report
  in
  let report = tailrec_report @ test_report in
  [Section ([Text "Question 2:"; Code "fast_exp_tl"], report)]

(* Generate the report to be shown to the student *)
let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
    test_fast_exp () @ test_fast_exp_tl ()
