open Test_lib
open Report
open Typed_ast_lib

(* Random samplers for binary trees *)
let sample_tree () =
  let rec builder h = match h with
    | 0 -> Empty
    | n -> match Random.int 3 with
      | 0 -> Empty
      | _ -> Node (builder (h - 1), sample_int (), builder (h - 1))
  in
  let h = Random.int 5 + 2 in
  builder h

let sample_single_node () = Node (Empty, sample_int (), Empty)

(* Scaling the grades in a report by a given (integer) factor *)
let rec scale factor report = List.map (scale_item factor) report
and scale_item factor item =
  match item with
  | Section (text, t) -> Section (text, scale factor t)
  | Message (text, Success n) -> Message (text, Success (factor * n))
  | _ -> item

module Mutation_test = Mutation_test.Make (Test_lib)

(* Mutants for mutation testing *)
let rec size_always_0 t =
  match t with
  | Empty -> 0
  | Node (l, _, r) -> (size_always_0 l) + (size_always_0 r)
let rec size_duplicates_l t =
  match t with
  | Empty -> 0
  | Node (l, _, _) -> 1 + (size_duplicates_l l) + (size_duplicates_l l)
let rec size_ignores_l t =
  match t with
  | Empty -> 0
  | Node (_, _, r) -> 1 + (size_ignores_l r)
let rec correct_size t =
  match t with
  | Empty -> 0
  | Node (l, _, r) -> 1 + (correct_size l) + (correct_size r)
let rec size_wrong_base t =
  match t with
  | Empty -> 1
  | Node (l, _, r) -> 1 + (correct_size l) + (correct_size r)
let size_mutants = [
  ("Node case doesn't add 1 to size", size_always_0);
  ("Node case counts the left child twice", size_duplicates_l);
  ("Node case ignores the left child", size_ignores_l);
  ("Incorrect base case", size_wrong_base)
]

let rec height_always_0 t =
  match t with
  | Empty -> 0
  | Node (l, _, r) -> max (height_always_0 l) (height_always_0 r)
let rec height_duplicates_l t =
  match t with
  | Empty -> 0
  | Node (l, _, _) -> 1 + max (height_duplicates_l l) (height_duplicates_l l)
let rec height_ignores_l t =
  match t with
  | Empty -> 0
  | Node (_, _, r) -> 1 + (height_ignores_l r)
let rec correct_height t =
  match t with
  | Empty -> 0
  | Node (l, _, r) -> 1 + max (correct_height l) (correct_height r)
let rec height_wrong_base t =
  match t with
  | Empty -> 1
  | Node (l, _, r) -> 1 + max (correct_height l) (correct_height r)
let height_mutants = [
  ("Node case doesn't add 1 to height", height_always_0);
  ("Node case counts the left child twice", height_duplicates_l);
  ("Node case ignores the left child", height_ignores_l);
  ("Incorrect base case", height_wrong_base)
]

let rec num_leaves_always_0 t =
  match t with
  | Empty -> 0
  | Node (l, _, r) -> (num_leaves_always_0 l) + (num_leaves_always_0 r)
let rec num_leaves_duplicates_l t =
  match t with
  | Empty -> 0
  | Node (Empty, _, Empty) -> 1
  | Node (l, _, _) -> (num_leaves_duplicates_l l) + (num_leaves_duplicates_l l)
let rec num_leaves_plus_one t =
  match t with
  | Empty -> 0
  | Node (Empty, _, Empty) -> 1
  | Node (l, _, r) -> 1 + (num_leaves_plus_one l) + (num_leaves_plus_one r)
let rec num_internal_nodes t =
  match t with
  | Empty -> 0
  | Node (Empty, _, Empty) -> 0
  | Node (l, _, r) -> 1 + (num_internal_nodes l) + (num_internal_nodes r)
let num_leaves_mutants = [
  ("Leaf case doesn't increment the count", num_leaves_always_0);
  ("Internal node case counts the left child twice", num_leaves_duplicates_l);
  ("Internal node case is off by 1", num_leaves_plus_one);
  ("Counts internal nodes instead of leaves", num_internal_nodes)
]

(* Message to be shown upon failing mutation testing *)
let mut_test_failure_str =
  "Your test suite was not sufficient to catch all of the bugs in our " ^
  "faulty implementations. Your grade for this question is capped at 50%."
let mut_test_failure_msg = Message ( [Text mut_test_failure_str], Failure)

(* If full marks were achieved in mutation testing
   (indicated by snd (Report.result mut_report) = false), scale test_report
   by 2. Otherwise prepend a failure message to test_report.
   Return the concatenation of the two reports.
*)
let scale_if_passed mut_report test_report =
  let test_report =
    if Mutation_test.passed_mutation_testing mut_report
    then scale 2 test_report
    else mut_test_failure_msg :: test_report (* mutation testing failed *)
  in
  mut_report @ test_report

(* Test the test suite for function name using mutation testing.
   Also run randomized tests on the function using the given arguments for
   test_function_1_against_solution.
   If mutation testing passes, scale the test report by 2.
*)
let test_conditional_on_mut_tests ty name muts tests gen =
  let mut_report = Mutation_test.test_unit_tests_1 ty name muts in
  let test_report =
    test_function_1_against_solution
      ty name
      ~gen
      tests
  in
  scale_if_passed mut_report test_report

(* Tester for the size function *)
let test_size () =
  let size_tests = [
      Empty;
      sample_single_node ();
      Node (sample_single_node (), sample_int (), sample_single_node ())
    ]
  in
  let report = test_conditional_on_mut_tests
    [%ty: tree -> int] "size"
    size_mutants size_tests 7
  in
  Section ([Text "Question 1:"; Code "size"], report)

(* Tester for the height function *)
let test_height () =
  let height_tests = [
      Empty;
      sample_single_node ();
      Node (sample_single_node (), sample_int (), sample_single_node ())
    ]
  in
  let report = test_conditional_on_mut_tests
    [%ty: tree -> int] "height"
    height_mutants height_tests 7
  in
  Section ([Text "Question 2:"; Code "height"], report)

(* Tester for the num_leaves function *)
let test_num_leaves () =
  let num_leaves_tests = [
      Empty;
      sample_single_node ();
      Node (sample_single_node (), sample_int (), sample_single_node ())
    ]
  in
  let report = test_conditional_on_mut_tests
    [%ty: tree -> int] "num_leaves"
    num_leaves_mutants num_leaves_tests 7
  in
  Section ([Text "Question 3:"; Code "num_leaves"], report)

(* Generate the report to be shown to the student *)
let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [test_size (); test_height (); test_num_leaves ()]
