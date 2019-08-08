type 'a test_result =
  | Pass
  | Fail of 'a
  | Err of exn

type 'a mutant = string * 'a

(* Run a test (a pair of input and expected output) on a function.
*)
val run_test_against: ('a -> 'b) -> ('a * 'b) -> 'b test_result

(* Run a test (a pair of input and expected output) on a mutant.
   Returns true if the mutant *fails* the test, either by deviating
   from the expected output or by raising an error.
   Returns false if the mutant *passes* the test.
*)
val run_test_against_mutant: ('a -> 'b) -> ('a * 'b) -> bool

(* Running mutation tests on a student's test suite.
   For testing a function call foo, the student's tests should
   be in a variable called foo_tests.
   If [test_student_soln] is [true] (as it is by default),
   also runs the student's test suite against the student's own
   implementation and reports the results.
   This module needs to be instantiated with an instance of
   Test_lib, which is available to the grader code:

   module M = Mutation_test.Make (Test_lib)
   M.test_unit_tests_1 ...
*)
module type S = sig
  val test_unit_tests_1:
    ?points: int ->
    ?test_student_soln: bool ->
    ('a -> 'b) Ty.ty -> string -> ('a -> 'b) mutant list -> Learnocaml_report.t
  val test_unit_tests_2:
    ?points: int ->
    ?test_student_soln: bool ->
    ('a -> 'b -> 'c) Ty.ty -> string -> ('a -> 'b -> 'c) mutant list -> Learnocaml_report.t
  val test_unit_tests_3:
    ?points: int ->
    ?test_student_soln: bool ->
    ('a -> 'b -> 'c -> 'd) Ty.ty
    -> string
    -> ('a -> 'b -> 'c -> 'd) mutant list
    -> Learnocaml_report.t
  val test_unit_tests_4:
    ?points: int ->
    ?test_student_soln: bool ->
    ('a -> 'b -> 'c -> 'd -> 'e) Ty.ty
    -> string
    -> ('a -> 'b -> 'c -> 'd -> 'e) mutant list
    -> Learnocaml_report.t

  (* To be called on a report returned by one of the above 4 functions,
     for checking whether the student passed or failed mutation testing.
     The [Learnocaml_report.result] function is not sufficient for
     checking this since a report will register as a failure if the
     student's implementation does not pass all of their own tests, even
     if the student did pass mutation testing.
     If this function is called on a report that did not result from
     one of the above 4 functions, the result is undefined.
  *)
  val passed_mutation_testing: Learnocaml_report.t -> bool
end

module Make (Test_lib: Test_lib.S) : S
