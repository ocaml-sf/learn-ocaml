(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type 'a test_result =
  | Pass
  | Fail of 'a
  | Err of exn

(** A mutant is a pair of a string describing the mutant and the
    mutant function itself.
*)
type 'a mutant = string * 'a

(** Run a test (a pair of input and expected output) on a function.
    The [compare] parameter specifies a comparison function for
    comparing the expected and actual outputs, and defaults to
    structural equality ([(=)]).
*)
val run_test_against:
  ?compare: ('b -> 'b -> bool) ->
  ('a -> 'b) -> ('a * 'b) -> 'b test_result

(** Run a test (a pair of input and expected output) on a mutant.
    Returns true if the mutant *fails* the test, either by deviating
    from the expected output or by raising an error.
    Returns false if the mutant *passes* the test.
    The [compare] parameter specifies a comparison function for
    comparing the expected and actual outputs, and defaults to
    structural equality ([(=)]).
*)
val run_test_against_mutant:
  ?compare: ('b -> 'b -> bool) ->
  ('a -> 'b) -> ('a * 'b) -> bool

(** Running mutation tests on a student's test suite.
    For testing a function call [foo], the student's tests should
    be in a variable called [foo_tests].

    This module needs to be instantiated with an instance of
    [Test_lib], which is available to the grader code:

    {[
      module M = Mutation_test.Make (Test_lib)

      M.test_unit_tests_1 ...
    ]}

    A grading function is defined for each arity function from
    one to four:

    [test_unit_tests_<args_nb> ty name mutants]
    grades unit tests for the [args_nb]-arity function named
    [name], which are stored in the variable called [name_tests],
    against the broken implementations in the list [mutants].

    The optional argument [~points] specifies how many points
    should be given for each mutant exposed by the test suite.
    If [test_student_soln] is [true] (as it is by default),
    also runs the student's test suite against the student's own
    implementation and reports the results.
    The [test] parameter specifies a comparison function for the
    expected and actual outputs, and defaults to structural
    equality ([(=)]).
*)
module type S = sig
  val test_unit_tests_1:
    ?points: int ->
    ?test_student_soln: bool ->
    ?test: ('b -> 'b -> bool) ->
    ('a -> 'b) Ty.ty -> string -> ('a -> 'b) mutant list -> Learnocaml_report.t
  val test_unit_tests_2:
    ?points: int ->
    ?test_student_soln: bool ->
    ?test: ('c -> 'c -> bool) ->
    ('a -> 'b -> 'c) Ty.ty -> string -> ('a -> 'b -> 'c) mutant list -> Learnocaml_report.t
  val test_unit_tests_3:
    ?points: int ->
    ?test_student_soln: bool ->
    ?test: ('d -> 'd -> bool) ->
    ('a -> 'b -> 'c -> 'd) Ty.ty
    -> string
    -> ('a -> 'b -> 'c -> 'd) mutant list
    -> Learnocaml_report.t
  val test_unit_tests_4:
    ?points: int ->
    ?test_student_soln: bool ->
    ?test: ('e -> 'e -> bool) ->
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
