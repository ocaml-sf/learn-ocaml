(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(* This module provides functions for automatically grading
   a student's unit tests using *mutation testing*.

   A student's tests for a function [foo] are run against several
   *mutants* (buggy implementations of [foo]). The student then
   receives a grade based on how many mutants *failed* at least
   one of their tests, i.e. how many buggy implementations were
   exposed as buggy by their test suite.

   Mutants are written by the instructor, and ideally should
   be chosen to emphasize test cases that students should be
   testing for. One could think of mutants as "test cases for
   the tests".
*)

(** The information about a mutant is made up of:
    - A name describing the bug in the mutant function
    - The number of points to be awarded for exposing the bug
    - The mutant function itself.
*)
type 'a mutant_info = string * int * 'a

(** Running mutation tests on a student's test suite.
    For testing a function called [foo], the student's tests
    should be in a variable called [foo_tests].

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
module M: sig

  (** Run a test (a pair of input and expected output) on a mutant
      function.
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

  val test_unit_tests_1:
    ?test_student_soln: bool ->
    ?test: ('b -> 'b -> bool) ->
    ('a -> 'b) Ty.ty -> string -> ('a -> 'b) mutant_info list -> Learnocaml_report.t
  val test_unit_tests_2:
    ?test_student_soln: bool ->
    ?test: ('c -> 'c -> bool) ->
    ('a -> 'b -> 'c) Ty.ty -> string -> ('a -> 'b -> 'c) mutant_info list -> Learnocaml_report.t
  val test_unit_tests_3:
    ?test_student_soln: bool ->
    ?test: ('d -> 'd -> bool) ->
    ('a -> 'b -> 'c -> 'd) Ty.ty
    -> string
    -> ('a -> 'b -> 'c -> 'd) mutant_info list
    -> Learnocaml_report.t
  val test_unit_tests_4:
    ?test_student_soln: bool ->
    ?test: ('e -> 'e -> bool) ->
    ('a -> 'b -> 'c -> 'd -> 'e) Ty.ty
    -> string
    -> ('a -> 'b -> 'c -> 'd -> 'e) mutant_info list
    -> Learnocaml_report.t

  (** To be called on a report returned by one of the
      [test_unit_tests_<args_nb>] functions,
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

include module type of M

(** For backwards compatibility *)
module Make (_: module type of Test_lib): module type of M
