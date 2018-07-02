# Step 1: Create a trivial exercise

As a first simple step, let us create an exercise which requires no
real effort from the student: any valid OCaml code will be a valid
answer for this exercise.

Let us focus on `test.ml`:
```ocaml
open Test_lib
open Report

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [
    Message ([ Text "That was easy!" ], Success 1)
  ]
```

This code is executed to grade the student answer. It has access to
all the toplevel definitions introduced by the student answer.

The interface for the module `Test_lib` can be found in
[test_lib.mli](https://github.com/ocaml-sf/learn-ocaml/blob/master/src/grader/test_lib.mli).

The interface for the module `Report` can be found in
[learnocaml_report.mli](https://github.com/ocaml-sf/learn-ocaml/blob/master/src/state/learnocaml_report.mli).

`Test_lib.set_result` waits for the final `report`. A value of type
`report` can be built from the constructors defined in the module
`Report`. Two functions are applied to get the final report: first,
`Test_lib.ast_sanity_check` performs some basic verification on the
OCaml source, typically that some forbidden modules are not used ;
second, it calls the function starting from `fun () -> ...` to
execute the user testing code.

The user testing code is responsible for three tasks: to test the
student answer, to assign a grade to this answer, and to generate
an informative report.

In that example, we do not test the user code: therefore only the
typechecking and the sanity checks are performed. If these checks
pass, then the report generates the message `That was easy!` and
give 1 point to the student.

### Do it yourself!

1. Copy the files of this trivial exercise to your own exercises
   directory.
2. Update index.json
3. Build and run the new instance of your local learn-ocaml platform:
```ocaml
learn-ocaml build; learn-ocaml serve
```

At this point, you should see the exercise in the instance opened
on `http://localhost:8080`. Click on grade to get your point!

---
<div style="text-align: right">[Previous step](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-0.md)</div>
<div style="text-align: right">[Table of contents](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/howto-write-exercises.md)</div>
<div style="text-align: right">[Next step](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-2.md)</div>
