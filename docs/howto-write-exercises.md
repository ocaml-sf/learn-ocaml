How to write an exercise
========================

This tutorial explains how to write an automatically graded exercise
for the learn-ocaml platform. As a prerequesite to this tutorial, make
sure you have followed the tutorial to setup your development
environment `howto-setup-exercise-development-environment.md`.

After this tutorial, you will be able to submit your exercise to an
exercise GIT repository if you want. The procedure for submission is
explained in `howto-submit-an-exercise.md`. If your exercises are not
to be shared, you can already deploy an instance of the learn-ocaml
platform using your local directory of exercises.

# Step -1: Download the source files for this tutorial

All the files used in that tutorial are available on a GIT repository:

```
git clone git@github.com:yurug/learn-ocaml-tutorial.git
```

Each step of the tutorial is a branch in the repository. Therefore,
do

```
git checkout step-1
```

to get the files for step 1, and replace `step-1` by `step-2` to
get the files for the second step, and so on and so forth.

# Step 0 : Preliminaries

The structure of your local directory of exercises must follow a
specific shape, illustrated by the following ascii art:

```
.
├── exercises
│   ├── exo1
│   │   ├── descr.html
│   │   ├── meta.json
│   │   ├── prelude.ml
│   │   ├── prepare.ml
│   │   ├── solution.ml
│   │   ├── template.ml
│   │   ├── test.ml
│   │   └── title.txt
│   ├── exo2
│   │   ├── ...
│   ├── index.json
├── lessons
│   ├── somelesson.json
│   └── lessons.json
├── tutorials
│   ├── index.json
│   ├── some-tutorial.html
│   ├── some-other-tutorial.md
```

The complete format specification for exercise description is given in
[https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/exercices_format.md](learn-ocaml/docs/exercices_format.md).

- The directory `exercises` must contain the descriptions of the
  exercises. Each exercise description is stored in a dedicated
  subdirectory. On this example, the directory `exercises` contain two
  exercises `exo1` and `exo2`. The file `index.json` puts some structure
  over this set of exercises, for instance:

```
{
  "learnocaml_version": "1",
  "groups":
  { "group1":
    {
      "title": "Some group of exercises",
      "exercises": [ "exo1", "exo2" ]
    }
  }
}
```

- An exercise description is composed of 8 files.

  - `descr.html` contains the exercise statement as a sequence of
  HTML elements (that can be grafted in a `div`).

  - `meta.json` contains metadata about the exercise.

  - `prelude.ml` is an OCaml code fragment loaded in the toplevel before
  the student answer.

  - `prepare.ml` is an OCaml code fragment inserted after the prelude when
  the student answer is graded.

  - `solution.ml` is your answer to the exercise.

  - `template.ml` is the OCaml code fragment that initializes a fresh
    student answer.

  - `test.ml` is the grader code.

  - `title.txt` is a one-line file containing the title of the exercise.

- `lessons` and `tutorials` are ignored in this tutorial.

## Do it yourself!

Reproduce this structure in your own exercise directory. For the moment,
your list of exercises is probably empty but it will be populated by the
next step of this tutorial.

# Step 1: Create a trivial exercise

As a first simple step, let us create an exercise which requires no
real effort from the student: any valid OCaml code will be a valid
answer for this exercise.

Let us focus on `test.ml`:
```
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
[https://github.com/ocaml-sf/learn-ocaml/blob/master/src/grader/test_lib.mli](test_lib.mli).

The interface for the module `Report` can be found in
[https://github.com/ocaml-sf/learn-ocaml/blob/master/src/state/learnocaml_report.mli](learnocaml_report.mli).

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
pass, then the report generate the message `That was easy!` and
give 1 point to the student.

## Do it yourself!

1. Copy the files of this trivial exercise to your own exercises
   directory.
2. Update index.json
3. Build and run the new instance of your local learn-ocaml platform:
```
learn-ocaml build; learn-ocaml serve
```

At this point, you should see the exercise in the instance opened
on `http://localhost:8080`. Click on grade to get your point!

# Step 2: Basic grading by comparison with your solution

Let us now move to the development a basic grader. We ask the student
to implement the identity function and provide the following `template.ml`:
```
let identity x = "Put your code here"
```

Our goal is to check that this function is modified by the student
in such a way that it behaves as our `solution.ml`:
```
let identity x = x
```

To that end, the grader will compare the outputs of the student
function with the function of the solution. We must provide inputs
of a ground type (i.e. of non polymorphic type), execute the two
functions and compare their outputs. Each time the outputs match,
the student gets one point.

Assuming that we choose `int` for this ground type, the behavior described
in the previous paragraph is implemented as follows:
```
open Test_lib
open Report

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [
    Section ([ Text "The identity of 0 is 0." ],
             test_function_1_against_solution
               [%ty: int -> int] "identity"
               ~gen:0 [0]
      );
  ]
```

The function `Test_lib.test_function_1_against_solution` is the key
here. Let us take a moment to understand how it is called:

- The argument `[%ty: int -> int]` is written in a PPX extension of
  OCaml: it reifies the type `int -> int` as a first-class value. With
  that information in its hands, `test_function_1_against_solution`
  can check that the student has written a function of the right type.

- The string `"identity"` is the identifier of the function to be tested.

- The optional argument `gen` is set to `0` because we are not willing to
  automatically generate inputs. (This will be the topic of the next step
  of this tutorial.)

- The final argument `[0]` is the unique input on which we want to test
  the function.

## Do it yourself!

1. Copy this exercise source to your own exercise directory.

2. Update your `index.json` file.

3. Run `learn-ocaml build && learn-ocaml serve`

4. Open `http://localhost:8080` in your browser.

5. Check that the templace does not get the point.

6. Modify the code, get your point!

7. Change the argument `~gen:0` to `gen:41` for instance and
   rebuild your learn-ocaml instance.

8. Grade your answer and observe the effect of the previous
   change. This is the topic of the next of this tutorial!

# Step 3: Grading with generators

To be continued.

# Step 4: Introspection of students code

To be continued.