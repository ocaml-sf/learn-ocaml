# Step 5: More about grading functions for functions

**Warning** This step is ahead of the current version of [learn-ocaml].

## Different grading functions for functions
There are 2 main grading functions:

* `test_function_<nb_args>_againt_solution`: the usual. Test the
  student code against a given solution written in the `solution.ml`
  file.

* `test_function_<nb_args>`: compare the student code to a series of
  tests where both inputs and expected outputs are given. Note that
  you still need to write a solution in `solution.ml` to build your
  exercise session since `learn-ocaml build` tests your grader with
  `solution.ml` as the student copy.

## A few words about grading function for functions

Grading functions for functions return a global report concatening 4 specific
ones:

- report resulting of comparison between student and solution
  outputs. The function `~test` is used to build this report. By
  default, `~test` uses the structural equality to compare outputs.

- report resulting of comparison between student and solution standard
  outputs. The function `~test_stdout` is used to build this
  report. By default, `~test_stdout` is set to ignore standard output
  and returns an empty report.

- report resulting of comparison between student and solution standard
  errors. The function `~test_stderr` is used to build this report.
  By default, `~test_stderr` is set to ignore standard error and
  returns an empty report.

- report resulting of the result of function `~after` and returns an
  empty report by default.

## `test_function_<nb_args>_against_solution`
### Signature for unary function
```ocaml
  val test_function_1_against_solution :
  ?gen: int ->
  ?test: 'b tester ->
  ?test_stdout: io_tester ->
  ?test_stderr: io_tester ->
  ?before_reference : ('a -> unit) ->
  ?before_user : ('a -> unit) ->
  ?after : ('a -> ('b * string * string) -> ('b * string * string) -> Learnocaml_report.report) ->
  ?sampler : (unit -> 'a) ->
  ('a -> 'b) Ty.ty -> string -> 'a list -> Learnocaml_report.item
```

### Mandatory arguments
[`test_function_1_againt_solution_1 ty name tests`]:

* `ty`: type of the tested function specified for the given tests. It
  must not contain type variables (i.e. `'a`, `'b` etc..), match the
  type of the tests and be compatible with the solution.

* `name`: name of the tested function in the student code and in
  `solution.ml`

* `tests`: list of tested inputs.

## `test_function_<nb_args>`

### Signature of grading function for unary function

```ocaml
  val test_function_1 :
  ?test: 'b tester ->
  ?test_stdout: io_tester ->
  ?test_stderr: io_tester ->
  ?before : ('a -> unit) ->
  ?after : ('a -> ('b * string * string) -> ('b * string * string) -> Learnocaml_report.report) ->
  ('a -> 'b) Ty.ty -> string -> ('a * 'b * string * string) list -> Learnocaml_report.item
```

### Mandatory arguments
[`test_function_1 ty name tests`]:

* `ty`: type of the tested function specified for the given tests. It
  must not contain type variables (i.e. `'a`, `'b` etc..), match the
  type of the tests and be compatible with the solution.

* `name`: name of the tested function in the student code and in
  `solution.ml`

* `(in, out, stderr, stdout) :: _` as `tests`: list of tuples where
  the first element `in` is the argument passed to the tested function
  (for a three-arguments functions `tests` will instead matched `(in1,
  in2, in3, out, stderr, stdout)`). The second element `out` is the
  expected output. `stderr` and `stdout` are the expected strings in
  standard output and error output respectively.

## Optional arguments of grading functions

* `gen`: number of automatically generated tests. See
  [step-3](step-3.md)
  and
  [step-4](step-4.md)
  for more information.

* `sampler`: used to define sampler for automatically generating
  inputs for tests. See
  [step-3](step-3.md)
  and
  [step-4](step-4.md)
  for more information.

* `test`: is used to redefine the function which compare the output of
  the student function and the output of the solution. See WIP for
  more information.

* `test_stdout`: is used to redefine the function which compare the
  standard output channel of the student function and the one of the
  solution. See WIP for more information.

* `test_stderr`: is used to redefine the function which compare the
  standard error channel of the student function and the one of the
  solution. See WIP for more information.

* `after`: is used to redefine a function which is called with the
  current tested inputs, the student result and the solution result
  and returns a report.  Enables for example to inspect references
  introduced with `~before`, `~before_user` or `~before_reference` and
  build an appropriate report.  See WIP for more information.

* `before_reference`: is used to redefine a function called right
  before the application of the current tested inputs to the
  solution. This enables for example to introduce a reference or make
  a side effect before each test. See WIP for more information.

* `before_user`: is used to redefine a function called right before
  the application the current tested inputs to the student
  function. This enables to introduce a reference or make a side
  effect between solution evaluation and student function
  evaluation. See WIP for more information.

* `before`: same as `before_reference` for
  `test_function_<nb_args>`. Since no solution is evaluated, there is
  no need to distinguish between before or after solution evaluation.

## Examples

Note: only trivial examples can be found here. For more advanced
examples, see the corresponding tutorials.

### Identity

This is a classical example of an unary function, with a user-defined sampler.

```ocaml
let exercise_1 =
  test_function_1_against_solution
  [%ty: int -> int]
  "identity"
  ~gen:10
  ~sampler:(fun () -> Random.int 42)
  [0 ; 42]
```
With `test_function_1`:
```ocaml
let exercise_2 =
  test_function_1
    [%ty: int -> int]
    "identity"
    [0, 0, "", "" ;
    42, 42, "", ""]  (* List of tests *)
```

Note that since the default comparison function for standard output
and error actually ignore them, we could put anything as expected outputs in
the tests list.


### Hello world

`Hello world` is also very classical, however grading this function is
a little more tricky since by default standard output and error output
are ignored (meaning the student standard/error output can be
different from the solution ones without causing a failure).


```ocaml
let exercise_2 =
  test_function_1_against_solution
    [%ty: unit -> unit]
    ~test:test_ignore
    ~test_stdout:io_test_equals
    "hello"
    ~gen:0
    [()]
```

```ocaml
let exercise_4 =
  test_function_1
  [%ty: unit -> unit]
  ~test:test_ignore
  ~test_stdout:io_test_equals
  "hello"
  [(), (), "Hello world!", ""]
```

The function `test_ignore` for optional argument `test` enables to
ignore the student and solution output. On the contrary,
`io_test_equals` for `test_stdout` enables to compare standard outputs
with the structural equality.
