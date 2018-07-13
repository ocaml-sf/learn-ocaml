# Step 5: Other ways to test the student code


## More test functions

The functions `Test_lib.test_function_<nb args>_against_solution` are not the
only test functions. There are actually 3 groups in total:

* `test_function_<nb_args>_againt_solution`: the usual. Test the student code againt a
  given solution written in the `solution.ml` file.

* `test_function_<nb_args>_against`: same than the usual except the solution is given as an input instead of being written in `solution.ml`

* `test_function_<nb_args>`: compare the student code to a serie of tests where both inputs and expected outputs are given.


### `test_function_<nb_args>_against_solution`

#### Type of the 1 argument version
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
	('a -> 'b) Ty.ty -> string -> 'a list -> Learnocaml_report.report
```

#### Mandatory arguments
In [`test_function_1_againt_solution_1 ty name tests`]:

* `ty`: type of the function for these tests. It must not contain type variables (i.e. `'a`, `'b` etc..). Must match the type of the tests (see examples) and be compatible with the solution.

* `name`: name of the tested function in the student code and in `solution.ml`

* `tests`: list of inputs for which the function is tested.

#### Optional arguments

* `gen`: number of automatically generated tests. See [step
  3](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-3.md)
  and
  [step-4](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-4.md)
  for more information.

* `sampler`: used to define sampler for generating tested inputs. See
  [step
  3](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-3.md)
  and
  [step-4](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-4.md)
  for more information.

* `test`: is used to redefine the function which compare the output of
  the student function and the output of the solution. See WIP for
  more information.

* `test_stdout`: is used to redefine the function which compare the
  standart output channel of the student function and the one of the
  solution. See WIP for more information.

* `test_stderr`: is used to redefine the function which compare the
  standart erro channel of the student function and the one of the
  solution. See WIP for more information.

* `before_reference`: enables to introduce a reference or make a side
  effect before each test. The input of this function is the current
  test.

* `before_user`: enables to introduce a reference or make a side
  effect juste before the evaluation of the student function on the
  current tested inputs.

* `after`: enables to change by doing a test after the normal
  report. 

#### Examples

Note: only simple examples can be found here. For more advanced
examples, see the corresponding tutorials.


### `test_function_<nb_args>_against`

There function is exactly the same than the previous one except it
takes one more mandatory argument: the solution

#### Type of the 1 argument version
```ocaml
val test_function_1_against :
	?gen: int ->
	?test: 'b tester ->
	?test_stdout: io_tester ->
	?test_stderr: io_tester ->
	?before_reference : ('a -> unit) ->
	?before_user : ('a -> unit) ->
	?after : ('a -> ('b * string * string) -> ('b * string * string) -> Learnocaml_report.report) ->
	?sampler : (unit -> 'a) ->
	('a -> 'b) Ty.ty -> string -> ('a -> 'b) -> 'a list -> Learnocaml_report.report
```

#### Examples

### `test_function_<nb_args>`

#### Type of the 1 argument version

```ocaml
  val test_function_1 :
	?test: 'b tester ->
	?test_stdout: io_tester ->
	?test_stderr: io_tester ->
	?before : ('a -> unit) ->
	?after : ('a -> ('b * string * string) -> ('b * string * string) -> Learnocaml_report.report) ->
	('a -> 'b) Ty.ty -> string -> ('a * 'b * string * string) list -> Learnocaml_report.report
```

#### Examples


## Compare value

## Compare imperative effects

To be continued.

---
[Previous step](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-4.md)

[Table of contents](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/howto-write-exercises.md)

[Next step](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-6.md)
