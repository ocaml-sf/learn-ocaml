# Step 5: More about test functions for functions


## A few words about test function for functions

Test functions for functions (like `test_function_1_against_solution`
for unary function) returns a report concataning 4 specific reports :

- report resulting of comparison between student and solution
  outputs. The function `~test` is used to build this report. By
  default, `~test` uses the structural equality to compare outputs.

- report resulting of comparison between student and solution standart
  outputs. The function `~test_stdout` is used to buils this
  report. By default, `~test_stdout` is set to ignore standart output
  and return an empty report.

- report resulting of comparison between student and solution standart
  errors. The function `~test_stderr` is used to buils this report.
  By default, `~test_stderr` is set to ignore standart error and
  returns an empty report.
  
- report resulting of the result of function `after` and returns an
  empty report by default. 
  
## Signature of 1 argument test function
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

## Mandatory arguments
[`test_function_1_againt_solution_1 ty name tests`]:

* `ty`: type of the function for the tests. It must not contain type variables (i.e. `'a`, `'b` etc..), match the type of the tests (see examples) and be compatible with the solution.

* `name`: name of the tested function in the student code and in `solution.ml`

* `tests`: list of inputs for which the function is tested.

## Optional arguments

* `gen`: number of automatically generated tests. See [step
  3](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-3.md)
  and
  [step-4](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-4.md)
  for more information.

* `sampler`: used to define sampler for automatically generating
  inputs for tests. See [step
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

* `before_reference`: is used to redefine a function called right
  before the application of solution function to the current tested
  inputs. This enables for example to introduce a reference or make a
  side effect before each test. See WIP for more information.

* `before_user`: is used to redefine a function called right before
  the application of the student function to the current tested
  inputs. This enables to introduce a reference or make a side effect
  between solution evaluation and student function evaluation. See WIP
  for more information.

* `after`: is used to redefine a function which is called with the
  current tested inputs, the student result and the solution result
  and returns a new report which is concatened to reports built with
  the result of the functions `~test`, `~test_sdtout` and
  `~test_sdterr`.  Enables for example to inspect references
  introduced with `~before`, `~before_user` or `~before_reference` and
  build an appropriate report.  See WIP for more information.

### Examples

Note: only trivial examples can be found here. For more advanced
examples, see the corresponding tutorials.

### Identity 

This is a classical example of an unary function, with a user-defined sampler. 

```ocaml
let exercise_1 =
  Section ([ Text "Function: "; Code "identity" ],
		   test_function_1_against_solution
			 [%ty: int -> int]
			 "identity"
			 ~gen:10
			 ~sampler:(fun () -> Random.int 42)
			 [0 ; 42]
	)
```


### Hello world

`Hello world` is also very classical, however grading this function is
a little more tricky since by default standart output and error output
are ignored (meaning the student standart/error output can be
different from the solution ones without causing a failure).


```ocaml
let exercise_2 =
  Section ([ Text "Function: "; Code "hello" ],
		   test_function_1_against_solution
			 [%ty: unit -> unit]
			 ~test:test_ignore
			 ~test_stdout:io_test_equals
			 "hello"
			 ~gen:0
			 [()]
		  )
```

The function `test_ignore` for optional argument `test` enables to
ignore the student and solution output. On the contrary,
`io_test_equals` for `test_stdout` enables to compare standart outputs
with the structural equality.

--- [Previous
step](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-4.md)

[Table of contents](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/howto-write-exercises.md)

[Next step](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-6.md)
