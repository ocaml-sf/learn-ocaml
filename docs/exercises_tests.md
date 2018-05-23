This section explains how to write tests for exercices, using the modules
`Test_lib`, `Introspection` and `Report` of the grader.

# `test.ml` format

The file `test.ml` is loaded after the user's code during grading. It has access
to the environment of the `toplevel` used for grading, which contains every
bindings (and state) from (after) `prepare.ml` and `prelude.ml`. The abstract
syntax tree from the code is also reified into the environment, which allows
inspecting its form for some specific tests.

A classic `test.ml` file is as follows:
```ocaml
open Test_lib
open Report

let exercise_1 = ..

let exercise_2 = ..

let exercise_3 = ..


let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ; exercise_3 ]

```

The values `exercice_x` are values of type `Learnocaml_report.report`, which is
a representation of the report given by the grader. In this example, each of
these values are refering to a specific question from the exercise. Their
content is detailed in the next section. These reports are then given to the
function `ast_sanity_check`, which ensures that some modules are never used
(`Obj`, `Marshall`, all the modules from `compiler-libs` or the library that
allows introspection), or some syntactic features of the language (`external` in
particular). Its results, which is a single report, is given to `set_result`,
which _basically_ gives the result of the tests to the grader.

# Writing tests and reports

The format of reports can be found in `src/state/learnocaml_report.ml`. A report
describes the result of that should be outputted and interpreted by the
grader. It can be classiied into sections for lisibility, and return many kind
of messages:
```ocaml
type report = item list

and item =
  | Section of text * report
  (** A titled block that groups subreports *)
  | Message of text * status
  (** Basic report block *)

and status =
  | Success of int (** With given points *)
  | Failure (** With missed points *)
  | Warning (** A student error without influence on the grade *)
  | Informative (** A message for the student *)
  | Important (** An important message *)

and text = inline list

and inline =
  | Text of string (** A word *)
  | Break (** Line separator *)
  | Code of string (** For expressions *)
  | Output of string (** For output *)
```

The most important part of the grading is the ability to test the user's code,
either by simply use it and check its correction against the solution, or even
introspect the abstract syntax tree to detect some patterns. Testing functions
are available in `src/grader/test_lib.mli`: we can observe this module is
actually functorized, but it is applied in the context of `test.ml`.

## `Test_lib`: checking values and output

Lets take the code that checks the first question of the exercise:
```ocaml
let exercise_1 =
  Section ([ Text "Exercise 1: " ; Code "print_int_list" ],
           test_function_1_against_solution
             ~test: test_ignore
             ~test_stdout: (io_test_items ~split: ['\n'] ~trim:[' '] ~skip_empty: true)
             [%ty: int list -> unit] "print_int_list"
             [])
```
This value describes a section in the report for the first exercise. Actually,
according to the type of `Report`, the second argument of this variant should be
a report : this is the result of applying
`test_function_1_against_argument`. This function, as its name suggests, tests a
unary function (hence the `_1_`) against the solution. Its type is the
following:
```ocaml
val test_function_1 :
  ?gen: int ->
  ?test: 'b tester ->
  ?test_stdout: io_tester ->
  ?test_stderr: io_tester ->
  ?before : ('a -> unit) ->
  ?after : ('a -> ('b * string * string) -> ('b * string * string) -> Learnocaml_report.report) ->
  ('a -> 'b) Ty.ty -> string -> 'a list -> Learnocaml_report.report
```
It takes multiple optional arguments, and three non optional arguments, which
are:
- a witness of the type expected for the function to test
- the name expected for the value, which should be bound in the environment
- a list of inputs to give to the function

The optional arguments are tests on the outputs of the function, or functions to
test the output of the function (which test structural equality by default).

The witness is given using a _ppx_ that reifies types into the OCaml language,
in our example: `[%ty int list -> unit]`. In our example, there are no inputs
given to test the function: instead, we let the test engine generating a list of
random inputs (10 by default, which can be changed by giving the argument
`~gen`).

The test functions named `test_function_*_against_solution` check the output of
the solution for the same values, which can be used to compare with the user's
code.

### Writing custom generators

By default, the grader is able to generate random values of base types. However,
it is possible to tweak the generation to ensure it uses only a certain set of
values. This is especially useful to ensure some properties of the algorithms to
test.

#### Custom types

Lets assume we are trying to test a function that only takes natural
numbers lower than 100, and returns `unit`. One way to doing it is to define a
custom type with its sample generator:

```ocaml
type natural = int

let sample_natural () = Random.int 100

(* .. *)

let report =
  test_function_1 (* .. *) [%ty: natural -> unit] "fun_natural" []

```

When the grader will test the function `fun_natural` from the user, it will look
for a sampler for the type `natural`, using the module `Introspection`. By
convention, the name of a sampler for a type `ty` is called `sample_ty`, which
is a function taking `unit` and returning `ty`. As long the sampler as a name
recognized by the grader, the generation is done automatically. For datatypes
that are parameterized, as `list`, they must be applied to a concrete type:

```ocaml
type alt_list = int list

let sample_alt_list () =
  let pos = ref (sample_bool ()) in
  sample_list (fun () ->
    pos := not !pos
    if !pos then Random.int 10 else Random.int 10 - 10)
```

In this example, we generate a list where values alternate between positive and
negative integers.

#### Specify samplers

Alas, sampler for custom types are not checked statically. As such, one way to
avoid this problem is by using the functions `test_function_*_against_*`, that
takes an optional argument `?sampler:('a sampler)`, _i.e._ the function used to
compute the arguments to test the function.

The previous examples can be simply rewritten as
```ocaml

let sample_natural () = Random.int 100

(* .. *)

let report_1 =
  test_function_1_against (* .. *)
    ~sampler:sample_natural
    [%ty: natural -> unit] "fun_natural" []

(* .. *)

let sampler_2 () =
  let pos = ref (sample_bool ()) in
  sample_list (fun () ->
    pos := not !pos
    if !pos then Random.int 10 else Random.int 10 - 10)

let report_2 = 
  test_function_1_against (* .. *)
    ~sampler:sampler_2
    [%ty: int list -> unit] "fun_natural" []
  
```

###  `Test_lib`: checking the AST

Since the user's code is reified, the parsed _abstract syntax tree_ is available
in the testing environment, as a variable named `code_ast`, with type
`Parsetree.structure`. As such, it can be checked using the iterators in the
module `Ast_mapper` from `compiler-libs`. However, `Test_lib` provides some
functions to check the Parsetree.

```ocaml

  type 'a ast_checker =
    ?on_expression: (Parsetree.expression -> Learnocaml_report.report) ->
    ?on_pattern: (Parsetree.pattern -> Learnocaml_report.report) ->
    ?on_structure_item: (Parsetree.structure_item -> Learnocaml_report.report) ->
    ?on_external: (Parsetree.value_description -> Learnocaml_report.report) ->
    ?on_include: (Parsetree.include_declaration -> Learnocaml_report.report) ->
    ?on_open: (Parsetree.open_description -> Learnocaml_report.report) ->
    ?on_module_occurence: (string -> Learnocaml_report.report) ->
    ?on_variable_occurence: (string -> Learnocaml_report.report) ->
    ?on_function_call: ((Parsetree.expression * (string * Parsetree.expression) list) -> Learnocaml_report.report) ->
    'a -> Learnocaml_report.report

  val ast_check_expr : Parsetree.expression ast_checker
  val ast_check_structure : Parsetree.structure ast_checker

  val ast_location_stripper : Ast_mapper.mapper

  val forbid_expr : string -> Parsetree.expression list -> (Parsetree.expression -> Learnocaml_report.report)
  val restrict_expr : string -> Parsetree.expression list -> (Parsetree.expression -> Learnocaml_report.report)
  val require_expr : string -> Parsetree.expression -> (Parsetree.expression -> Learnocaml_report.report)

  val (@@@) : ('a -> Learnocaml_report.report) -> ('a -> Learnocaml_report.report) -> ('a -> Learnocaml_report.report)

  val ast_sanity_check : ?modules: string list -> Parsetree.structure -> (unit -> Learnocaml_report.report) -> Learnocaml_report.report

  val find_binding : Parsetree.structure -> string -> (Parsetree.expression -> Learnocaml_report.report) -> Learnocaml_report.report
```

These functions scan the AST, looking for specific expressions which can be
forbidden or required. The two functions `ast_check_expr` and
`ast_check_structure` takes functions for each syntactic category and allows
pattern-matching on some specific patterns into the code. The function
`find_binding` look for a toplevel value and apply a given function on its
syntax tree.
