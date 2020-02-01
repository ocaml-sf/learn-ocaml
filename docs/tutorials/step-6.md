# Step 6 : Grading functions for variables

**Warning** This step is ahead of the current version of [learn-ocaml].

## Test functions for variables
There are 3 test functions for variables.

### Signatures
```ocaml
  val test_variable :
    'a Ty.ty -> string -> 'a -> Learnocaml_report.report

  val test_variable_against_solution :
    'a Ty.ty -> string -> Learnocaml_report.report

  val test_variable_property :
    'a Ty.ty -> string -> ('a -> Learnocaml_report.report) -> Learnocaml_report.report
```

### Examples
#### Trivial example with `grade_variable`
`grade_variable` is usually used only for trivial examples since
it is directly compared to an expected result and not to a solution.

```ocaml
let exercise_0 =
  grade_variable [%ty: int] "forty_two" 42
```

#### More classical example with `grade_variable_against_solution`
`grade_variable_against_solution` is more versatile and works
basically like `test_function_against_solution`.
```ocaml
let exercise_1 =
  grade_variable_against_solution [%ty: float] "norm"
```

#### `grade_variable_property`
`grade_variable_property` is used in specific cases when you
want to write your own report depending on the value of the graded
variable.

```ocaml
let exercise_2 =
  grade_variable_property [%ty: side] "s"
    (
      fun s ->
      match s with
      | Right -> if vect.x > 0. then
                   [ Message ([ Text "Expected value"] , Success 1) ]
                 else if vect.x = 0. then
                   [ Message ([ Text "Wrong value"] , Failure) ;
                     Message ([ Text "The answer should "; Code "Middle" ; Text "."] , Informative) ]
                 else
                   [ Message ([ Text "Wrong value"] , Failure) ;
                     Message ([ Text "The answer should "; Code "Left" ; Text "."] , Informative) ]
      | Left -> if vect.x < 0. then
                  [ Message ([ Text "Expected value"] , Success 1) ]
                else if vect.x = 0. then
                  [ Message ([ Text "Wrong value"] , Failure) ;
                     Message ([ Text "The answer should "; Code "Middle" ; Text "."] , Informative) ]
                 else
                   [ Message ([ Text "Wrong value"] , Failure) ;
                     Message ([ Text "The answer should "; Code "Right" ; Text "."] , Informative) ]
      | Middle -> if vect.x = 0. then
                  [ Message ([ Text "Expected value"] , Success 1) ]
                else if vect.x > 0. then
                  [ Message ([ Text "Wrong value"] , Failure) ;
                     Message ([ Text "The answer should "; Code "Right" ; Text "."] , Informative) ]
                 else
                   [ Message ([ Text "Wrong value"] , Failure) ;
                     Message ([ Text "The answer should "; Code "Left" ; Text "."] , Informative) ]
    )
```
