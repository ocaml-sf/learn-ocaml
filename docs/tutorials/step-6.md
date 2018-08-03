# Step 6 : Test functions for variables and references

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
#### Trivial example with `simple_test_variable`
`simple_test_variable` is usually used only for trivial examples since
it is directly compared to an expected result and not to a solution.

```ocaml
let exercise_0 =
  simple_test_variable [%ty: int] "forty_two" 42
```
  
#### More classical example with `simple_test_variable_against_solution`
`simple_test_variable_against_solution` is more versatile and works 
basically like `simple_test_function_against_solution`.
```ocaml
let exercise_1 =
  simple_test_variable_against_solution [%ty: float] "norm"
```

#### `simple_test_variable_property`
`simple_test_variable_property` is used in specific cases when you
want to write your own report depending on the value of the graded
variable.

```ocaml
let exercise_2 =
  simple_test_variable_property [%ty: side] "s"
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
	
## Test functions for references
### Signature 
```ocaml
  val test_ref :
    'a Ty.ty -> 'a ref -> 'a -> Learnocaml_report.report
```
### Examples
WIP

---
[Previous step](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-5.md)

[Table of contents](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/howto-write-exercises.md)

[Next step](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-7.md)
