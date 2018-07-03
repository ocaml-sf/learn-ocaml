# Step 3: Grading with generators for Ocaml built-in types
 You can find the examples below in the
	`exercises/sampler-built-in-types` directory (branch: step-3).
	
As see previously, you can either give manually both inputs of the
tested functions or you can ask the grader to automatically generate
inputs. 

For built-in types, the grader actually do most of the work for
you: you only need to precise the number of inputs sets you want to be generated.

In the example below, five tests are automatically generated.

```ocaml
let exercise_1 =
	Section ([ Text "Function: "; Code "identity" ],
           test_function_1_against_solution
             [%ty: int -> int] "identity"
             ~gen:5 [0]
          )
```


However, with this method, you have no control on how the inputs are
generated. If, for example, you want a function of type `int -> int ->
int` to be tested for inputs between 12 and 42, you need to give the
test function the sampler you want. There are 2 ways to do that. 

## Method 1 : using the `~sampler` argument
One way is to use the optional argument `~sampler` of type `unit ->
<arg1 type> * <arg2 type> * <arg3 type> etc.`.

```ocaml
let exercise_2 =
  Section ([ Text "Function: "; Code "pi1" ],
           test_function_2_against_solution
             [%ty: int -> int -> int] "pi1"
             ~sampler:(fun () -> Random.int 31 + 12, Random.int 31 + 12)
             ~gen:5
             []
          )
```

## Method 2 : redefining the corresponding sampling function.
Another way is to define a sampling function of type `unit -> <arg1
type> * <arg2 type> * <arg3 type> etc.` using the naming convention :
`sample_<type>`. In this case, nothing needs to be add to the test
function call.

```ocaml
let sample_int = Random.int 31 + 12

let exercise_3 =
	Section ([ Text "Function: "; Code "pi1" ],
		test_function_2_against_solution
		[%ty: int -> int -> int] "pi1"
		~gen:5
		[]
	)
```

---
[Previous step](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-2.md)

[Table of contents](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/howto-write-exercises.md)

[Next step](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-4.md)
