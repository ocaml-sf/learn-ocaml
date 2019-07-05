# Step 2: Basic grading by comparison with your solution

Let us now move to the development a basic grader. We ask the student
to implement the identity function and provide the following `template.ml`:
```ocaml
let identity x = "Put your code here"
```

Our goal is to check that this function is modified by the student
in such a way that it behaves as our `solution.ml`:
```ocaml
let identity x = x
```

To that end, the grader will compare the outputs of the student
function with the function of the solution. We must provide inputs
of a ground type (i.e. of non polymorphic type), execute the two
functions and compare their outputs. Each time the outputs match,
the student gets one point.

Assuming that we choose `int` for this ground type, the behavior described
in the previous paragraph is implemented as follows:
```ocaml
open Test_lib
open Report

let exercise_1 =
  grade_function_1_against_solution
    [%ty: int -> int] (* Type of the tested function *)
    "identity"        (* Identifier of the tested function *)
    ~gen:0            (* Number of automatically generated tests *)
    [0]               (* List of tested inputs *)

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [exercise_1]
```

The function `Test_lib.grade_function_1_against_solution` is the key
here. Let us take a moment to understand how it is called:

- The argument `[%ty: int -> int]` is written in a PPX extension of
  OCaml: it reifies the type `int -> int` as a first-class value. With
  that information in its hands, `grade_function_1_against_solution`
  can check that the student has written a function of the right type.

- The string `"identity"` is the identifier of the function to be tested.

- The optional argument `gen` is set to `0` because we are not willing to
  automatically generate inputs. (This will be the topic of another step
  of this tutorial.)

- The final argument `[0]` is the unique input on which we want to test
  the function.

This function also determines the text written in the header of the
corresponding report. For a function called `my_function`, it will be
"Function: my_function". 

## Do it yourself!

1. Copy this exercise source to your own exercise directory.

2. Update your `index.json` file.

3. Run `learn-ocaml build && learn-ocaml serve`

4. Open `http://localhost:8080` in your browser.

5. Check that the template does not get the point.

6. Modify the code, get your point!

7. Change the argument `~gen:0` to `gen:41` for instance and
   rebuild your learn-ocaml instance.

8. Grade your answer and observe the effect of the previous
   change. This is the topic of the next step of this tutorial!
   

## Want to learn more about grading function ?
The next steps will bring you progressively to understand most of the
possibilities of grading functions. However, if you want to have a better
overview right now, you can go directly to [step
5](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-5.md)
where you will:
* find the signature of `grade_function_1_against_solution`
* learn a new grading function for functions
* learn how to change the header report
* have a quick resumé of the utility of each optional arguments with a
link to the right tutorial.

## Multiple arguments 
To grade a function with multiple arguments you simply need to use the
corresponding grading function which follows this pattern :
`Test_lib.grade_function_<function arity>_against_solution` and give
the inputs as n-uplets: 

```ocaml
open Test_lib
open Report

let exercise_1 =
	grade_function_2_against_solution
		[%ty: int -> int -> int] "op"
		~gen:5 
		[ (1,2) ; (0,1) ]

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ]
	
```


You can find this example in the
`exercises/grade-function-multiple_args` directory (branch: step-2).


## Polymorphic functions : testing several types
For a polymorphic functions, you may want to test the function with
different types. To do so, you can concat the result of numerous grading
functions and encapsulate it in a `Section` which has two arguments :
some text and a list of items produced by grading functions.

```ocaml
open Test_lib
open Report


let exercise_1 =
    Section ([ Text "Function: "; Code "identity" ; Text " with multiple tested input types." ],
             [ grade_function_1_against_solution
               [%ty: int -> int] (* [identity] tested with integer *)
               "identity"
               ~gen:0 [1 ; 2];
               grade_function_1_against_solution
                 [%ty: char -> char] (* [identity] tested with char *)
                 "identity"
                 ~gen:0 ['c' ; 'a'];
               grade_function_1_against_solution
                 [%ty: float -> float] (* [identity] tested with float *)
                 "identity"
                 ~gen:0 [1.1 ; 2.4]]
            )

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ]
```

You can find this example in the
`exercises/grade-function-polymorphism` directory (branch: step-2).

---
[Previous step](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-1.md)

[Table of contents](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/howto-write-exercises.md)

[Next step](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-3.md)
