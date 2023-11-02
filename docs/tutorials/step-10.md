# Step 10: Create a trivial multiplce choice question exercise

In this step, we'll create a simple multiple choice question that only requires the student to answer interactively.

Let's start with the descr.md/html file:
 
```html
<form name="mc1">
	<fieldset>
		<legend>1. The correct answer is A!</legend>
		<input type="checkbox" name="A"> Choose me!<br>
		<input type="checkbox" name="B"> Don't choose me!<br>
		<input type="checkbox" name="C"> Don't choose me!<br>
		<input type="checkbox" name="D"> Don't choose me!<br>
	</fieldset>
</form>
```

The way that we chose to present multiple choice questions was with html forms. Each form has four possible options represented with the name `A`, `B`, `C` or `D`.
The student then select one or more options directly in the exercise description.

The student can then complete the rest of the exercise or grade it to check if his choice was the right one. The grader then verifies which choice was clicked and compares it to the solution file in the form of a string.

The `solution.ml`, for this example should have:

```ocaml
let mc1 = "A"
```

Having the solutions in the form of a string the exercise can have multiple right answer. If thats the case the solution must be written alphabetecly.

Finally, the test function is just a normal function that compares two variables.
In this example the `test.ml` should be:

```ocaml
open Test_lib
open Report

let multipleChoice1_test =
  set_progress "A corrigir pergunta 1" ;
  Section ([ Text "Exercício 1: " ; Code "solution" ],
            test_variable_against_solution
              [%ty: string] 
              "mc1")

let () =
  set_result                @@
  ast_sanity_check code_ast @@ fun () ->
    [ multipleChoice1_test ]
```
