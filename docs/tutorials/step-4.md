# Step 4: Grading with generators for user-defined types

In the case of user-defined types, it is mandatory to define a
sampler. Both two previous methods (using the `~sampler` optional
argument or defining a sampler function `sample_my_type`) can be used
but required a little more work, especially for parametric types.

## Non parametric type
For non-parametric type, it is exactly the same than in the previous
step.

You can find the examples below in the
`exercises/sampler-user-defined-types` directory (branch: step-4).

In the examples, we use the type `color` defined as :
```ocaml 
type color = Green | Yellow | Red | Blue
```

### Method 1:  using the `~sampler` argument

As in the previous step, you can add the argument `~sampler` of type
`unit -> <arg1_type> * <arg2_type> * <arg3_type> * etc.`

```ocaml
let exercise_1 =
  Section ([ Text "Function: "; Code "color_to_string" ],
           test_function_1_against_solution
             [%ty: color -> string] "color_to_string"
             ~sampler: (fun () ->  match Random.int 4 with
                 | 0 -> Red | 1 -> Green | 2 -> Yellow | _ -> Blue)
             ~gen:5
             []
          )
```
		 
### Method 2: Defining a sampler 
You can also define your own sampler and not use the `~sampler`
argument with the following rule: a sampler of type `unit -> my_type`
has to be named `sample_my_type`.

```ocaml
let sample_color () : color =
  match Random.int 4 with
    | 0 -> Red
    | 1 -> Green
    | 2 -> Yellow
    | _ -> Blue

let exercise_2 =
  Section ([ Text "Function: "; Code "color_to_string" ],
           test_function_1_against_solution
             [%ty: color -> string] "color_to_string"
             ~gen:5
             []
          )
```

In this case, the grader will automatically use your sampler
`sample_color` for the type `color`. Be careful to write
`sample_color` and not `sampler_color`.

## Parametric types

You can find the examples below in the
`exercises/sampler-user-defined-parametric-types` directory (branch: step-4).

In the examples below, we use the types:
```ocaml
type col = R | B

type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree
```
### Method 1:  using the `~sampler` argument

No change here, just don't forget that the optional argument`~sampler`
has type `unit -> <arg1_type> * <arg2_type> * <arg3_type> * etc.`

```ocaml
let sample_col () = match Random.int 2 with
  | 0 -> B
  | _ -> R

let sample_col_tree () = 
  let rec builder h = match h with
    | 0 -> Leaf
    | n -> match Random.int 3 with
      | 0 -> Leaf
      | _ -> Node (builder (h-1), sample_col (), builder (h-1))
  in
  let h = Random.int 5 + 2 in
  builder h
  
let exercise_3 =
  Section ([ Text "Function: "; Code "monochrome" ],
           test_function_2_against_solution
             [%ty: col tree -> col -> col tree] "monochrome"
             ~sampler:(fun () -> sample_col_tree (), sample_col ())
             ~gen:5
             []
          )
```

### Method 2: Defining a sampler

A sampler of a parametric type ` ('a * 'b * ... ) my_type` has a
type : `(unit -> 'a) -> (unit -> 'b) -> ... -> -> (unit -> ('a * 'b *
...) my_type` and must be named `sample_my_type`.


So for example, if we want to test a function of type `col tree -> int`, so we need two samplers :
``` ocaml
(*Not a parametric type*)
let sample_col () = match Random.int 2 with
  | 0 -> B
  | _ -> R
      
(*A parametric type*)
let sample_tree (sample: unit -> 'a) : unit -> 'a tree =
  let rec builder h = match h with
    | 0 -> Leaf
    | n -> match Random.int 3 with
      | 0 -> Leaf
      | _ -> Node (builder (h-1), sample (), builder (h-1))    
  in
  let h = Random.int 5 + 2 in
  fun () -> builder h
```

The grading function is then simply :
```ocaml
let exercise_1 =
  Section ([ Text "Function: "; Code "height" ],
           test_function_1_against_solution
             [%ty: col tree -> int] "height"
             ~gen:5
             []
          )
```

With these two samplers, we are also be able, with no more effort, to
graduate a function of type `col tree -> col -> col tree` for
example. The grader is simply:

```ocaml
let exercise_2 =
  Section ([ Text "Function: "; Code "monochrome" ],
           test_function_2_against_solution
             [%ty: col tree -> col -> col tree] "monochrome"
             ~gen:5
             []
          )
```


### Advanced examples
More advanced examples (but nothing new) can be found in
`exercises/advanced_examples` directory (branch: step-4).

There is nothing new in these examples, only more complexed types, in
particular examples for functional types graded with both methods.



### Which method should I use ?

Both method works well for a lot of exercises. However for functional
types and tuples, you will need do give an alias to your types to be
able do use the second method (see the examples in `advanced_examples`).

---
[Previous step](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-3.md)

[Table of contents](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/howto-write-exercises.md)

[Next step](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-5.md)
