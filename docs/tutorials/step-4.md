# Step 4: Grading with generators for user-defined types

In the case of user-defined types, it is mandatory to define a
sampler. Both two previous methods (defining a sampler function
`sample_my_type` or using the `~sampler` optional argument) can be
used but required a little more work, especially for parametric types.

## Non parametric type
For non-parametric type, it is exactly the same than previously. 

You can find the examples below in the
`exercises/sampler-user-defined-types` directory (branch: step-4).

In the examples, we use the type `color` defined as :
```ocaml 
type color = Green | Yellow | Red | Blue
```

### Method 1:  using the `~sampler` argument

As previously, you can simply add the argument `~sampler` of type
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
 Same than above: a sampler of type `unit -> my_type` has to be named
 `sample_my_type`.

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

No change here, just don't forget that `~sampler` has type 
`unit -> <arg1_type> * <arg2_type> * <arg3_type> * etc.`

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

More advanced examples (but nothing new) can be found in
`exercises/advanced_examples` directory (branch: step-4).

---
<div style="text-align: right">[Previous step](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-3.md)</div>
<div style="text-align: right">[Table of contents](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/howto-write-exercises.md)</div>
<div style="text-align: right">[Next step](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-5.md)</div>
