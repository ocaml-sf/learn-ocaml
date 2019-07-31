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
	grade_function_1_against_solution
		[%ty: color -> string] "color_to_string"
		~sampler: (fun () ->  match Random.int 4 with
		           | 0 -> Red | 1 -> Green | 2 -> Yellow | _ -> Blue)
		~gen:5
		[]
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
	grade_function_1_against_solution
		[%ty: color -> string] "color_to_string"
		~gen:5
		[]
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
has type `unit -> <arg1_type> * <arg2_type> * <arg3_type> * etc.`.

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
  
let exercise_1 =
	grade_function_2_against_solution
		[%ty: col tree -> col -> col tree] "monochrome"
		~sampler:(fun () -> sample_col_tree (), sample_col ())
		~gen:5
		[]
```

### Method 2: Defining a sampler

A sampler of a parametric type `('a * 'b * ... ) my_type` has a
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
let exercise_2 =
	grade_function_1_against_solution
		[%ty: col tree -> int] "height"
		~gen:5
		[]
```

Note that if instead of [col tree], the input type is [int tree] (or
another type with a predefined sampler), you need nothing more.
```ocaml
let exercise_2bis =
  grade_function_1_against_solution
    [%ty: int tree -> int] "height"
    ~gen:5
    []
```

With these two samplers, we are also able, without more effort, to
grade a function of type `col tree -> col -> col tree` for
example. The grader is simply:

```ocaml
let exercise_3 =
	grade_function_2_against_solution
		[%ty: col tree -> col -> col tree] "monochrome"
		~gen:5
		[]
```


### Advanced examples
More advanced examples (but nothing new) can be found in
`exercises/advanced-examples-step-4` directory (branch: step-4).

There is nothing new in these examples, only more complexed types, in
particular examples for functional types graded with both methods and
using the predefined sampler of list.

The user-defined type is:
```ocaml
type position = {x: int ; y: int}
```

and its corresponding sampler: 
```ocaml
let sample_position () = { x=sample_int () ; y=sample_int () }
```

#### First example: `get_x`

Exactly as shown previously, using metdho 2:
```ocaml 
let exercise_1 =
  grade_function_1_against_solution
    [%ty: position -> int ]
    "get_x"
    ~gen:5
    [{ x=0 ; y=0 }]
```

#### Second example: `map` (functional input type)

We want to grade the function 'map' for 'int list' so we need a
sampler for function of type 'int -> int'. 

This is not possible to use the naming convention for a functional
type without an alias (see method 2).

```ocaml
let sampler_fun () = match Random.int 3 with
  | 0 -> succ
  | 1 -> pred
  | _ -> fun x -> if x < 0 then -1 else 1
```
##### Method 1
For this method, we can just build the proper sampler for all the function arguments. 
```ocaml
let sampler_2 () =
  (sampler_fun (), sample_list ~min_size:1 ~max_size:10 sample_int ())

let exercise_2 =
  grade_function_2_against_solution
    [%ty:  (int -> int) -> int list -> int list ] "map"
    ~sampler:sampler_2
    ~gen:5
    [(succ, [])]
```

##### Method 2
For this method, we need to use an alias for type `int -> int`.

```ocaml
type f_int_int = int -> int

let sample_f_int_int = sampler_fun

let exercise_2bis =
  grade_function_2_against_solution
    [%ty:  f_int_int -> int list -> int list ] "map"
    ~gen:5
    []
```

#### Third example: 'first_elt' (tuple)
In case you want to grade a function with a tuple as an input type,
you can either use method 1 or define an alias and use method 2.

##### Method 1
```ocaml
let exercise_3 =
  grade_function_1_against_solution
    [%ty:  int * int -> int ] "first_elt"
    ~sampler: (fun () -> sample_int (), sample_int ())
    ~gen:5
    []
```

##### Method 2
```ocaml
type pair_int = int * int
let sample_pair_int () = sample_int (), sample_int ()
                  
let exercise_3bis =
  grade_function_1_against_solution
    [%ty:  pair_int -> int ] "first_elt"
    ~gen:5
    []
```


### Which method should I use ?

Both methods work well for a lot of exercises. However for functional
types and tuples, you will need do give an alias to your types to be able
do use the second method (see the examples in `advanced_examples`).
This is useful if you need to grade several functions that share some
input types.

