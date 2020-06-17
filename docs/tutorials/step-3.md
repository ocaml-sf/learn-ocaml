# Step 3: Grading with generators for Ocaml simple built-in types
 You can find the examples below in the
  `exercises/sampler-built-in-types` directory (branch: step-3).

As see previously, you can either give manually inputs for the tested
functions or you can ask the grader to automatically generate inputs.

For simple built-in types, the grader actually do most of the work for
you: you only need to precise the number of inputs sets you want to be
generated and the grader will use predefined sampler.

## Which types have a predefined sampler ?

Before beginning, we should make an important warning : there is no
default sampler for functional types, tuples (or type composed of
tuple) for example. Here is a list of the signatures of predefined
samplers :

```ocaml
  val sample_int : int sampler
  val sample_float : float sampler
  val sample_list : ?min_size: int -> ?max_size: int -> ?dups: bool -> ?sorted: bool -> 'a sampler -> 'a list sampler
  val sample_array : ?min_size: int -> ?max_size: int -> ?dups: bool -> ?sorted: bool -> 'a sampler -> 'a array sampler
  val sample_option : 'a sampler -> 'a option sampler
  val sample_string : string sampler
  val sample_char : char sampler
  val sample_bool : bool sampler
```

## How to use a default sampler
When the function to test has inputs of type with predefined sample,
you have nothing to do. In the example below, five tests are
automatically generated.

```ocaml
let exercise_1 =
  test_function_1_against_solution
    [%ty: int -> int] "identity"
    ~gen:5 [0]

```

However, with this method, you have no control on how the inputs are
generated. If, for example, you want a function of type `int -> int ->
int` to be tested for inputs between 12 and 42, you need to give the
grade function the sampler you want the same way you will to for type
without predefined sampler. There are actually two ways do to that:


## Method 1 : using the `~sampler` argument
The more general way do provide a sampler is to use the optional
argument `~sampler` that has type:

`unit -> <arg1 type> * <arg2 type> * <arg3 type> etc.`.


```ocaml
let exercise_2 =
  test_function_2_against_solution
    [%ty: int -> int -> int] "pi1"
    ~sampler:(fun () -> (Random.int 31 + 12, Random.int 31 + 12) )
    ~gen:5
    []
```

## Method 2 : redefining the corresponding sampling function.
Another way is to define a sampling function of type `unit -> <arg1
type> * <arg2 type> * <arg3 type> etc.` using the naming convention :
`sample_<type>`. In this case, nothing needs to be add to the grade
function call.

```ocaml
let sample_int = Random.int 31 + 12

let exercise_3 =
  test_function_2_against_solution
    [%ty: int -> int -> int] "pi1"
    ~gen:5
    []
```

## More avanced examples
 You can find the examples below in the
  `exercises/advanced-examples-step-3` directory (branch: step-3).

There is nothing new to learn in this part, there are only more
examples of how to build a sampler for more complexed types. In
particular, there are examples with:

* list
```ocaml
let exercise_1 =
  test_function_2_against_solution
    [%ty: int -> int list -> int list] "push"
    ~gen:5
    []
```
* tuple
```ocaml
let exercise_2 =
  test_function_1_against_solution
    [%ty: (int * int) -> int] "first"
    ~gen:5
    ~sampler:(fun () -> (Random.int 10, Random.int 10))
    []
```

* type option
```ocaml
let exercise_3 =
  test_function_1_against_solution
    [%ty: int option -> int] "opt"
    ~gen:5
    []

let sampler_4 () =
  let sampler_tuple () = (sample_int (), sample_int ()) in
  (sample_option sampler_tuple) ()

let exercise_4 =
  test_function_1_against_solution
    [%ty: (int * int) option -> int] "opt_add"
    ~gen:5
    ~sampler:sampler_4
    []
```

* functional type

```ocaml
let sampler_5 () =
  let sampler_f () = match Random.int 3 with
    | 0 -> succ
    | 1 -> pred
    | _ -> fun _ -> 0 in
  sampler_f (), sample_int ()


let exercise_5 =
  test_function_2_against_solution
    [%ty: (int -> int) -> int -> int] "apply"
    ~gen:5
    ~sampler:sampler_5
    []

```

* array

```ocaml
let sampler_6 =
  sample_array ~min_size:1 ~max_size:10 sample_int

let exercise_6 =
  test_function_1_against_solution
    [%ty: int array -> int list] "array_to_list"
    ~gen:5
    ~sampler:sampler_6
    []
```




