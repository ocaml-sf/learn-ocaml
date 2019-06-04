## Step 7: Modifying the comparison functions (testers) with the optional arguments [~test], [~test_stdout], [~test_stderr]

### Tester `~test`
Tester are functions used to compare the student output result with
the solution output result. The output result can be either `Ok _` or
`Error _` (i.e. a raised exception).

#### Signatures of predefined testers and tester builders
See [Test_lib
documentation](https://github.com/ocaml-sf/learn-ocaml/blob/master/src/grader/test_lib.mli)
for more information. Some typical examples are shown below.

```ocaml
val test : 'a tester (* default value of optional argument [~test] *)
val test_ignore : 'a tester
val test_eq : ('a result -> 'a result -> bool) -> 'a tester
val test_eq_ok : ('a -> 'a -> bool) -> 'a tester
val test_eq_exn : (exn -> exn -> bool) -> 'a tester
val test_canon : ('a result -> 'a result) -> 'a tester
val test_canon_ok : ('a -> 'a) -> 'a tester
val test_canon_error : (exn -> exn) -> 'a tester
val test_translate : ('a -> 'b) -> 'b tester -> 'b Ty.ty -> 'a tester
```

#### Examples

In the examples below, we use the user-defined type :

```ocaml
type tri = Zero | One | Two
exception OutOfRange of int
```

For the first graded function, we want to be sure the student function
returned both the right `Ok` output and the right exception with its
is correct argument. The predefined tester that compares both possible
results with `Pervasives.compare` function is called `test`. This is
obviously the default value of optional argument [~test].

```ocaml
let exercise_1 =
  grade_function_1_against_solution
	[%ty: int -> tri] "convert"
	~gen:0
	~test:test (* optional since [test] is the default value of [~test] *)
	[-1; 0; 1; 2; 3]
```

For the second example, we want the student to return an `Failure` exception
when necessary but we don't care about the failure text. We can use
the predefined function `test_eq_exn` to redefine the comparison
function between exception.

```ocaml
let sample_tri () = match Random.int 3 with
  | 0 -> Zero
  | 1 -> One
  | _ -> Two

let exercise_2 =
  grade_function_2_against_solution
	[%ty: tri -> tri -> tri] "-"
	~gen:9
	~test:(test_eq_exn
		(fun exn1 exn2 -> match exn1, exn2 with
			Failure _, Failure _ -> true | _, _ -> false))
	[One, Two]
```

In this third example, we want the student to return a list but don't
care about its order. We use the predefined tester builder
`test_canon_ok` to apply a preprocess function to both student and
solution outputs. Here this preprocess function is simply a sorting
function.

```ocaml
let exercise_3 =
  grade_function_1_against_solution
	[%ty: int list -> tri list] "convert_list"
	~gen:5
	~sampler:(sample_list (fun () -> Random.int 3))
	~test:(test_canon_ok (List.sort compare))
	[]
```

### IO testers `~test_stdout` and `~test_stderr`
IO testers are used to compare string such are standard output.

By default, the values of `test_stdout` and `test_sdterr` are
`io_test_ignore`. In this case, the grader simply ignore any standard
and error outputs.

#### Signatures of predefined IO testers and IO tester builders
See [Test_lib
documentation](https://github.com/ocaml-sf/learn-ocaml/blob/master/src/grader/test_lib.mli)
for more information. Some typical examples are shown below.

```ocaml
  val io_test_ignore : io_tester
  val io_test_equals :
	?trim: char list -> ?drop: char list -> io_tester
  val io_test_lines :
	?trim: char list -> ?drop: char list ->
	?skip_empty: bool -> ?test_line: io_tester -> io_tester
  val io_test_items :
	?split: char list -> ?trim: char list -> ?drop: char list ->
	?skip_empty: bool -> ?test_item: io_tester -> io_tester
```

#### Examples

In these examples, we grade functions that print `tri`, `tri list` and
`tri list list` with the same `tri` type as previously.

```ocaml
type tri = Zero | One | Two
```

In the following examples, we don't care about the functions output
(that is always `()`) so we set `~test` to `test_ignore`.

In the first example, we want to compare the string standard
outputs. We can use the predefined function `io_test_equals` that
enables us to remove some chars (here spaces) at the beginning and the
end of the compared strings using the optional argument `~trim`.

```ocaml
let exercise_1 =
  grade_function_1_against_solution
	[%ty: tri -> unit]
	"print_tri"
	~gen:0
	~test:test_ignore
	~test_stdout:(io_test_equals  ~trim:[' '])
	[Zero; One; Two]
```

The two next examples show how to use the predefined functions
`io_test_items` and `io_test_lines`. The first one splits a string
using a list of given chars as separator and compares each resulting
items. The second one compares one by one each line of the given
strings.

```ocaml
let exercise_2 =
  grade_function_1_against_solution
	[%ty: tri list-> unit]
	"print_tri_list"
	~gen:3
	~test:test_ignore
	~sampler:(sample_list sample_tri)
	~test_stdout:(io_test_items ~split:[','] ~trim:[' '])
	[]

let exercise_3 =
  grade_function_1_against_solution
	[%ty: tri list list-> unit]
	"print_tri_list_list"
	~gen:4
	~test:test_ignore
	~sampler:(sample_list (sample_list sample_tri))
	~test_stdout:(io_test_lines ~test_line:(io_test_items ~split:[','] ~trim:[' ']) ~trim:[' '])
```

### A specific example: grading with a predicate

Here we want to grade a function by using a predicate: the output of
the graded function must satisfay a given predicate. It is actually a
specific use of `[~test]` where the comparison function between [Ok]
result need to be redefined.

The first function graded is a function that generates randomly
integer. We want the integer to be between 0 and 10.
```ocaml
let p x = if x >= 0 && x < 10 then true else false

let exercise_1 =
  grade_function_1_against_solution
	[%ty: unit -> int] "rand_int"
	~gen:10
	~test:(test_eq_ok (fun x _ -> p x))
	[]
```

Obviously we only check here that the output integer is in the right
range (but this is just a trivial example).

The second example is a function using the previous one to generate a list of integers.
```ocaml
let p_list l =
  (* Check all elements of the input list are in the right range*)
  let t = List.fold_left (fun a x -> a && p x) true l in
  if t then
	(* Check that there is at least two different elements *)
	let l = List.sort_uniq (Pervasives.compare) l in
	if List.length l > 1 then true else false
  else false

let exercise_2 =
  grade_function_1_against_solution
	[%ty: int -> int list] "rand_list"
	~gen:0
	~test:(test_eq_ok (fun x _ -> p_list x))
	[10 ; 20]
```

---
[Previous step](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/tutorials/step-6.md)

[Table of contents](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/howto-write-exercises.md)
