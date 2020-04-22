# Step 8: Reusing the grader code

This step explains how to separate the grader code, and eventually reuse it in other exercises. 

During the grading, the file **test.ml** is evaluated in an environment that contains notably: 
- **prelude.ml** and **prepare.ml** ;
- the student code isolated in a module `Code` ;
- **solution.ml** in a module `Solution` ;
- the grading modules **Introspection**, **Report** and **Test_lib**.

### Separating the grader code

It is possible to extend this environment by declaring some other user-defined modules in an optional file **depend.txt**, located in the exercise directory.

Each declaration in **depend.txt** is a single line containing the relative path of an *.ml* or *.mli* file. The order of the *.ml* declarations specifies the order in which each module is loaded in the grading environment.

By default each dependency *foo.ml* is isolated in a module *Foo*, which can be constrained by the content of an optional signature file *foo.mli*. Furthermore, we can add an annotation `@included` at the beginning of a file *foo.ml* to denote that all the bindings of *foo.ml* are evaluated in the toplevel environment (and not in a module *Foo*). 

Dependencies that are not defined at the root of the exercise repository are ignored by the build system: therefore, if you modify them, do not forget to refresh the timestamp of `test.ml` (using `touch` for instance).

### A complete example

Let's write an exercise dedicated to *Peano numbers*. Here is the structure of the exercise:

```
.
├── exercises
│   ├── index.json
│   └── lib
│   │   ├── check.ml
│   │   └── check.mli
│   ├── peano
│   │   ├── depend.txt
│   │   ├── descr.md
│   │   ├── meta.json
│   │   ├── prelude.ml
│   │   ├── prepare.ml
│   │   ├── solution.ml
│   │   ├── template.ml
│   │   ├── test.ml
│   │   └── tests
│   │   │   ├── samples.ml
│   │       ├── add.ml
│   │       └── odd_even.ml
│   ├── an-other-exercise
│   │   ├── depend.txt
│   │   │ ...
```
The exercise **peano** follows the classical format : **prelude.ml**, **prepare.ml**, **solution.ml**, **template.ml** and **test.ml**. It also includes several dependencies (**check.ml**, **samples.ml**, **add.ml** and **odd_even.ml**) which are declared as follows in **depend.txt**:

```txt
../lib/check.mli
../lib/check.ml    # a comment

tests/samples.ml
tests/add.ml
tests/odd_even.ml
``` 

Here is in details the source code of the exercise :

- **descr.md**

> * implement the function `add : peano -> peano -> peano` ; 
> * implement the functions `odd : peano -> bool` and `even : peano -> bool`.

- **prelude.ml**
```ocaml
type peano = Z | S of peano
```

- **solution.ml**
```ocaml
let rec add n = function
| Z -> n
| S m -> S (add n m)

let rec odd = function
| Z -> false 
| S n -> even n
and even = function
| Z -> true
| S n -> odd n
```

- **test.ml**
```ocaml
let () =
  Check.safe_set_result [ Add.test ; Odd_even.test ]
```

Note that **test.ml** is very compact because it simply combines functions defined in separated files.

- **../lib/check.ml**:
```ocaml
open Test_lib
open Report

let safe_set_result tests =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
    List.mapi (fun i test -> 
                Section ([ Text ("Question " ^ string_of_int i ^ ":") ],
                         test ())) tests
```
- **../lib/check.mli**:
```ocaml
val safe_set_result : (unit -> Report.t) list -> unit
```

- **tests/add.ml**: 
```ocaml
let test () =
  Test_lib.test_function_2_against_solution
    [%ty : peano -> peano -> peano ] "add"
    [ (Z, Z) ; (S(Z), S(S(Z))) ]
```
- **tests/odd_even.ml** :
```ocaml
let test () =
  Test_lib.test_function_1_against_solution
    [%ty : peano -> bool ] "odd"
    [ Z ; S(Z) ; S(S(Z)) ] 
  @
  Test_lib.test_function_1_against_solution
    [%ty : peano -> bool ] "even"
    [ Z ; S(Z) ; S(S(Z)) ]
```
Remember that **Test_lib** internally requires a user-defined sampler `sample_peano : unit -> peano` to generate value of type `peano`. This sampler has to be present in the toplevel environment -- and not in a module -- in order to be found by the introspection primitives during grading. Therefore, we define this sampler in a file starting with the annotation `@included`.
- **samples.ml**:
```ocaml
@included

let sample_peano () =
  let rec aux = function
  | 0 -> Z
  | n -> S (aux (n-1)) 
  in aux (Random.int 42)
```

Finally, **test.ml** will be evaluated in the following environment:

```ocaml
val print_html : 'a -> 'b
type peano = Z | S of peano
module Code :
  sig
    val add : peano -> peano -> peano
    val odd : peano -> bool
    val even : peano -> bool
  end
module Solution :
  sig
    val add : peano -> peano -> peano
    val odd : peano -> bool
    val even : peano -> bool
  end
module Test_lib : Test_lib.S
module Report = Learnocaml_report
module Check : sig val check_all : (unit -> Report.t) list -> unit end
val sample_peano : unit -> peano
module Add : sig val test : unit -> Report.t end
module Odd_even : sig val test : unit -> Report.t end
```

In the end, this feature can provide an increased comfort for writing large automated graders and for reusing them in other exercises.


