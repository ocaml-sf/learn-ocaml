A Taste of Imperative Programming
=================================

Variables
---------

As in other languages, you can associate a name to a value. To do that, we
use the `let` syntax which associates the result of some computation with a
name:

    | let x = 6 * 7

We can now check the value associated with `x` in the toplevel:

    | x

And we can use the name where we would like the value:

    | let y = x + 1

Mutable Variables
-----------------

In OCaml, you cannot change the value associated with a name after its
definition. Trying to do so will trigger a compilation error:

    | x <- x + 1

You will better understand the error message later.If you really want to
modify the value associated with a name, you must use a trick. OCaml provides
a function `ref` that creates a special value that can be modified later \(a
reference\):

    | let x = ref 42

You can then change the value of the reference associated with `x` using the
`:=` operator:

    | x := 100 / 4

You can also access the value contained in the reference using the operator
`!` :

    | let y = !x + 1

Sequences and Printing
----------------------

OCaml provides different functions to print basic types. For example:

    | print_int 3

and:

    | print_string "Hello"

Sequences of expressions are separated by `;` :

    | print_string "x = ";
    | print_int !x;
    | print_newline ()

A more powerful method to print values is `Printf.printf` , whose behavior is
similar to `printf` in C:

    | Printf.printf "x = %d. Bye %s\n" !x "John"

For loops
---------

Let's define a reference on a list:

    | let xl = ref []

OCaml provides a simple `for` loop to iterate on integers in a range:

    | for i = 1 to 10 do
    |   xl := i :: !xl;
    | done;
    | !xl

Here, for each value between 1 and 10, we have added it in this order at the
head of the reference.Of course, if we want them in the correct order, we
need to reverse the list:

    | List.rev !xl

Or we can just execute the loop directly from the upper bound to the lower
bound:

    | for i = 10 downto 1 do
    |  xl := i :: !xl
    | done;
    | !xl

Computing Conditions
--------------------

Boolean values in OCaml can be either `true` or `false` . They are often
created from comparing other values. For example:

    | 1 > 2

OCaml's comparison operators can be used on values of any type, not only on
numerical values. We can compare strings and characters too:

    | "aaaaaa" < "bbb"

    | "3" <= "22"

    | 22 >= 3

To test equality, you can use `=` , and for inequality, there is `<>` . For
example:

    | 1 = 1

    | 1 <> 1

    | "1" = 1

Oops, OCaml does not allow you to compare values with different types, so we
must be more careful:

    | "1" = string_of_int 1

If then else
------------

Now that we know how to test conditions, we can use them to choose between
computations. Let's define two values :

    | let a = 1 and b = 2

We can compute the minimum of them:

    | Printf.printf "min(%d,%d) = %d\n" a b
    |     (if a < b then a else b)

We can of course execute sequences within branches:

    | let z = if a < 100 then begin
    |     print_string "return at least 100 !";
    |     print_newline ();
    |     100
    |  end else a

While loops
-----------

OCaml also provides a `while` loop, to execute some code as long as an
expression is true:

    | while !x > 20 do
    |   print_int !x; print_newline ();
    |   x := !x - 2
    | done

Congratulations
---------------

You have seen that OCaml allows you to use some imperative style of
programming. Some functional programmers will tell you that side-effects are
bad: they are right, in many cases, but don't listen to them too early !In
the next lessons, you will see that by combining both imperative and
functional programming styles!
