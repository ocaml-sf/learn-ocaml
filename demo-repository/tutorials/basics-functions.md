Functions
=========

Defining a one-argument function
--------------------------------

In OCaml defining a function with one argument, will look like this:

    | let incr n = n + 1

You can also use tuples:

    | let plus_and_divide (x, y, z) = (x + y) / z

Note that even though you have `x` , `y` and `z` as arguments, this syntax
with tuples means that you call the function plus_and_divide with *one*
argument which is a triple in this case. To call a function, nothing is
simpler:

    | incr 42

    | plus_and_divide (10, 20, 2)

Defining a multiple-arguments function
--------------------------------------

In C or Java, a function `plus` , will look like
`int plus (int x, int y) { return x + y; }` . Then the call to this function,
will be: `plus (1, 2);` In OCaml, the same function `plus` will be define
like follow:

    | let plus x y = x + y

To call this function, nothing is simpler:

    | plus 1 2

Note that there is no need to bracket or comma between function's parameters.

Returning multiple values
-------------------------

You will notice that in OCaml, there is no `return` statement; to return a
value, the whole body expression is implicitly returned.To return multiple
values, we will use tuples. For example:

    | (2, 3, 4, 5)

We can write functions which will return multiple values thanks to tuples.
For example:

    | let divide x y = (x / y, x mod y)

Then we get:

    | divide 10 3

Partial application
-------------------

It is possible to apply a number of parameters less than what is required by
a function. The result would be a partial application of a function.Let's
take the `plus` example:

    | let plus x y = x + y

Using the partial application, we could rewrite the function `incr` , by
giving just one argument to the function `plus` :

    | let incr = plus 1

In this way, `plus 1` will return a function arity 1:
`val incr : int -> int = <fun>` . To increment an integer by one, you can now
use the function `incr` :

    | incr 42

Similarly, we can define a function which double each integer passed as
argument of the function:

    | let mul x y = x * y

    | let double = mul 2

    | double 8

Anonymous functions
-------------------

In OCaml, we can write anonymous functions, functions defined without being
bound to an identifier. For example:

    | (fun x -> x + 1) 42

We can bound an anonymous function to an identifier. That's way, we have
severals ways to define functions:

    | let incr = fun x -> x + 1

    | incr 42

Iterators
---------

What is really fun is that you can now mix anonymous functions and iterators.
Let's take an example with list. If you want to increment all elements of a
list, you will use `List.map` :

    | List.map (fun x -> x + 1) [ 1; 2; 3; 4 ]

Here, `x` is an element of the list, `x + 1` is the operation that will be
done on `x` . The result will be a list with all its elements incremented by
1.Here is an example with `fold_left` . If you want to compute the sum of all
elements of a list:

    | List.fold_left
    |      (fun acc x -> acc + x)
    |      0
    |      [ 1; 2; 3; 4 ]

If we have :

    | let plus = fun acc x -> acc + x

then:

    | List.fold_left plus 0 [ 1; 2; 3; 4 ]

is equivalent to:

    | plus (plus (plus (plus 0 1) 2) 3) 4

To finish, you may need to print values from a list:

    | List.iter print_int [ 1; 2; 3; 4 ]

You can do the same sort of things on arrays too.
