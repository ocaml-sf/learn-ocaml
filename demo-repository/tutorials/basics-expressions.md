Simple Expressions
==================

Simple Expressions
------------------

Numerical expressions are easy to compute in OCaml. For example:

    | 1+2

Strings are also easy to define. For example, you can define a string with
your name:

    | "Mary"

The basic element of a string is a char:

    | 'a'

Lists and arrays
----------------

Lists and arrays are predefined data structures, for example you can have a
list of integers:

    | [ 42; 1; 55 ]

An array of strings:

    | [|"John"; "Doe"|]

Operations on lists
-------------------

You can use some predefined functions on lists such as reversing the list

    | List.rev [1; 2; 3]

Get the first element of the list:

    | List.hd [1; 2; 3]

Operations on arrays
--------------------

You can also use some predefined functions on arrays such as concatenate two
arrays

    | Array.append [| 1; 2 |] [| 3; 4; 5 |]

To get the element number `i` of an array, you have two ways to do this
\(don't forget, the first element has number `0` \):

    | Array.get [| 42; 51; 32 |] 2

or

    | [| 42; 51; 32 |].(2)

Basic operations on strings
---------------------------

Strings are sequences of chars. It is possible to create a string from just
one char, using:

    | String.make 10 'x'

Strings can also be created by joining other strings:

    | "Mary" ^ " and " ^ "John"

It might also be interesting to get the length of a string:

    | String.length "abcdefghijklmnopqstuvwxyz"

\(and yes, one character is missing !\)

More operations on strings
--------------------------

There are many operations available on strings, like a function which returns
a copy of the argument, with all uppercase letters translated to lowercase:

    | String.lowercase "MARY"

Here's a more practical example, generating a filename from components:

    | String.concat "/" [""; "usr"; "local"; "bin"]

Note that the second argument is a list of strings.It is sometimes useful to
extract an integer from a string. This is easy in OCaml:

    | int_of_string "546"

Operations on Tuples
--------------------

A tuple consists of values \(zero or more\), enclosed in parentheses and
separated by commas. Note that you can have differents types for each
element, remember

    | (42, "John", true)

When you have a pair \(a tuple with two elements\), you can use some
predefined functions like get the first element:

    | fst (42, "John")

Or the second element:

    | snd (42, "John")
