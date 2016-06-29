Simple Expressions
==================

Simple Expressions
------------------

Numerical expressions are easy to compute in OCaml.
For example:

    1+2

Strings are also easy to define.
For example, you can define a string with your name:

    "Mary"

The basic element of a string is a char:

    'a'

Lists and arrays
----------------

Lists and arrays are predefined data structures, for example you can
have a list of integers:

    [ 42 ; 1 ; 55 ]

An array of strings:

    [| "John" ; "Doe" |]

Operations on lists
-------------------

You can use some predefined functions on lists such as reversing the
list

    List.rev [ 1 ; 2 ; 3 ]

Get the first element of the list:

    List.hd [ 1 ; 2 ; 3 ]

Operations on arrays
--------------------

You can also use some predefined functions on arrays such as
concatenate two arrays

    Array.append [| 1 ; 2 |] [| 3 ; 4 ; 5 |]

To get the element number `i` of an array, you have two ways
to do this (don't forget, the first element has number `0`):

    Array.get [| 42 ; 51 ; 32 |] 2

or

    [| 42 ; 51 ; 32 |].(2)"
