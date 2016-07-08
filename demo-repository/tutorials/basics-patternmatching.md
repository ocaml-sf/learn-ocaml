Pattern-matching
================

Pattern-matching on integers
----------------------------

A powerful feature of OCaml is pattern-matching. For simple values as
integers, pattern-matching is quite similar to case switches in other
languages \( `_` corresponds to the default case\). Moreover, each case is
handled in chronological order:

    | let string_of_int x = match x with
    |    | 0 -> "zero"
    |    | 1 -> "one"
    |    | 2 -> "two"
    |    | _ -> "many"

    | let string_of_int2 = function
    |     | 0 -> "zero"
    |     | 1 -> "one"
    |     | 2 -> "two"
    |     | _ -> "many"

Pattern-matching on chars
-------------------------

Pattern-matching on characters is also possible, with a special syntax to
denote character ranges:

    | let is_capital = function
    |   | 'a' .. 'z' -> false
    |   | 'A' .. 'Z' -> true
    |   | _          -> failwith "Not a valid letter"

The `as` keyword can then be used to give a name to the actually matched
character:

    | let capitalize = function
    |   | 'a' .. 'z' as letter -> Char.uppercase letter
    |   | 'A' .. 'Z' as letter -> letter
    |   | _                    -> failwith "Not a valid letter"

Pattern-matching on tuples
--------------------------

Pattern-matching is also possible on tuples:

    | let fit str len = match (str,len) with
    |   | ("foo", 51) -> true
    |   | ("bar", 51) -> true
    |   | (_    , 42) -> false
    |   | _           -> (String.length str) = len

    | let fit str len = match (str,len) with
    |   | ("foo", 51)
    |   | ("bar", 51) -> true
    |   | (_    , 42) -> false
    |   | _           -> (String.length str) = len

when

    | let fit str len = match (str,len) with
    |   | (_,51) when (str="foo" || str="bar") -> true
    |   | (_, x) when x=42 -> false
    |   | _ -> (String.length str) = len

Pattern-matching on lists
-------------------------

However, the real power of pattern-matching appears when we start using more
structured values, when we start needing giving a name to matched patterns.
For instance, a list is either the empty list `[]` or a head and a tail,
denoted by the pattern `h::t` where `h` and `t` are fresh variables bound to
the matched patterns:

    | let head = function
    |   | []   -> failwith "empty list"
    |   | h::t -> h

    | let second_element = function
    |   | []      -> failwith "the list is empty"
    |   | [_]     -> failwith "the list contains only one element"
    |   | _::e::_ -> e

    | let head_head = function
    |   | []        -> failwith "the list is empty"
    |   | []::_     -> failwith "the head is the empty list"
    |   | (h::_)::_ -> h

Pattern-matching on arrays
--------------------------

You can also pattern match on arrays:

    | let has_size_two = function
    |   | [| _; _ |] -> true
    |   | _          -> false

    | let f = function
    |   | []                 -> failwith "empty list"
    |   | [| _; (_, x) |]::_ -> x
    |   | _                  -> failwith "the first array should be of size two"

Exhaustiveness
--------------

One of the benefit of pattern-matching is the exhaustiveness check done by
the compiler statically. Indeed, the OCaml compiler can verify that all the
cases are handled. For instance, when pattern-matching on a list, the
compiler will warn the user if she forgets to handle the empty list case:

    | let head_partial = function
    |   | h::_ -> h

    | let head = function
    |   | []   -> failwith "empty list"
    |   | h::_ -> h
    |   | [h]  -> h
