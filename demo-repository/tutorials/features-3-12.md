New Features in OCaml 3.12
==========================

Local opens
-----------

Since OCaml 3.12, you can locally open a module within an expression, using
the `let open` syntax. It can be very useful when accessing multiple values
of a modules, without exposing them elsewhere:

    | let open List in
    | map fst (map (fun x -> (x,1)) [1;2;3;4])

Local opens
-----------

You can also open a module for an expression within parenthesis:

    | let int_6L = Int64.(add 1L (sub 7L 2L))

Abbreviated Record Notation
---------------------------

A new notation has been introduced to switch between record labels and local
variables.If you have defined a record for vectors like:

    | type vect = { x : float; y : float; }

You can easily bind its labels to canonical variables in a pattern matching:

    | let norm { x; y } = sqrt (x *. x +. y *. y)

You can also create new records using the same notation:

    | let vect x y = { x ; y }

Record Labels Exhaustiveness
----------------------------

You can now verify the exhaustiveness of pattern-matchings on records, when
the `R` \(or `9` \) warning is activated.Let's activate this warning with the
default ones:

    | #warnings "+a-4-6-7-27..29"

We can now implement a function that does not use all the labels and see what
happens:

    | let abs { x } = x

To avoid the warning, we must tell the compiler that we are aware that some
labels are missing:

    | let abs { x; _ } = x

Explicit Polymorphic Type Annotations
-------------------------------------

OCaml 3.12 introduces a new syntax to require that a type be
polymorphic.Let's start with what was there before. Many people thought that
they could already force a polymorphic type on a function:

    | let f : 'a -> 'a = function x -> x + 1

Although you might have thought you were forcing a polymorphic type on the
function, you were only enforcing a constraint of equality between the
argument type and the result type.With explicit polymorphism, you can force
the polymorphism \(and get an error, of course\):

    | let f : 'a.'a -> 'a = function x -> x + 1

This notation can also be used to define really polymorphic recursive
methods.Let's define a new tree type:

    | type 'a t = Leaf of 'a | Node of ('a * 'a) t

If we try to compute the depth of that tree, OCaml will complain:

    | let rec depth = function Leaf _ -> 1 | Node x -> 1 + depth x

Indeed, depth is a recursive function, and the typer does not allow to use it
will two different types of arguments. But now, with explicit polymorphism,
you can tell the typer that it's ok to do so, since the function is
polymorphic:

    | let rec depth : 'a. 'a t -> _ = function Leaf _ -> 1 | Node x -> 1 + depth x
