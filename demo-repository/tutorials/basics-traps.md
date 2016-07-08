Syntax Traps
============

Sequence of expressions
-----------------------

It is easy to get confused by OCaml syntax since it's different from
mainstream languages. So you'd better be aware of a few things before digging
any deeper. To begin with, you should know that a proper command must
normally end with `';;'` to be processed by the top-level. This tutorial
automatically adds the double semicolon as soon as you hit enter but the
normal top-level won't. The double semicolon is only required when
interacting with the top level interpreter and as such is not part of OCaml
syntax. What *is* part of OCaml syntax, though, is the simple semicolon `';'`
which is commonly used as an expression terminator, except that in OCaml it's
a *separator* . In other words, you must not write `expr1; expr2;` but
`expr1 ; expr2` . You may now enter `next ()` to check your understanding.

Exercise: Get the Punctuation Right!
------------------------------------

Let's see if you got this right. Here is a sequence of *erroneous* commands.
Your task is to fix all of them in order to get the correct answer at the
end.

    | let fernand = "King of Castille";

    | let rodrigue = "The cid"; let diegue = "cid's father"

    | characters = [ fernand;; rodrigue;; diegue ]

    | rodrigue.[4] <- 'C' ; diegue.[0] <- rodrigue.[4] ;

    | characters

The `let` keyword
-----------------

The other source of confusion for newcomers is the `let` keyword which acts
differently in the toplevel than in normal OCaml expressions.In the toplevel
`let x = 1` binds the name `x` to the integer 1 as seen in `lesson 2` . If
`x` was already bound to something its previous binding is lost:

    | let x = "I am now a string!"

The `let` keyword is also used to form an expression in which a name is given
to some value temporarily, for the evaluation of a subexpression only:
`let x = 41 in x + 1` The value of `x` is `41` during the evaluation of
`x + 1` only; the global binding of `x` to `"I am now a string!"` is
preserved. See what `x` is evaluated to now, and type `next ()` for a little
practice.

Exercise: let there be lets!
----------------------------

Fix all these `let` expressions in order to get the expected result at the
end:

    | let xy =
    |   let x = 'x' and let y = 'y' in x ::[y]

    | let ab =
    |   let a = 'a'
    |   let b = 'B' in Char.lowercase b
    |   in a ::[b]

    | let up = Char.uppercase in
    |   big_xy = List.map up xy ;
    |   big_ab = List.map up ab ;
    |   big_ab @ big_xy

Parentheses
-----------

With regard to grouping expression or enforcing order of evaluation, OCaml
syntax is surprisingly easy: you can use pervasively either parentheses or
`begin` / `end` keywords.Example grouping expressions in an `if` form:

    | if 1+2 = 3 then (
    |   print_string "did you knew that?\n" ;
    |   print_string "amazing!\n"
    | )

Or forcing order of evaluation with `begin` / `end` \(although you won't find
this often!\):

    | begin 1 + 2 end * 3

Also, as function application takes precedence over infix operators you will
frequently uses parentheses to make explicit the expected evaluation order,
as in: `square (1 + 1)` since `square 1+1` would yield `2` . Enter `next ()`
when you are ready to practice.

Exercise: Fix the grouping
--------------------------

A Lisp programmer stole all our parentheses! Get them back in order to get
the proper result at the end.

    | let ten =
    |   let double x = x+x in
    |   double 3 + 2

    | let hundred =
    |   if true || false then
    |     print_string "May I help you?\n" ;
    |     100
    |   else 0

    | let one =
    |   let accum = ref -54 in
    |   for i = 1 to ten do accum := !accum + i done ;
    |   !accum

    | one + match hundred with
    |   | 42  -> match ten with 10 -> 52  | _ -> 0
    |   | 100 -> match ten with 10 -> 110 | _ -> 0
    |   | _ -> -1
