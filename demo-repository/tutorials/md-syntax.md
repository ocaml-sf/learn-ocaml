Markdown syntax tutorial for Learn OCaml tutorials
==================================================

Each tutorial starts with a main title
--------------------------------------

The main title must be a level 1 title, which in markdown syntax is
either of the following syntaxes.

    # Main Title

Or:

    Main Title
    ==========

It must be the very first element of the file. Tutorial written in
this syntax are valid Markdown documents, but they must respect some
strict structure. Not all Markdown files are valid tutorials).

Tutorials are split into a series of steps
------------------------------------------

Each section must start with alevel 2 title, which in markdown syntax is
either of the following syntaxes.

    ## Step Title

Or:

    Step Title
    ----------

The first section (and thus the first level 2 title) must directly
follow the main title.

Titles can contain enriched text.

Steps are split into paragraphs
-------------------------------

In markdown, two paragraphs are simply separated by an empty line.
There are three kinds of toplevel paragraph kinds allowed in tutorials.

  * Simple paragraphs of enriched text.

  * Code blocks.

  * Enumerations (one level only) using the standard bulleted syntax
    of Markdown.

Enriched text
-------------

The assumed character encoding is `UTF-8`.
The supported markdown escapes are the following.

  * Code using `` `this syntax` ``. Any newline will be ignored when
    using this code notation.

  * Runnable code using `` `| this syntax |` ``.

  * Math formulas using `` `$ this syntax $` ``. The contents have to
    be formatted in the ascii variant of Mathjax.js.

  * Emphasized parts `*like this*`.

Code blocks
-----------

Code that span on several lines must be put at the top level of the
tutorial, in a code block. You just have to put your code verbatim
with an indentation of at least 4 characters.

To make the code block runnable, use a vertical bar (`|`) at the
beginning of each line, just after the margin, as in the following
example (the quotes hilight the beginning and end of each line and are
not part of the syntax).

    `    | this text`
    `    |   is clickable`

Whish is displayed as follows:

    | this text
    |   is clickable

The four margin spaces, the vertical line and any optional margin on
the right of the vertical bar will be dropped.