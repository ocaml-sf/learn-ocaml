HTML syntax tutorial for Learn OCaml tutorials
==============================================

Main HTML structure
-------------------

Tutorials written in the HTML syntax are valid HTML files that can be
viewed in any Web browser (but not all HTML files are valid tutorials).

Here is an example HTML 5 stub that you can copy and past.

    <html>
      <head>
         <meta charset='UTF-8'>
         <title>This title is ignored.</title>
         <!-- Anything here is ignored, you can add some
              CSS of JS is you want your files to be readable
              outside of Learn OCaml. -->
      </head>
      <body>
         <!-- Here goes the tutorial contents -->
      </body>
    </html>

Each tutorial starts with a main title
--------------------------------------

The main title must be a level 1 title (`<h1>title</h1>`). It must be
the very first element of the html `<body>`.  Title can contain
enriched text.

Tutorials are split into a series of steps
------------------------------------------

Each section must start with alevel 2 title (`<h2>title</h2>`). The
first section (and thus the first level 2 title) must directly follow
the main title.

Steps are split into paragraphs
-------------------------------

There are three kinds of toplevel paragraph kinds allowed in tutorials.

  * Simple paragraphs of enriched text in a `<p></p>` markup.

  * Code blocks in a `<pre></pre>` markup.

  * Enumerations in a `<ul></ul>` block of `<li></li>` items.

Enriched text
-------------

The assumed character encoding is `UTF-8`.
The supported markdown escapes are the following.

  * Code using a `<code></code>` markup.
    Any newline will be ignored when using this code notation.

  * Runnable code by setting the `data-run` attribute (`<code data-run></code>`).

  * Math formulas by setting `data-math` attribute (`<code data-math></code>`).
    The contents have to be formatted in the ascii variant of Mathjax.js.

  * Emphasized parts `<em>like this</em>`.

Code blocks
-----------

Code that span on several lines must be put at the top level of the
tutorial, in a `<pre>` block. Unlike in plain HTML, any whitespace
margin on the left of the code will be dropped. As for blocks, the
`data-run` attribute makes code blocks runnable.
