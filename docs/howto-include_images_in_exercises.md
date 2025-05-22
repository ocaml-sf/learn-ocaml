How to include our own static content to an Exercise Statement
==============================================================

## Context

If you create an exercise statement, you probably want to add your own static content such as images in an exercise statement. 

`LearnOCaml` provides a feature to include static contents in an exercise statement. In addition to always being able to include a link to external content, it's possible to include specific static content. 

## How-to

Lets see how to include `OCaml.png` in exercise `demo`.
Follow these steps:

1. Create a directory `<repo>/exercices/demo/images`.
2. Put `OCaml.png` in there.
3. In `<repo>/exercises/demo/decr.html` or `<repo>/exercises/demo/descr.md`, use the following syntaxe <br> `<img src="/static/demo/images/OCaml.png">`. 

To do this we can't use relative URLs. 