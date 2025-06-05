How to include images in exercises
==================================

## Context

If you create an exercise, you may want to add your own static content such as images in the exercise description (`descr.html` or `descr.md`).

Learn-OCaml provides this feature, so that you can either add images hosted in an external domain, or images statically served by the Learn-OCaml server itself (which can be handy for summative exams, for example, when remote resources are blocked).

## How-to

In order to include `OCaml.png` in an exercise called `demo`, you just need to:

1. Create a directory `<repo>/exercices/demo/subdirectory`. (The name of the `subdirectory` is free.)
2. Put `OCaml.png` in there.
3. In `<repo>/exercises/demo/descr.html` (resp. `<repo>/exercises/demo/descr.md`), use the following syntax:  
`<img src="/static/demo/subdirectory/OCaml.png" alt="fallback text">` (resp. `![fallback text](/static/demo/subdirectory/OCaml.png)`).

Note that we can't use relative URLs, hence the leading `/`.

If you want to fix the width (or height) of an image in `descr.html` or `descr.md`, you can use the usual HTML syntax:  
`<img src="/static/demo/subdirectory/OCaml.png" alt="fallback text" width="400px">`