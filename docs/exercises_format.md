This section describes the format parsed by `learn-ocaml` for
exercises, tutorial and lessons.

# Exercices and groups format


## Groups

A group is either a set of exercices or other groups. As such exercices are classified
into a tree representation, where groups are nodes and leafs are exercises. This
tree (or index of exercises) can be described by a file `index.json` at the root
of the `exercises` directory from the repository. If no `index.json`
is found, the subdirectories are scanned and all exercises found are returned.

The file `index.json` has the following format (in version "1"):
```
<group name> = <string>
<directory> = <string>

<exercices> = "exercises : [ <directory>, .., <directory> ]
<groups> =
  "groups" :
    { ( <group name> : { "title" : <string>, (<groups> | <exercices> ) } )+ }

{ "learnocaml_version": <string>, <groups> }
```

For example, this `index.json` describes two groups of exercices, one with only
one exercise, two in the other:
```
{ "learnocaml_version": "1",
  "groups":
  { "imperative-traits":
    { "title": "Imperative traits",
      "exercises": [ "mooc-simple-input-output" ] },
    "data-structures":
    { "title": "Data Structures",
      "exercises": [ "mooc-binary-search-trees",
                     "mooc-trie-data-structure" ] } } }
```

This format reflects how exercises should be classified into the application.
If this file does not exists, the exercises are simply put at the root of the
index.

## Exercise

An exercise is described by a directory containing at most the following files:
- meta.json
- descr.html
- prelude.ml
- prepare.ml
- template.ml
- solution.ml
- test.ml
- max_score.txt
- test_libs.txt

> Note: as of learn-ocaml 1.0, the `.ml` files get compiled into the exercise.
> It is therefore not possible to use directives like `#install_printer`.
> However, you can still define your own printers in a way similar to defining
> custom `sample_<type>` functions:
>
> ```ocaml
> (* Custom printer for a pre-defined type *)
> let print_float ppf x = Format.fprintf ppf "%.2f" x
> 
> (* Name the alias to define a printer for a specific instanciation of a
>    generic type *)
> type int_list = int list
> let print_int_list ppf l = ...
>
> (* Define a generic printer for a generic type *)
> let print_result ppok pperr ppf = function
>   | Ok ok -> Format.fprintf ppf "OK(%a)" ppok ok
>   | Error err -> Format.fprintf ppf "ERR(%a)" pperr err
> ```
>
> Printers defined in `prelude.ml` or `prepare.ml` affect the toplevel and the
> grader. Printers defined in `test.ml`, obviously, affect only the grader.

### meta.json

Json description with the following format (in version "1"):
```
{ "learnocaml_version" : "1",
  "kind"               : "exercise" | "problem" | "project",
  "stars"              : [1 .. 5]
}
```

Json description in version "2" contains more metadata:
```
{
  "learnocaml_version" : "2",
  "kind"               : "exercise" | "problem" | "project",
  "stars"              : [1 .. 5],
  "title"              : "Title of the exercise",
  /* In an exercise repository, each exercise must have a unique identifier. */
  "identifier"         : "some_unique_identifier",
  /* Authors with their emails. */
  "authors"            : [["Xavier Leroy", "some@email"], ["Damien Doligez", "someother@email"]],
  /* The skills and concepts that are practiced by this exercise. */
  "focus"              : ["skill1", ..., "skillN", ..., "concept1", ..., "conceptM"],
  /* The skills and concepts that are required to do this exercise. */
  "requirements"       : ["skill1", ..., "skillN", ..., "concept1", ..., "conceptM"],
  /* The suggested exercises in case of success */
  "forward_exercises"  : [ "exercise1", "exercise2", ... ],
  /* The suggested exercises in case of difficulty */
  "backward_exercises" : [ "exercise1", "exercise2", ... ],
  /* Maximum score for the exercise. */
  "max_score" : [ 0 .. n ],
}
```

### descr.html

Description of the exercise in HTML format.

### prelude.ml

OCaml file containing the definitions that are known by the user. In the
web-application, the prelude is shown after the exercise description. The
prelude is loaded in the environment before any other `.ml` files from the
exercise directory.

### prepare.ml

OCaml file containing some definitions that are unknown by the user. As such, it
can be used to to redefine some functions, or execute some code. For example, it
is useful to redefine some standard library functions to track their usage for
algorithmic purpose. `prepare.ml` is loaded directly after `prelude.ml` in the
environment.

### template.ml

OCaml file containing a template of code for the exercise, and loaded into the
editor by default if the user does not have a saved code for the current
exercise.

### solution.ml

OCaml file containing the solution for the exercise. It can be used for testing
the user's code by comparing its results with a code reference.

### test.ml

OCaml file containing the test program that is used to check and grade the user's code and
the solution. It has access to many functions allowing the introspection of the
code, which will be described and detailed in another section.

### max_score.txt

Maximum score that is possible to get for this exercise, even if the grader
grades more. Overridden by the field `max_score`, if present in `meta.json`.

### test_libs.txt

List of additional libraries (one per line) to be used by the grader. The
libraries will be looked up using `ocamlfind`, available to `test.ml` during its
compilation, and bundled in the exercise grader.

# Metadata

When building the corpus and extracting the metadatas of all exercises, the
builder will generate two files `requirements.json` and `focus.json`, which
aggregates every skill present in their respecting fields in every
`meta.json`. It associates a skill to the list of exercises that requires it (or
focuses on it). These files behave as some sort of static database for this
information and should not be written by hand.

Their format is the following:

```
[{ "<skill name>" : [ "<exercise_id>" ; .. ; "<exercise_id>" ] }; .. ]
```
