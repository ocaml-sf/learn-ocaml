# Step 0 : Preliminaries

The structure of your local directory of exercises must follow a
specific shape, illustrated by the following ascii art:

```
.
├── exercises
│   ├── exercise1
│   │   ├── descr.html
│   │   ├── meta.json
│   │   ├── prelude.ml
│   │   ├── prepare.ml
│   │   ├── solution.ml
│   │   ├── template.ml
│   │   └── test.ml
│   ├── exercise2
│   │   ├── ...
│   ├── index.json
├── lessons
│   ├── somelesson.json
│   └── lessons.json
├── tutorials
│   ├── index.json
│   ├── some-tutorial.html
│   ├── some-other-tutorial.md
```

The complete format specification for exercise description is given in
[docs/exercises_format.md](../../exercises_format.md).

- The directory `exercises` must contain the descriptions of the
  exercises. Each exercise description is stored in a dedicated
  subdirectory. On this example, the directory `exercises` contain two
  exercises `exo1` and `exo2`. The file `index.json` puts some structure
  over this set of exercises, for instance:

```json
{
  "learnocaml_version": "1",
  "groups":
  { "group1":
    {
      "title": "Some group of exercises",
      "exercises": [ "exo1", "exo2" ]
    }
  }
}
```

- An exercise description is composed of 8 files.

  - `descr.html` contains the exercise statement as a sequence of
  HTML elements (that can be grafted in a `div`).

  - `meta.json` contains metadata about the exercise including the title of the exercise.

  - `prelude.ml` is an OCaml code fragment loaded in the toplevel before
  the student answer.

  - `prepare.ml` is an OCaml code fragment inserted after the prelude when
  the student answer is graded and hidden to the student.

  - `solution.ml` is your answer to the exercise.

  - `template.ml` is the OCaml code fragment that initializes a fresh
    student answer.

  - `test.ml` is the grader code.

- `lessons` and `tutorials` are ignored in this tutorial.

## Do it yourself!

Reproduce this structure in your own exercise directory. For the moment,
your list of exercises is probably empty but it will be populated by the
next step of this tutorial.
