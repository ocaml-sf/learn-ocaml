How to classify students answers
================================

## Context

If you are a teacher with a large number of students, you probably
want to get an idea of your students' typical answers to a given
question without having to look at each of them individually.

LearnOCaml provides an (experimental) feature to *automatically
classify students answers*. From a given identifier, LearnOCaml
computes the disimilarities between students' answers and regroup
them in different clusters, so that the teacher can only look at
one representant of each cluster to get a fairly comprehensive overview
of students' ways to approach the assignment.

## How-to

Let's see how to analyze students' definitions of function `foo` in exercise `x`.
Follow these steps:

1. Go to the teaching tab in the LearnOCaml Web UI (needs a teacher token).

2. Hold Ctrl (on macOS: Hold ⌘) and left click on the exercise `x` in the list of exercises.

3. Enter `foo` in the dialog box.

At this point, LearnOCaml opens a new window with the results.

## References

For more details about how this classification internally works, have
a look at [asak](https://github.com/nobrakal/asak), a library to
identify similar OCaml pieces of code.
