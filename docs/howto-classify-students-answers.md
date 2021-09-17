How to classify students answers
================================

# Context

If you are a teacher with a large number of students, you probably
want to get an idea of your students' typical answers to a given
question without having to look at each of them individually.

LearnOCaml provides an (experimental) feature to *automatically
classify students answers*. Provided a given identifier, LearnOCaml
computes the disimilarities between students' answers and regroup
them in different cluster, so that the teacher can only look at
one representant of each cluster to get a quite complete feeling
of students' ways to approach to the assignment.

# How-to

To analyze students' definitions of function `foo` in exercise `x`.
Follow these steps:

1. Go to the teaching workspace.

2. Click with the middle click on the exercise `x` in the list of exercise.

3. Enter `foo` in the dialog box.

At this point, LearnOCaml opens a new window with the results.

# References

For more details about how this classification internally works, have
a look at [assak](https://github.com/nobrakal/asak), a library to
identify similar OCaml pieces of code.
