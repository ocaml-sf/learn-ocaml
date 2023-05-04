(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2023 OCaml Software Foundation.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type markdown_doc = string * string (** title, markdown text *)

let exercises_pane_md =
  [%i"Handling exercises"], [%i{markdown|
> Don't forget to click "Apply" on the bottom-right after any changes

- **Filtering exercises**<br/>
  Use the 🔍 box on top.
  - type for raw text search on the titles
  - click again to list the tags and filter based on those
  - multiple search terms are allowed

- **Selecting which exercises can be seen by the students**
  - select the exercises with a click (the box on top (de)selects all; you can
    also click folder titles)
  - click the [Open/Close] button to toggle
  - for complex selections, use the filter box then select all

- **Defining exercise skill tags**<br/>
  Those can also be pre-defined in the exercises metadata.
  1. Select the exercises to tag
  2. Select or type a new tag in one of the boxes on the lower-right of the pane
     (left for requirements, right skills trained by the exercise)
  3. Click the corresponding [+] button to add the tag ([-] to remove)

- **Checking an exercise contents**
  - double-click an exercise
  - it will be opened in a new tab

- **Classifying students answers**<br/>
  As documented
[here](https://ocaml-sf.org/learn-ocaml/howto-classify-students-answers.html):
  - Assume you want to analyze students' definitions of function `foo` in
    exercise `exo`
  - Hold Ctrl (on macOS: Hold ⌘) and left click on `exo` in the list of
    exercises
  - Enter `foo` in the dialog box
  - The dissimilarity analysis will be opened in a new tab
|markdown}]

let students_pane_md =
  [%i"Handling students"], [%i{markdown|
> Don't forget to click "Apply" on the bottom-right after any changes

- **Filtering students**<br/>
  Use the 🔍 box on top.
  - type for raw text search on the nicknames
  - click again to list the tags and filter based on those
  - multiple search terms are allowed

- **Tagging students**<br/>
  Tags can be used for groups, level, etc.
  1. select the students to tag. To select a group, use the filter box and the
     box on top to select all. Use the `Sort by` box on the top right to easily
     select new students or untagged studends
  2. select or type the tag on the box to the lower-right
  3. Click the corresponding [+] button to add the tag ([-] to remove)

- **Following a student**
  - at a glance:
    a bar on the right of each student shows a short insight of their progress,
    - vertical stripes correspond to the exercises, in order
    - grey is shown for exercises not attempted
    - the color otherwise varies from green (good) to red (bad) depending on the
      grade
  - in detail:
    - double-click on the student line to open their track record in a new tab
    - there you can see their detailed results per exercise, and check their
      answers
    - see "Classifying students answers" in the exercise pane guide for a way
      to compare solutions between students

- **Downloading the grades**
  - use the [Actions] menu at the bottom to export a CSV file containing a line
    per student, and two columns (grade and date) per exercise
  - if any exercises or students are selected, the CSV will only contain those
  - otherwise everything is exported
|markdown}]

let assignments_pane_md =
  [%i"Handling assignments"], [%i{markdown|
Assignments provide a way to open select exercises to select students for a
given period of time. Once due, the students will be restricted to read-only
access.

- **Creating an assignment**
  1. Select exercises, as above
  2. Select students, as above. You can use the "any future students" line to
     apply to students who haven't yet registered
  3. Click the "New assignment" button at the bottom
  4. Edit the two dates (start and end of the assignment, resp.)
  5. Don't forget to [Apply] on the bottom right
  6. Click the assignment again to unselect it and quit assignment mode

- **Editing an assignment**
  - select it in the bottom pane
  - the corresponding exercises and students are automatically selected. Update
    the selection as needed
  - click the assignment again when finished (to quit editing mode), and don't
    forget to [Apply]

- **Following assignments**
  - an additional small "progression" bar is shown for each assignment in the
    students pane
  - past assignment dates are outlined in red
  - when selecting students, exercises assigned to any of them will be greyed
    out to prevent assigning multiple times the same exercise to the same
    student
  - conversely, when selecting exercises, students who already have them
    assigned are greyed out
|markdown}]
