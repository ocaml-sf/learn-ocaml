How to Assign Exercises Automatically
=====================================

## Context
If you are a teacher with a large number of students and want to assign a different set of exercises to each student based on their level and exercise difficulty, this can be a time-consuming process. 

With this feature, you can save time and effort when assigning exercises to your students on the Learn-OCaml platform.

## How-To
1. Assign each student with their current level. You can do this by adding a tag to each student's profile indicating whether they are in their 1st, 2nd, or 3rd year of their bachelor's degree. If no tag is provided, the system will assume that the student is in their 3rd year.

2. Choose which students you want to assign exercises to. You can select individual students, groups of students, or the entire class.

3. Choose which exercises you want to assign. You can select exercises based on difficulty level: basic (up to but not including 2 stars), intermediate (up to but not including 3 stars), or consolidated (everything from 3 stars).

4. Click the "Generate Assignment" button.

## Notes
The assignments are generated based on the student's level and exercise difficulty. The settings for this feature can be found in the file src/app/gen_assignment.ml from line 6 to 8. Each line contains an array of arrays of integers, which represent the possible assignments that a student from each corresponding level can receive. Each inner array represents one of these possible assignments, and each integer represents an exercise with one of the difficulty levels mentioned above.



