How to edit an exercise from the learn-ocaml platform?
======================================================

This tutorial explains how to create or edit an automatically graded exercise from the learn-ocaml platform.

Step 0 : Preliminaries

- An exercise description is composed of 8 files.

- `descr.html` contains the exercise statement as a sequence of
  HTML elements (that can be grafted in a `div`).

  - `meta.json` contains metadata about the exercise.

  - `prelude.ml` is an OCaml code fragment loaded in the toplevel before
  the student answer.

  - `prepare.ml` is an OCaml code fragment inserted after the prelude when
  the student answer is graded.

  - `solution.ml` is your answer to the exercise.

  - `template.ml` is the OCaml code fragment that initializes a fresh
    student answer.

  - `test.ml` is the grader code.

  - `title.txt` is a one-line file containing the title of the exercise.

### Do it yourself !

With the Editor view, this structure is grouped for facilitate you the creation of an exercise without forget a file for example.
For the moment, your list of exercises is probably empty but it will be populated by the next step of this tutorial.

## Step 1: Create a new exercise

As the first time you just need to click on "New exercise". You go to be redirected towards a form of metadatas.

- Fields to be filled

	- id : this is the identifier of the exercise. Careful, he must be unique, consisted of small letter, figure dash or underscore.
	
	- title : this is the title of the exercise, it is thanks to him that you will reconignize the exercise in the list. Careful, he must be unique.

	- description : a short description of the exercise, which will be posted in the list of the exercises created below its title. (Optional)

	- difficulty : a note between 0 and 4 allowing to place the difficulty of the exercise.

- Different buttons

	- Cancel : if you wish to cancel the cration of an exercise and to return to the main menu.

	- Save : if you wish to validate and pass following the creation of an exercise.

Congratulation you have just created an exercise !
For draft your exercise meeting in the step 2.

## Step 2 : Write your exercise

Now that you find yourselves in the view of edition of a exercise you can see various tabs. But let us proceed by stage.

1. First of all, position you in the tab Question where you have to write in Markdown (or HTML if you prefer) the various questions which will be asked to the pupil. Attention to write well the name of the various functions to have the same base for the correction.

2. Now that you have establishes the various functions which the student has to write. We recommend to you of draft them you even.
For it return on the tab Solution (or the left tab if you have the screen separated in two) and write the code of the various functions as on a basic publisher.	

	- button Check allows to verify if your solution is syntactically correct.

3. If wish to be sure that the student leaves on good bases to draft his code. We advise you to write him a template whom he can complete with his own solution.It is in the tab Template that all this takes place.

	- button Generate Template permit to write automatic the temple from your solution

4. Now that the solution is to write it is necessary to be able to test it. for it you have two possibilities:

	- High-level (tab Test): you don't know too much the syntax in use, or you wish to save time. This solution is made for you. You can generate automaticaly tests for every function of your solution (button Generate), or you can create them manually thanks to the headband New Question.
		
		- Solution: In this case you wish to test the solution of the student with your solution. For it you can supply a set of test (Arguments) or create randomly sets of tests.

		- Specification: Here you wish to look if the solution of the student verifies certain properties (as the double of the argument, a number bigger than 5). For it you can supply a set of test (Arguments) or create randomly sets of tests, et you have to give the specification of the function.

		- Suite: In this last case, you wish compared the solution of the student with results directly. For it you have to supply a set of couple (entered, taken out)

Watch out! In it is three cases you have to respect the syntax which is you explain directly on the platform.

If you have end to write all the tests you can compile for cross directly of this code high level in a code low level.

	- Low-level (tab Test.ml): you known the syntax to write the tests to be made for the solution. Then you can write if you wish directly the code in this buffer.

Here is you now wrote a ready for use exercise. But be you safe that he is correct? If you wish verify him, let us pass in the step 3.

## Step 3 : Test your exercise!

- Grade: First of all you can test your solution with her even, it allows to verify that the test is correct. If you haven't all the points it is that there is an error in the sets of tests.

- Experiment: You have an ended exercise and you wish to test it from the point of view of a pupil? We invite you to click on this button to realize this experience.

Congratulation you have just ended the creation of an exercise!
