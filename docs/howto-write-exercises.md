How to write an exercise for the learn-ocaml platform?
======================================================

This tutorial explains how to write an automatically graded exercise
for the learn-ocaml platform. As a prerequesite to this tutorial, make
sure you have followed the tutorial to setup your development
environment `howto-setup-exercise-development-environment.md`.

After this tutorial, you will be able to submit your exercise to an
exercise GIT repository if you want. The procedure for submission is
explained in `howto-submit-an-exercise.md`. If your exercises are not
to be shared, you can already deploy an instance of the learn-ocaml
platform using your local directory of exercises.


## Download the source files for this tutorial

All the files used in that tutorial are available on a GIT repository:

```bash
git clone git@github.com:yurug/learn-ocaml-tutorial.git
```

Each step of the tutorial is a branch in the repository. Therefore,
do

```bash
git checkout step-1
```

to get the files for step 1, and replace `step-1` by `step-2` to
get the files for the second step, and so on and so forth.

## The tutorials
[Step 0 : Preliminaries](../tutorials/step-0)
	
- Structure of an exercise
	
- Purpose of each file
	
[Step 1: Create a trivial exercise](../tutorials/step-1)
      
[Step 2: Basic grading by comparison with your solution](../tutorials/step-2)

- Simple example to grade by comparison with a solution

- With polymorphic functions

- With multiple arguments functions

[Step 3: Grading with generators for Ocaml built-in types](../tutorials/step-3)

- Generate tests by using the pre-construct samplers 

- Generate tests by defining its own sampler 

[Step 4: Grading with generators for user-defined types](../tutorials/step-4)

- Generate tests for non-parametric user-defined types 
	
- Generate tests for parametric user-defined types 

[Step 5 : More test functions](../tutorials/step-5)
	
[Step 6 : Grading functions for variables](../tutorials/step-6)
	
[Step 7 : Introspection of students code](../tutorials/step-7)
    
