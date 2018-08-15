How to use Learn-OCaml in my classroom
======================================

You want to teach OCaml programming using Learn-OCaml? This
documentation provides you an overview of the steps required to use
Learn-OCaml in your classroom.

## Step 1: Deploy a learn-ocaml instance on your own machine

To prepare your teaching material, you need to have a working instance
of Learn-OCaml on your machine. Follow the tutorial:

[How to setup an environment to develop exercises?](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/howto-setup-exercise-development-environment.md)

## Step 2: Write exercises

Learn how to write your first exercises using our dedicated tutorials:

[How to write exercises?](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/howto-write-exercises.md)

## Step 3: Deploy the infrastructure

Let us assume that you have developed the exercises for the first week
of your semester and that you want to setup the infrastructure to check
that your students will be able to actually work on these exercises.

Ask your system administrator to do two things:

1. Deploy an instance of Learn-OCaml on a server (reachable by your students).
   Here is a documentation that your system administrator can follow:

[How to deploy an instance of the Learn OCaml server?](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/howto-deploy-a-learn-ocaml-instance.md)

2. If you want your students to use the command line (not the web
   application) to submit their solutions, you need Learn-OCaml to be
   installed on the lab machines or students' machines. The procedure
   is documented here:

[How to install the Learn OCaml client?](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/howto-install-learn-ocaml-client.md)

## Step 4: Initialize student accounts

Each student must acquire a secret token from the server to be able to
store and to submit answers to the server. The account creation is
done directly on the web application or when running the command line
client `learn-ocaml-client` for the first time.

We recommend that you create a fake student account to test that everything
works fine. Create this account by doing:

```
learn-ocaml-client
```

This command will ask you for the server URL and some basic information
about this user.

In any case, as a teacher, you will be assigned a special account that
will allow you to assign tag to student users to be able to sort them.
You can use the command line to do that:

```
learn-ocaml-admin --tag class2018 login1 login2 login3 ...
```

## Step 5: Upload your exercise repository to the server

At any time, you can upload your exercise repository to the server to
make new exercises available or the modify existing ones. From your
repository, simply do:

```
learn-ocaml-admin --update .
```

### Step 6: Check that your students can work

A student can see homework assignments by browsing the web
application. The student can program inside the browser
or write the answer in a file. This file can be submitted
using the command line tool:

```
learn-ocaml-client --id exercise-id myanswer.ml
```

Notice that each exercise has an identifier that is displayed
on the web application.

Check that you can answer your exercise using the fake student account.

### Step 7: Download students answers

You can get your student grades from the web application or by:

```
learn-ocaml-admin --tag class2018 download-grades
```

This will generate a CSV file with three columns -- the student
username, the exercise identifier and the number of points.

### Run your course during the semester

Steps 5, 6 and 7 can be repeated at will during the semester.

