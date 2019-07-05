How to practice OCaml with Learn-OCaml
======================================

If you are a student that wants to know how to setup your environment
to practice your OCaml skills with Learn-OCaml, this tutorial is for
you. There are two ways to use the Learn-OCaml system: in web
application, directly on the browser (hence, wit zero
configuration) ; or with a command line tool named
`learn-ocaml-client`.

## Using Learn-OCaml in the browser

Your teacher must have provided an URL for the Learn-OCaml instance
used in your OCaml course. Then, to start using the platform:

- Open a browser on this URL.

- Fill the nickname to be identifiable by your teacher.

- Create a token.

- Click on the exercise button. At this point, you should see a list
  of exercises.

- Select the exercise you want to work on.

- Read the exercise statement and use the text editor to complete the
  source code.

- Once you think you have a valid solution for the exercise, click on
  the "Grade" button. This will automatically check if your answer is
  indeed correct and save it on the server.

*Important note*: Write down your token! This is the information that
will allow you to recover your session from another machine.

## Using Learn-OCaml from the command line

As an experienced programmer, you probably want to use your favorite
editor (Emacs, Vim, Atom, VSCode, etc) to program in OCaml. This is
possible thanks to our command line tool named `learn-ocaml-client`!
Yet, it requires more efforts in terms of setup. Maybe your teacher
already installed `learn-ocaml-client` on the computer lab machines.

### Installation of `learn-ocaml-client`

There are two methods to install `learn-ocaml-client`.

#### Method 1: Using `opam`

If you are already using the OCaml package manager OPAM, simply do:

```
opam install learn-ocaml
```

#### Method 2: Compiling `learn-ocaml` by hand

Note: you need a working ```opam``` environment (at least `2.0.0~rc2`).

* Clone the git repository on your machine
```
git clone git@github.com:ocaml-sf/learn-ocaml.git && cd learn-ocaml
```

* Install the dependencies using:
```
opam install opam-installer && eval $(opam env) && make build-deps
```

* Compile the learn-ocaml management tool using:
```
make && make opaminstall
```

### Using `learn-ocaml-client`

#### Initializing configuration file

The first time you run `learn-ocaml-client`, you will be asked for the
URL of the Learn-OCaml instance you want to use and you will also be
asked to enter your nickname and your token.

Once this is done, this information is stored in the file
`~/.config/learnocaml/client.json` so that you do not have to enter it
anymore. If you want to change this configuration, remove this file and
the client will recreate it the next time it is launched.

#### Practicing your OCaml skills with the client

`learn-ocaml-client` allows you to submit the content of a file as an
answer to an exercise of the Learn-OCaml instance. This answer will be
graded and saved onto the server, exactly as in the web application.

The first thing to do is to get the identifier of the exercise you want
to work on. This identifier is written on the webpage of the server.

The, let us assume that the exercise identifier is `exercise1` and
that you have written an answer in the file named `myanswer.ml`, you
can submit your answer from the command line by:

```
learn-ocaml-client --id=exercise1 myanswer.ml
```

As a response, you will obtain a grading report of the following form:

```

```




