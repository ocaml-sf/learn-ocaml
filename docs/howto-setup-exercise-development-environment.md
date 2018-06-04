How to setup your development environment
=========================================

This section explains how to setup a development environment on your
local machine. At the end of this tutorial, you will be able to follow
the tutorial `howto-write-an-exercise.md`. For the moment, only
GNU/Linux and MacOS X are supported.

## Software requirements

Please make sure that the following tools are available on your machine:
- git    (>= 2.00)
- make   (>= 4)
- opam   (>= 2.00)

To install opam, please follow the instructions described on the
[Official OPAM website](https://opam.ocaml.org/doc/Install.html).

The other dependencies are installed automatically by opam.

## Step 0: What we are about to do

We will install in directory ``$DIR`` a fresh copy of learn-ocaml
source tree as well as a source directory into which you will develop
your exercises.

## Step 1: Install the learn-ocaml platform locally

We assume that the current directory is `$DIR`.

First, clone the current learn-ocaml source tree:
```
git clone git@github.com:ocaml-sf/learn-ocaml.git && cd learn-ocaml
```

Second, compile and install the platform:
```
make && make opaminstall
```

At this point, you should get a working `learn-ocaml` program in
your path. Try:
```
learn-ocaml --help
```
This should open the manpage of the command-line tool to interact
with the platform.

## Step 2: Setup a work directory

Now, let us go back to `$DIR` and create a root for the source tree of exercises:
```
cd $DIR && cp -fr learn-ocaml/demo-repository my-learn-ocaml-repository
```

## Step 3: Sanity check

Check that your installation works:
```
cd $DIR/my-learn-ocaml-repository
learn-ocaml build
learn-ocaml serve
```

This should output several lines in your terminal ending with:
```
Starting server on port 8080
```

At this point, browsing `http://localhost:8080` should get you to the
homepage of the learn-ocaml instance that is running on your local
machine.

You are now ready to write your first exercise! Please proceed to
the next tutorial [How to write exercises?](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/howto-write-exercises.md)
