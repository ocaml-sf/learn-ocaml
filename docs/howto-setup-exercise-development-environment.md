How to setup your development environment
=========================================

This section explains how to setup a development environment on your
local machine. At the end of this tutorial, you will be able to follow
the tutorial `howto-write-an-exercise.md`. For the moment, only
GNU/Linux and MacOS X are supported.

> An alternative to following the instructions below is to use a pre-built
> Docker container. Assuming you have an exercise repository in directory
> `REPOSITORY` (absolute path), and a recent enough version of Docker installed,
> use:
>
>     docker run --rm -v REPOSITORY:/repository:ro -v learn-ocaml-sync:/sync -p 80:8080 --name learn-ocaml-server altgr/learn-ocaml
>
> This will start an instance of the learn-ocaml server on port 80 (ignore the
> message about 8080, this is the port used internally).
> An example repository can be obtained in the `demo-repository` directory of
> [learn-ocaml](https://github.com/ocaml-sf/learn-ocaml/archive/master.zip).

## Software requirements

Please make sure that the following tools are available on your machine:
- git    (>= 2.00)
- make   (>= 4)
- opam   (>= 2.00)
- ocaml  (>= 4.05.0)

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

Get an opam environment (a.k.a "switch") with the learn-ocaml dependencies
ready:
```
opam switch create . --deps-only --locked
eval $(opam env)
```
(Alternatively, use `opam install . --deps-only` to install the dependencies in
your current opam switch, without creating a dedicated one)

Then, compile and install the platform:

If you do not have a GitHub account, do instead:
```
git clone https://github.com/ocaml-sf/learn-ocaml.git && cd learn-ocaml
```

Get an opam environment (a.k.a "switch") with the learn-ocaml dependencies
ready:
```
opam switch create . --deps-only --locked
eval $(opam env)
```
(Alternatively, use `opam install . --deps-only` to install the dependencies in
your current opam switch, without creating a dedicated one)

Then, compile and install the platform:
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
learn-ocaml build --repo my-learn-ocaml-repository
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
