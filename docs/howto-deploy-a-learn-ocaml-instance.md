How to deploy a learn-ocaml instance
====================================

This section explains how to deploy an instance of the learn-ocaml
platform on a server.

## Using pre-built docker images

Assuming your exercise repository is in directory REPOSITORY, you can either:

- build and serve it directly on port 80 (REPOSITORY needs to be an absolute
  path) using

      docker run --rm -v REPOSITORY:/repository:ro -v learn-ocaml-sync:/sync -p 80:8080 --name learn-ocaml-server ocamlsf/learn-ocaml:master

NB: Do not forget to escape `:` if it appears in `REPOSITORY` to avoid a parsing error of the command line arguments.

- or generate a new docker image that includes your repository:

        cd REPOSITORY
        wget https://raw.githubusercontent.com/ocaml-sf/learn-ocaml/master/Dockerfile.app
        docker build -t learn-ocaml-app -f Dockerfile.app .

  and then deploy that image:

      docker run --rm -p 80:8080 -v learn-ocaml-sync:/sync --name learn-ocaml-server learn-ocaml-app

  The user data will persist between runs within the Docker "Volume" called
  `learn-ocaml-sync`. You can find out where it stores its data using:

      docker volume inspect learn-ocaml-sync -f '{{.Mountpoint}}'

  This method can allow you to distribute the Docker image, which doesn't
  contain the exercise solutions in plaintext.

> **A remark about security**
>
> As a reminder, the Docker deamon's socket is owned by default by `root`.
> Hence, these scripts cannot work without `sudo` or adding your current user to
> the group `docker`. For more information, see
> [post installation steps for Linux](https://docs.docker.com/install/linux/linux-postinstall/)
> from the documentation. In any case, be advised there exists security flaws in
> the Docker daemon (see
> [documentation](https://docs.docker.com/engine/security/security/#docker-daemon-attack-surface)).

## Manual compilation

Note: you need a working ```opam``` environment (at least `2.0.0~rc2`).

* Install the dependencies using:
``
opam switch create . --deps-only && opam install opam-installer && eval $(opam env)
``

* Compile the learn-ocaml management tool using:
```
make && make opaminstall
```

* Build the app for your given exercise repository. This will put it into a
  `www/` directory:
  ```
  learn-ocaml build --repo DIR
  ```
  (depending on your opam configuration, you might need to run the command from the `learn-ocaml` directory or to specify the absolute path to the command: `.../learn-ocaml/_opam/bin/learn-ocaml`)

* Run the learn-ocaml web server
```
learn-ocaml serve --port 8080
```

## Enabling passwords

By default, authentication is performed with a token instead of a more
traditionnal email/password pair, but this can now be enabled by
setting the `use_passwd` option to `true` (by default, it is set to
`false`).

## Integration with Moodle and other teaching tools

If you enabled passwords, you can also enable LTI, enabling to login
in Learn-OCaml from Moodle and other teaching tools.

> *Warning*
>
> Passwords must be enabled to use the LTI integration.

The option `use_moodle` must be set to `true` in the config file (by
default, it is set to `false`).  When running `learn-ocaml build`,
Learn-OCaml generate a private key for LTI authentication if there is
none yet, and print it to the standard output.

This key can be then inserted as the secret in the LTI-compatible
application (eg. Moodle).  You can set any value you want as the
consumer key, but take care to not reuse the value between multiple
applications.
