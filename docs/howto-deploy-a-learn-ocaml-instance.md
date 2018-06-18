How to deploy a learn-ocaml instance
====================================

This section explains how to deploy an instance of the learn-ocaml
platform on a server.

## Using pre-built docker images

Assuming your exercise repository is in directory REPOSITORY, you can either:

- build and serve it directly on port 80 (REPOSITORY needs to be an absolute
  path) using

      docker run --rm -v REPOSITORY:/repository:ro -v learn-ocaml-sync:/sync -p 80:8080 --name learn-ocaml-server altgr/learn-ocaml

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

Note: you need a working ```opam``` environment with OCaml ```4.05.0```.

* Install the dependencies using:
  - if you have opam >=2.0 available:
``
opam switch create . --deps-only && opam install opam-installer && eval $(opam env)
``
  - otherwise:
``
make build-deps
``
(You may want to first read the script `scripts/install-opam-deps.sh` to check what it does.)

* Compile the learn-ocaml management tool using:
```
make && make opaminstall
```

* Build the app for your given exercise repository. This will put it into a
  `www/` directory:
```
learn-ocaml build --repo DIR
```

* Then either put the resulting directory ```www/``` behind a Web server.

This step is mandatory. Indeed, if you try to open the ```index.html``` file
directly from the local file system, it will fail for security restrictions
enforced by modern Web browsers. Hence, you need a local web server.

* Use the provided minimal server, that also does the job,
  and includes a minimal server-side synchronization mechanism.

  You can launch it via

      learn-ocaml serve --port 8080

* If you do not have a Web server configured, and don't need synchronisation you can probably use some
  other tool that is already present on your machine. For instance,
  running ```python3 -m http.server 8080``` or ```php -S
  localhost:8080``` in the ```www``` directory and pointing you browser
  to ```http://localhost:8080/``` should do the job.
