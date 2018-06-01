How to deploy a learn-ocaml instance
====================================

This section explains how to deploy an instance of the learn-ocaml
platform on a server.

## Software requirements

Please make sure that the following tools are available on your machine:
- git    (>= 2.00)
- docker (>= 18)

## Cloning the git repository

Clone the git repository:

```
git clone git@github.com:ocaml-sf/learn-ocaml.git
```

## Building the docker image

Assuming that ``$EXERCISE_DIRECTORY`` contains the exercises
definitions, the following command generates a docker image for this
specific set of exercises:

```
bash scripts/build-docker-image.sh -repo-dir $EXERCISE_DIRECTORY
```

The execution of this command generates an image named
```learnocaml-docker```. This is the default identifier, but it can be
renamed using the option ``-image-name <my_image>``.

## Initialize and launch a fresh instance

A container is initialized from the image by doing:

```
bash scripts/docker-server.sh init
```

This command creates a fresh container and runs it.

## Control the instance

An initialized instance can be stopped using:

```
bash scripts/docker-server.sh stop
```

A stopped instance can be started using:

```
bash scripts/docker-server.sh start
```

A running instance can be restarted using:

```
bash scripts/docker-server.sh restart
```

An instance can be removed using:

```
bash scripts/docker-server.sh remove archive.tar
```

Be aware that the `remove` command destroys the container from the
docker system. As such, it will remove every saved sessions from the
`sync` directory, __i.e.__ every saved users' code. For this reason,
this command also needs a filename argument that will contain a backup
of these files using the `tar` format.

## Remark about security

As a reminder, the Docker deamon's socket is owned by default by
`root`. Hence, these scripts cannot work without `sudo` or adding your
current user to the group `docker`. For more information, see [post
installation steps for
Linux](https://docs.docker.com/install/linux/linux-postinstall/) from
the documentation. In any case, be advised there exists security flaws
in the Docker daemon (see
[documentation](https://docs.docker.com/engine/security/security/#docker-daemon-attack-surface)).

