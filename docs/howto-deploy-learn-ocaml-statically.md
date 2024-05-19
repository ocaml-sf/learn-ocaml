How to deploy learn-ocaml statically
====================================

This section explains how to deploy a static version of learn-ocaml on
an HTTP server.

## Compilation using pre-built docker images

You will just need to:

- install Docker Engine (<https://docs.docker.com/get-docker/>)
- build the `www` folder by using the commands below
- use an HTTP server to serve learn-ocaml statically.

Then use either of the following approaches:

### Manual docker commands

Assuming your exercise repository is in directory `$REPOSITORY` and
you want to generate the website contents in directory `$TARGET/www`
to serve it at `$URL` (the base URL **without trailing slash**), then
run the following commands:

```bash
# Remove old version
cd "$TARGET"
rm -fr www
# Get the last (dev) version of learn-ocaml
sudo docker pull ocamlsf/learn-ocaml:master
# Build the site within a Docker container
sudo docker run --rm -i --entrypoint="" \
  -v "$REPOSITORY:/repository" -v "$TARGET:/home/learn-ocaml/target" \
  ocamlsf/learn-ocaml:master \
  sh -c "learn-ocaml build --repo=/repository --base-url $URL && mv www target/"
```

Regarding the `--base-url` option, if you plan to deploy the `www`
directory with **GitHub Pages**, assuming the underlying GitHub repo
(either public or private) is `https://github.com/user-name/repo-name`
then you should first run:

```bash
export URL=https://user-name.github.io/repo-name
```

### An automatic deploy script

Just use the following script, committed within a GitHub Pages repo:

- <https://github.com/erikmd/learn-ocaml-meetup-demo/blob/master/deploy>

For a comprehensive example of one such deployment, you may take a
look at the following repos:

- <https://github.com/erikmd/learn-ocaml-meetup-demo-repository> (with extensive documentation in the [README.md](https://github.com/erikmd/learn-ocaml-meetup-demo-repository#readme))
- <https://github.com/erikmd/learn-ocaml-meetup-demo> (separated repo containing a .gitignore and the `deploy` script)
- <https://erikmd.github.io/learn-ocaml-meetup-demo/> (URL of the deployment)

## Manual compilation

Note: you need a working `opam` environment (version 2.0+) as well as
an `opam switch` and a compiled version of `learn-ocaml` (see
[How to deploy a learn-ocaml instance](howto-deploy-a-learn-ocaml-instance.md#manual-compilation)
for details).

Assuming your exercise repository is in directory `$REPOSITORY` and
you want to generate the website contents in directory `$TARGET/www`
to serve it at `$URL` (the base URL **without trailing slash**), then
you can run:

```bash
rm -fr "$TARGET/www"
cd .../learn-ocaml  # go to the learn-ocaml git repo
learn-ocaml build --repo=$REPOSITORY --base-url $URL
mv www "$TARGET/www"
```
