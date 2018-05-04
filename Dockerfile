FROM ocaml/opam

WORKDIR learn-ocaml

ADD . .

RUN sudo chown -R opam:nogroup .

RUN cd /home/opam/opam-repository && git pull

RUN opam config exec -- opam update

RUN opam config exec -- make build-deps

RUN opam config exec -- make REPO_DIR=exercises_repository

EXPOSE 9090

CMD /home/opam/learn-ocaml/learnocaml-simple-server.byte -port 9090
