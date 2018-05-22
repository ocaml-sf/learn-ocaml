FROM ocaml/opam

RUN cd /home/opam/opam-repository && git pull

RUN opam config exec -- opam update

WORKDIR learn-ocaml

COPY src/opam src/opam
COPY scripts/install-opam-deps.sh scripts/install-opam-deps.sh 
COPY Makefile Makefile

RUN opam config exec -- make build-deps

COPY . .

RUN sudo chown -R opam:nogroup .

RUN opam config exec -- make REPO_DIR=exercises_repository

EXPOSE 9090

CMD ["/home/opam/learn-ocaml/learnocaml-simple-server.byte", "-port", "9090" ]
