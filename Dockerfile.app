FROM learn-ocaml
LABEL Description="learn-ocaml app server instance" Vendor="OCamlPro" Version="0.2"

USER root
ADD tutorials repository/tutorials
ADD exercises repository/exercises
ADD lessons repository/lessons
RUN chown -R learn-ocaml:learn-ocaml repository

USER learn-ocaml
RUN learn-ocaml build --repo=repository
RUN rm -rf repository

VOLUME ["/sync"]
EXPOSE 8080

USER learn-ocaml
ENTRYPOINT ["dumb-init","learn-ocaml","serve","--sync-dir=/sync","--port=8080"]
