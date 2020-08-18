FROM ocaml/opam2:alpine as raml_deps

RUN opam switch 4.06

#Need this dependency for conf-m4 (ocaml module)
RUN sudo apk add m4


RUN sudo apk add linux-headers



RUN opam install ocamlfind 
RUN eval $(opam env)
# Installing ocamlfind seems to mess up PATH
ENV PATH $PATH:/home/opam/.opam/4.06/bin





RUN opam install core ppx_deriving cohttp lwt cohttp-lwt-unix ezjsonm.0.6.0 ocplib-json-typed







#COPY the LP solver files
COPY . .



#Installing the LP solver
WORKDIR clp

RUN sudo tar -xvzf coin-Clp.tar.gz && sudo rm coin-Clp.tar.gz

WORKDIR ./coin-Clp 


#Need a fortran compiler for coin-Clp
RUN sudo apk add gfortran

RUN sudo ./configure -C
RUN sudo make
RUN sudo make install


WORKDIR ../..

RUN sudo ./configure --with-coin-clp clp/coin-Clp

ENV LD_LIBRARY_PATH /clp/coin-Clp/lib

#RUN eval $(opam env)

# Installing ocamlfind seems to mess up PATH
ENV PATH $PATH:/home/opam/.opam/4.06/bin



ENV LD_LIBRARY_PATH  /home/opam/opam-repository/clp/coin-Clp/lib


RUN sudo env "PATH=$PATH" "LD_LIBRARY_PATH=$LD_LIBRARY_PATH" ./configure --with-coin-clp clp/coin-Clp
RUN sudo env "PATH=$PATH" "LD_LIBRARY_PATH=$LD_LIBRARY_PATH" make 


EXPOSE 5000

CMD ["./main"]

