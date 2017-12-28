FROM alpine
RUN apk update
RUN apk add git ocaml opam 
RUN git clone https://github.com/MLstate/opalang /opalang
RUN apk add abuild bash binutils binutils-doc build-base gcc gcc-doc git m4 \
    make which zlib zlib-dev
RUN opam init
RUN opam install ulex camlzip ocamlgraph ocamlfind ocamlbuild
WORKDIR /opalang
RUN ls -la ~/.opam/
# RUN eval `opam config env` && ./configure
# RUN make
# RUN make install
# RUN mkdir /app
# COPY . /app
