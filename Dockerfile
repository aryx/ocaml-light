# Build and test ocaml-light (bytecode and x86 native) on Ubuntu Linux.

FROM ubuntu:22.04

# Setup a basic C dev environment
RUN apt-get update # needed otherwise can't find any package
RUN apt-get install -y build-essential autoconf automake
# multilib is needed for gcc -m32; asmcomp currently supports only x86
RUN apt-get install -y gcc-multilib
#alt: LATER: use kencc instead of gcc

WORKDIR /src

# Now let's build from source
COPY . .

# configure
RUN ./configure

# make
RUN make clean
RUN make coldstart
RUN make world
# this requires gcc-multilib
RUN make opt

# make install
RUN make install
RUN make installopt

# make test
RUN make test

# good self test
RUN make ocamlc.opt
RUN make ocamlopt.opt

# basic tests
RUN which ocaml
RUN which ocamlopt
RUN ocamlc -v
RUN ocamlopt -v
RUN echo '1+1;;' | ocaml
