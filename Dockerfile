# Build and test ocaml-light (bytecode and x86 native) on Ubuntu.

FROM ubuntu:22.04
#alt: 24.04

# Setup a basic C dev environment
RUN apt-get update # needed otherwise can't find any package
RUN apt-get install -y build-essential autoconf automake
# multilib is needed for gcc -m32; asmcomp currently supports only x86
#alt: LATER: use kencc or even better goken! instead of gcc
RUN apt-get install -y gcc-multilib

# This is for graphics.cma
RUN apt-get install -y libx11-dev

WORKDIR /src

# Now let's build from source
COPY . .

# configure
RUN ./configure -x11lib /usr/lib/x86_64-linux-gnu/

# make
RUN make clean
RUN make coldstart
RUN make world
# this requires gcc-multilib
RUN make opt

# make install
RUN make install
RUN make installopt

# make test (it requires make install first)
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
