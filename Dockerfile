# Build and test ocaml-light (bytecode and x86 native) on Ubuntu.

FROM ubuntu:22.04
#alt: 24.04

# Setup a basic C dev environment
RUN apt-get update # needed otherwise can't find any package
RUN apt-get install -y build-essential autoconf automake
# multilib is needed for gcc -m32; asmcomp currently supports only x86
#alt: LATER: use kencc or even better goken! instead of gcc
RUN apt-get install -y gcc-multilib

# The RUN below is for graphics.cma but unfortunately libx11-static
# does not exist on Ubuntu (it exists on Alpine though) and ocaml-light
# can only link statically libs so for now this docker image will not 
# have graphics.cma (or maybe libx11-dev should be enough but ocaml-light
# configure failed to enable x11?)
#TODO: RUN apt-get install -y libx11-dev libx11-static

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
