###############################################################################
# Overview
###############################################################################
# Build and test ocaml-light (bytecode and x86/arm native) on Ubuntu.
# See https://docs.docker.com/build/building/multi-stage/ for more info on the
# multi-stage approach.

###############################################################################
# Stage1: build just the bytecode part
###############################################################################

FROM ubuntu:22.04 AS build
#alt: ubuntu:24.04, alpine

# Setup a basic C dev environment to *build* ocaml-light
RUN apt-get update # needed otherwise can't find any package
# alt: build-essential
RUN apt-get install -y --no-install-recommends binutils gcc libc6-dev make
# This is for graphics.cma
RUN apt-get install -y --no-install-recommends libx11-dev

WORKDIR /src

# Now let's build from source
COPY . .

# configure
RUN ./configure

# make
RUN make clean
RUN make coldstart

RUN make

# make install
RUN make install

###############################################################################
# Stage2: bytecode image
###############################################################################

FROM ubuntu:22.04 AS bytecode

# We also need a basic C dev environment to *use/run* ocaml-light
# as ocamlc will occasionnally call gcc and link with libc (e.g.,
# when using -custom and relying on C libs)
RUN apt-get update
RUN apt-get install -y --no-install-recommends binutils gcc libc6-dev make
RUN apt-get install -y --no-install-recommends libx11-dev

COPY --from=build /usr/local /usr/local

WORKDIR /tmp

# basic tests
RUN which ocaml
RUN ocamlc -v
RUN echo '1+1;;' | ocaml
RUN echo 'let _ = print_string "hello"' > foo.ml
RUN ocamlc -cclib -lunix -custom foo.ml
RUN ./a.out

###############################################################################
# Stage3: build also the native part
###############################################################################

FROM build AS build-native-x86_64

# multilib is needed for gcc -m32; asmcomp currently supports only x86
#alt: LATER: port amd64 and even later use goken instead of gcc!
RUN apt-get install -y gcc-multilib
WORKDIR /src
# this requires gcc-multilib
RUN make opt

RUN make installopt
# make test (it requires make install first)
RUN make test
# good self test
RUN make ocamlc.opt
RUN make ocamlopt.opt

RUN echo 'let _ = print_string "hello"' > foo.ml
RUN ocamlopt -cclib -lunix foo.ml
RUN ./a.out

###############################################################################
# Stage4: native image
###############################################################################

FROM bytecode AS native-x86_64

COPY --from=build-native-x86_64 /usr/local /usr/local

# basic tests
RUN which ocaml
RUN ocamlc -v
RUN echo '1+1;;' | ocaml
# more basic tests
RUN which ocamlopt
RUN ocamlopt -v
