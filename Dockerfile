# Build and test ocaml-light on Ubuntu Linux.

FROM ubuntu:22.04
#alt: alpine:3.21

# Setup a basic C dev environment
RUN apt-get update # needed otherwise can't find any package
RUN apt-get install -y build-essential autoconf automake
#alt: apk add build-base make bash git rsync curl
#alt: LATER: use kencc instead of gcc

WORKDIR /src

# Now let's build from source
COPY . .

# Configure
RUN ./configure

# Make
RUN make clean
RUN make coldstart
RUN make world

# Install
RUN make install

# Basic tests
RUN which ocaml
RUN ocamlc -v
RUN echo '1+1;;' | ocaml

# Regression tests
RUN make test
