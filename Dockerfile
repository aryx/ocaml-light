# Build and test OCaml-light on Ubuntu Linux.

FROM ubuntu:22.04
#alt: alpine:3.21

# Setup a basic C dev environment
RUN apt-get update # needed otherwise can't find any package
RUN apt-get install -y build-essential autoconf automake
#alt: apk add build-base make bash git rsync curl
#alt: LATER: use kencc and compile our own ocaml-light

WORKDIR /src

# Now let's build from source
COPY . .

RUN ./configure
RUN make clean
RUN make coldstart
RUN make world

