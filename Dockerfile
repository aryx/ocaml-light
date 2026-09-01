###############################################################################
# Overview
###############################################################################
# Build and test ocaml-light (bytecode and x86/arm/mips/alpha native) on Ubuntu.
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

# make install (classic triptic)
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
RUN echo '1+2;;' | ocaml
RUN echo 'let _ = print_string "hello bytecode"' > foo.ml
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
# claude: configure no longer defaults a 64-bit host's native-code target
# to a 32-bit cross target (so amd64 can later get its own native
# backend without this silently meaning "cross to i386" instead) -- ask
# for it explicitly.
RUN ./configure -target-arch i386
# this requires gcc-multilib
RUN make opt

RUN make installopt
# make test (it requires make install first)
RUN make test
# good self test
RUN make ocamlc.opt
RUN make ocamlopt.opt

#TODO: note that -cclib -lunix does not work, maybe because i386 vs x86_64?
RUN echo 'let _ = print_string "hello native x86"' > foo.ml
RUN ocamlopt foo.ml
RUN ./a.out

FROM build AS build-native-aarch64

# use arm32 backend for now and the armhf gcc/binutils cross compiler
RUN dpkg --add-architecture armhf
RUN apt-get update
RUN apt-get install -y gcc-arm-linux-gnueabihf libc6:armhf
WORKDIR /src
# claude: same as build-native-x86_64 above -- configure no longer
# defaults a 64-bit host's native-code target to a 32-bit cross target,
# so ask for it explicitly.
RUN ./configure -target-arch arm
RUN make opt

RUN make installopt
RUN make test
# good self test
RUN make ocamlc.opt
RUN make ocamlopt.opt
RUN echo 'let _ = print_string "hello native arm"' > foo.ml
#RUN ocamlopt -cclib -lunix foo.ml
RUN ocamlopt foo.ml
RUN ./a.out

FROM build AS build-native-mips

# claude: unlike i386 (gcc-multilib on a matching host) and arm (native
# execution via aarch64's AArch32 compat mode), there is no CPU-level
# compat mode from x86_64/aarch64 down to mips -- every mips binary here
# needs qemu-user, regardless of which host architecture builds this
# image. qemu-user-static is invoked explicitly below rather than relying
# on the host's binfmt_misc registration, so this stage is self-contained
# on any Docker host.
# claude: no --no-install-recommends here (unlike the base image setup
# above) -- gcc-mipsel-linux-gnu only Recommends (not Depends on)
# libc6-dev-mipsel-cross, which provides the mipsel target headers
# (bits/libc-header-start.h etc.) needed to compile asmrun/*.c.
RUN apt-get install -y gcc-mipsel-linux-gnu qemu-user-static
WORKDIR /src
RUN ./configure -target-arch mips
RUN make opt

RUN make installopt
RUN make test
# good self test
RUN make ocamlc.opt
RUN make ocamlopt.opt

RUN echo 'let _ = print_string "hello native mips"' > foo.ml
RUN ocamlopt foo.ml
# claude: configure's -target-arch mips links with -static (see the
# nativecclinkopts comment in configure), so a.out needs no mipsel
# shared libraries on this filesystem -- just the qemu-user-static
# interpreter installed above. Uses test/run-native (see there) rather
# than invoking qemu-mipsel-static directly, for the same reason
# `make test` above does: no reliance on qemu-binfmt/binfmt_misc
# registration.
RUN test/run-native ./a.out

# claude: unlike i386/arm/mips above, this stage is NOT "FROM build":
# ubuntu:22.04 (the base of every other stage here) doesn't package
# gcc-alpha-linux-gnu at all ("Unable to locate package") -- it only
# showed up in Ubuntu's archives around 24.04. Rather than bumping every
# other stage's base image (wider blast radius than this port needs),
# just this one stage runs on ubuntu:24.04, reusing the already-built
# bytecode tree from the (22.04) `build` stage via COPY -- ocamlrun/
# ocamlc are plain ELF binaries, and a newer glibc runs older binaries
# fine, so nothing needs rebuilding, only the native (alpha) half.
FROM ubuntu:24.04 AS build-native-alpha

RUN apt-get update
RUN apt-get install -y --no-install-recommends binutils gcc libc6-dev make
RUN apt-get install -y --no-install-recommends libx11-dev

WORKDIR /src
COPY --from=build /src /src
COPY --from=build /usr/local /usr/local

# claude: same reasoning as build-native-mips above -- alpha has no
# CPU-level compat mode on x86_64/aarch64, so every alpha binary here
# needs qemu-user regardless of the Docker host's own architecture.
# claude: no --no-install-recommends here either -- gcc-alpha-linux-gnu
# only Recommends (not Depends on) libc6.1-dev-alpha-cross, which
# provides the alpha target headers needed to compile asmrun/*.c.
RUN apt-get install -y gcc-alpha-linux-gnu qemu-user-static
RUN ./configure -target-arch alpha
RUN make opt

RUN make installopt
RUN make test
# good self test
RUN make ocamlc.opt
RUN make ocamlopt.opt

RUN echo 'let _ = print_string "hello native alpha"' > foo.ml
RUN ocamlopt foo.ml
# claude: configure's -target-arch alpha links with -static -Wl,--no-relax
# (see the nativecclinkopts comment in configure -- the --no-relax part
# is not optional, see there for why), so a.out needs no alpha shared
# libraries on this filesystem -- just the qemu-user-static interpreter
# installed above. Uses test/run-native for the same reason
# build-native-mips does: no reliance on qemu-binfmt/binfmt_misc
# registration.
RUN test/run-native ./a.out

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

FROM bytecode AS native-aarch64
COPY --from=build-native-aarch64 /usr/local /usr/local
# basic tests
RUN which ocaml
RUN ocamlc -v
RUN echo '1+1;;' | ocaml
# more basic tests
RUN which ocamlopt
RUN ocamlopt -v

# claude: like native-x86_64/native-aarch64 above, this final image does
# not carry the full cross toolchain (mipsel-linux-gnu-gcc/as, only
# present in build-native-mips) needed to actually compile a new program
# -- ocamlopt itself is bytecode and runs fine here, but calling it to
# compile+link would fail on a missing assembler, same as those other
# two stages would on a missing multilib/cross gcc. The real mips
# regression test (compile, link, and run under qemu-user-static) lives
# in build-native-mips above; this stage only smoke-tests the installed
# tools, matching native-x86_64/native-aarch64's scope.
FROM bytecode AS native-mips
COPY --from=build-native-mips /usr/local /usr/local
# basic tests
RUN which ocaml
RUN ocamlc -v
RUN echo '1+1;;' | ocaml
# more basic tests
RUN which ocamlopt
RUN ocamlopt -v

# claude: same scope note as native-mips above -- no alpha-linux-gnu-gcc/as
# here, just a smoke test of the installed (bytecode) tools. The real
# alpha regression test lives in build-native-alpha above.
FROM bytecode AS native-alpha
COPY --from=build-native-alpha /usr/local /usr/local
# basic tests
RUN which ocaml
RUN ocamlc -v
RUN echo '1+1;;' | ocaml
# more basic tests
RUN which ocamlopt
RUN ocamlopt -v
