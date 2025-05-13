TOP=../..
include ../Makefile.config
include ../Makefile.common

all: doctor.byt

test:
	echo "hello\ngoodbye"| ./doctor.byt | cmp - doctor.out
