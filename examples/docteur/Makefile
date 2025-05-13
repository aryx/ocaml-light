TOP=../..
include ../Makefile.config
include ../Makefile.common

all: docteur.byt

test:
	echo "hello\ngoodbye"| ./doctor.byt | cmp - doctor.out
