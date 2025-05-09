
SUBDIRSX= asl basics calc camltk colwheel compress demonstr docteur doctor grep \
  hanoi kb minicaml minilogo pascal picomach showsort soli spirals

# TODO: basics
SUBDIRS=soli

#TODO: add depend: target? and recurse? called from toplevel Makefile?

default:
	$(MAKE) all
	$(MAKE) test

all:
	set -e; for i in $(SUBDIRS); do cd $$i; $(MAKE); done
test:
	set -e; for i in $(SUBDIRS); do cd $$i; $(MAKE) test; done
