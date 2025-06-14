TOP=..
include $(TOP)/config/Makefile

CODEGEN=../cmm/codegen
CC=$(NATIVECC)
CFLAGS=$(NATIVECCCOMPOPTS) -g -Wno-implicit-function-declaration -Wno-builtin-declaration-mismatch -Wno-parentheses

PROGS=fib.out tak.out quicksort.out quicksort2.out soli.out integr.out \
      arith.out checkbound.out

all: $(PROGS)

# The test programs

fib.out: main.c fib.o $(ARCH).o
	$(CC) $(CFLAGS) -o fib.out -DINT_INT -DFUN=fib main.c fib.o $(ARCH).o

tak.out: main.c tak.o $(ARCH).o
	$(CC) $(CFLAGS) -o tak.out -DUNIT_INT -DFUN=takmain main.c tak.o $(ARCH).o

quicksort.out: main.c quicksort.o $(ARCH).o
	$(CC) $(CFLAGS) -o quicksort.out -DSORT -DFUN=quicksort main.c quicksort.o $(ARCH).o

quicksort2.out: main.c quicksort2.o $(ARCH).o
	$(CC) $(CFLAGS) -o quicksort2.out -DSORT -DFUN=quicksort main.c quicksort2.o $(ARCH).o

soli.out: main.c soli.o $(ARCH).o
	$(CC) $(CFLAGS) -o soli.out -DUNIT_INT -DFUN=solitaire main.c soli.o $(ARCH).o

integr.out: main.c integr.o $(ARCH).o
	$(CC) $(CFLAGS) -o integr.out -DINT_FLOAT -DFUN=test main.c integr.o $(ARCH).o

tagged-fib.out: main.c tagged-fib.o $(ARCH).o
	$(CC) $(CFLAGS) -o tagged-fib.out -DINT_INT -DFUN=fib main.c tagged-fib.o $(ARCH).o

tagged-tak.out: main.c tagged-tak.o $(ARCH).o
	$(CC) $(CFLAGS) -o tagged-tak.out -DUNIT_INT -DFUN=takmain main.c tagged-tak.o $(ARCH).o

tagged-quicksort.out: main.c tagged-quicksort.o $(ARCH).o
	$(CC) $(CFLAGS) -o tagged-quicksort.out -DSORT -DFUN=quicksort main.c tagged-quicksort.o $(ARCH).o

tagged-integr.out: main.c tagged-integr.o $(ARCH).o
	$(CC) $(CFLAGS) -o tagged-integr.out -DINT_FLOAT -DFUN=test main.c tagged-integr.o $(ARCH).o

arith.out: mainarith.c arith.o $(ARCH).o
	$(CC) $(CFLAGS) -o arith.out mainarith.c arith.o $(ARCH).o

checkbound.out: main.c checkbound.o $(ARCH).o
	$(CC) $(CFLAGS) -o checkbound.out -DCHECKBOUND main.c checkbound.o $(ARCH).o

# The runtime environment

power.o: power-$(SYSTEM).o
	cp power-$(SYSTEM).o power.o

.SUFFIXES:
.SUFFIXES: .cmm .c .o .S

.cmm.o:
	$(CAMLRUN) $(CODEGEN) $*.cmm > $*.s
	$(AS) $(ASFLAGS) -o $*.o $*.s

.S.o:
	$(ASPP) $(ASPPFLAGS) -o $*.o $*.S

clean::
	rm -f *.out *.s *.o *~
