#include <stdlib.h>
#include <unistd.h>

// should be caml/mlvalues.h, caml/memory.h
// but with intree compilation we use -I ../byterun hence the code below
#include <mlvalues.h>
#include <memory.h>

void caml_test_foreign(value x, value y) {
  CAMLparam2(x, y);

  CAMLreturn0;
}


extern void invalid_argument (char *);

void caml_test_foreign2() {
  invalid_argument("Array.get");
}

