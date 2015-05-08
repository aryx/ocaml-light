#include <stdlib.h>
#include <unistd.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>

void caml_test_foreign(value x, value y) {
  CAMLparam2(x, y);

  CAMLreturn0;
}


extern void invalid_argument (char *);

void caml_test_foreign2() {
  invalid_argument("Array.get");
}

