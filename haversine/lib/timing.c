#include "caml/mlvalues.h"
#include <x86intrin.h>

CAMLprim value add(value a, value b) {
  int a_val = Int_val(a);
  int b_val = Int_val(b);
  return Val_int(a_val + b_val);
}

CAMLprim value rdtsc(value a) {
  return Val_int(__rdtsc());;
}
