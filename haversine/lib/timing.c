#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <sys/time.h>
#include <x86intrin.h>

CAMLprim value add(value a, value b) {
  CAMLparam2(a,b);
  int a_val = Int_val(a);
  int b_val = Int_val(b);
  CAMLreturn (Val_int(a_val + b_val));
}

CAMLprim long rdtsc(value a) {
  return Val_long(__rdtsc());
}

CAMLprim value my_gettimeofday(value a) {
  CAMLparam1 (a);
  struct timeval v;
  gettimeofday(&v, NULL);
  CAMLlocal1 (result);
  result = caml_alloc_tuple(2);
  Store_field (result, 0, Val_int(v.tv_sec));
  Store_field (result, 1, Val_int(v.tv_usec));
  CAMLreturn (result);;
}
