#include <stdio.h>
#include <inttypes.h>

#include "printer.h"

void printValueHelper(int64_t x) {
  if ((x & 0x0000000000000001) == 0) {
    // Then this is an integer.
    x /= 2;
    printf("%"PRId64, x);
  } else if ((x & 0x0000000000000003) == 3) {
    // Then this is a boolean.
    if (x == 0x7FFFFFFFFFFFFFFF) {
      printf("false");
    } else if (x == 0xFFFFFFFFFFFFFFFF) {
      printf("true");
    } else {
      // This should never happen!  Print the hex of the value so we can debug.
      printf("UNINTERPRETABLE VALUE: %16"PRIx64, x);
    }
  } else {
    // Then this is a heap pointer.
    x &= 0xFFFFFFFFFFFFFFFC;
    int64_t* p = (int64_t*)x;
    int64_t size = *p;
    if ((size & 0x8000000000000000) == 0) {
      // Then it points to a tuple.
      printf("(");
      for (int64_t i=0;i<size;i++) {
        if (i!=0) {
          printf(", ");
        }
        printValueHelper(p[i+2]);
      }
      printf(")");
    } else {
      // Then it points to a closure
      size ^= 0x8000000000000000;
      int64_t max = *(p+2);
      int64_t addr = *(p+3);
      printf("<closure@%016"PRIx64">[%"PRId64"/%"PRId64"]", addr, size, max);
      printf("(");
      for (int64_t i=0;i<max;i++) {
        if (i!=0) {
          printf(", ");
        }
        if (i < size) {
          printValueHelper(p[i+4]);
        } else {
          printf("?");
        }
      }
      printf(")");
    }
  }
}

void printValue(int64_t x) {
  printValueHelper(x);
  printf("\n");
}
