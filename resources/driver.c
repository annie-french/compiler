#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include "printer.h"
#include <stdlib.h>

int64_t bird_main(int64_t*, int64_t*) asm("bird_main");

int main(int argc, char** argv) {
  int64_t* mem;
  int64_t* end_of_mem;
  mem = (int64_t*) malloc(sizeof(int64_t)*1000000); //100000
  end_of_mem = mem + 1000000;
  int64_t result = bird_main(mem, end_of_mem);
  printValue(result);
  return 0;
}
