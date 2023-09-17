#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>

#include "error.h"

void stopWithError(int64_t type) {
  switch (type) {
    case 1:
      printf("Expected an int.\n");
      break;
    case 2:
      printf("Expected a boolean.\n");
      break;
    case 3:
      printf("Expected a tuple.\n");
      break;
    case 4:
      printf("Index is invalid.\n");
      break;
    case 5:
      printf("Expected a closure.\n");
      break;
    case 7:
      printf("No more heap memory available\n");
      break;
    default:
      printf("Unknown error %"PRId64" occurred.\n", type);
      break;
  }
  exit(type);
}
