#include <stdio.h>
#include <stdlib.h>

void assert_func(int run_time_assertion, char* message) {
  if (run_time_assertion) return;
  printf("%s\n", message);
  exit(1);
}
