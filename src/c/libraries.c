#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

void assert_func(int run_time_assertion, char* message) {
  if (run_time_assertion) return;
  printf("%s\n", message);
  exit(1);
}

void mutex_init(void **ptr) {
  pthread_mutex_t *mutex = malloc(sizeof(pthread_mutex_t));
  pthread_mutex_init(mutex, NULL);
  *ptr = mutex;
}

void mutex_lock(void *ptr) {
  pthread_mutex_t *mutex = ptr;
  pthread_mutex_lock(mutex);
}

void mutex_unlock(void *ptr) {
  pthread_mutex_t *mutex = ptr;
  pthread_mutex_unlock(mutex);
}
