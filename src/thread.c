#include <pthread.h>
#include <stdio.h>

void spawn_thread(void* routine(void *p)) {
  pthread_t thread;
  pthread_create(&thread, NULL, routine, NULL);
  pthread_join(thread, NULL);
}

#ifdef BUILD_TEST
int main() { return 0; }
#endif