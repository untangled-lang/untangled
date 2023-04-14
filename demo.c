#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

typedef struct Data_t Data_t;
typedef struct Queue_t Queue_t;
typedef struct Routine_t Routine_t;

/*
 * Opcode to identify the variable type
 */
typedef enum Type_t {
  number,
  boolean,
  decimal,
  tuple
} Type_t;

/*
 * Similar representation to cons cell back in 105.
 * If type == tuple, then head and tail point to fst and snd in the tuple. Otherwise,
 * first field holds the data and second field is NULL.
 */
struct Data_t {
  Type_t type;
  Data_t *head;
  Data_t *tail;
};

/*
 * Queue to hold message pool
 */
struct Queue_t {
  int size;
  int capacity;
  Data_t **data;
};

/*
 * This is passed as thread's argument
 */
struct Routine_t {
  /*
   * Before writing to the queue, we need to lock it
   */
  pthread_mutex_t parent_pool_mutex, child_pool_mutex;
  /*
   * parent << expr pushes to parent pool
   * child << expr pushes to child pool
   */
  Queue_t *parent_pool, *child_pool;
};

Queue_t *Queue_init() {
  Queue_t *queue = malloc(sizeof(Queue_t));
  Data_t *data = calloc(2, sizeof(Data_t));
  queue->size = 0;
  queue->capacity = 2;
  queue->data = data;
  return queue;
}

void *Queue_push(Queue_t *queue, Data_t *data) {
  if (queue->size == queue->capacity) {
    queue->data = realloc(queue->data, sizeof(struct Data_t) * queue->capacity * 2);
    queue->capacity *= 2;
  }
  queue->data[queue->size] = data;
  queue->size += 1;
  return queue;
}

bool Queue_empty(Queue_t *queue) {
  return queue->size == 0;
}

Data_t *Queue_pop(Queue_t *queue) {
  Data_t *data = queue->data[0];
  for (int i = 1; i < queue->size; i++) {
    queue->data[i - 1] = queue->data[i];
  }
  queue->size -= 1;
  return data;
}

void *routine(void *arg) {
  Routine_t *argument = arg;
  Data_t *data = malloc(sizeof(Data_t));
  data->type = number;
  data->head = (Data_t *) 10;
  pthread_mutex_lock(&argument->parent_pool_mutex);
  Queue_push(argument->parent_pool, data);
  pthread_mutex_unlock(&argument->parent_pool_mutex);
  return NULL;
}

int main() {
  pthread_t thread;
  pthread_mutex_t parent_pool_mutex, child_pool_mutex;
  pthread_mutex_init(&parent_pool_mutex, NULL);
  pthread_mutex_init(&child_pool_mutex, NULL);

  /*
   * Initializing arguments
   */
  Routine_t *argument = malloc(sizeof(Routine_t));
  argument->parent_pool_mutex = parent_pool_mutex;
  argument->child_pool_mutex = child_pool_mutex;
  argument->parent_pool = Queue_init();
  argument->child_pool = Queue_init();

  /*
   * Run the thread
   */
  pthread_create(&thread, NULL, routine, argument);

  /*
   * Blocking happens when a thread wants to receive a message
   */
  while (Queue_empty(argument->parent_pool)) {}

  /*
   * Lock the queue when a message is received
   */
  pthread_mutex_lock(&parent_pool_mutex);
  Data_t *data = Queue_pop(argument->parent_pool);
  pthread_mutex_unlock(&parent_pool_mutex);

  /*
   * How pattern match is done
   */
  switch (data->type) {
    /*
     * If it is a number, we just need to cast head to number
     */
    case number: {
      int value = (int) data->head;
      printf("Message received %d\n", value);
    }
    default: {
      /* Same logic apply for other types */
      break;
    }
  }

  pthread_join(thread, NULL);
  return 0;
}
