#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

unsigned long long ntasks = 0;

typedef struct task_t {
    void (*fn)(void *);
    void *args;
    int priority;
} task_t;

task_t **tasks;

int
omp_get_thread_num()
{
    return 0;
}

void
omp_set_priority_(int titi)
{
}

void
GOMP_task (void (*fn) (void *), void *data, void (*cpyfn) (void *, void *),
	   long arg_size, long arg_align, bool if_clause, unsigned flags,
	   void **depend, int priority)
{
    /*printf("This is a task\n");*/
    char *dynargs = malloc(arg_size + arg_align - 1);
    task_t *task = malloc(sizeof(task_t));
    if (__builtin_expect (cpyfn != NULL, 0))
    {
        /*char buf[arg_size + arg_align - 1];*/
        char *arg = (char *) (((uintptr_t) dynargs + arg_align - 1)
                & ~(uintptr_t) (arg_align - 1));
        /*printf("caca, %lu\n", arg_size);*/
        cpyfn (arg, data);
        task->args = arg;
        task->fn = fn;
        task->priority = priority;
        /*fn(arg);*/
    }
    else {
        /*printf("coucou, %lu\n", arg_size);*/
        memcpy(dynargs, data, arg_size);
        task->args = dynargs;
        task->fn = fn;
        task->priority = priority;
		/*fn(data);*/
    }
    tasks[ntasks] = task;
    ntasks++;
}


void
GOMP_parallel (void (*fn) (void *), void *data, unsigned num_threads, unsigned int flags)
{
    printf("This a region\n");
    tasks = malloc(4000000 * sizeof(task_t *));
    fn(data);
    printf("End of a region, %llu tasks\n", ntasks);
    printf("Rexecuting stuff:\n");
    for (unsigned int i = 0; i < ntasks; i++)
        tasks[i]->fn(tasks[i]->args);
}

