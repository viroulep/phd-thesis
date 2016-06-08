#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

unsigned long long ntasks = 0;

typedef struct task_id_t {
    int t;
    int x;
    int y;
    int z;
} task_id_t;

typedef struct task_t {
    void (*fn)(void *);
    void *args;
    task_id_t id;
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

task_id_t current_id;

void
omp_set_taskid(int t, int x, int y, int z)
{
    current_id.t = t;
    current_id.x = x;
    current_id.y = y;
    current_id.z = z;
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
        cpyfn (arg, data);
        task->args = arg;
        task->fn = fn;
        task->id = current_id;
        /*fn(arg);*/
    }
    else {
        memcpy(dynargs, data, arg_size);
        task->args = dynargs;
        task->fn = fn;
        task->id = current_id;
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
    for (unsigned int i = 0; i < ntasks; i++) {
        task_id_t *id = &tasks[i]->id;
        /*printf("Executing id (t,x,y,z) = (%i,%i,%i,%i)\n", id->t, id->x, id->y, id->z);*/
        tasks[i]->fn(tasks[i]->args);
    }
}

