#ifndef PRIO_TASKS_H
#define PRIO_TASKS_H

#include "utils.h"

void low_io(elem_t *block, elem_t *dep1, elem_t *dep2, int ld);
void medium_io(elem_t *block, elem_t *dep1, elem_t *dep2, int ld);
void high_io(elem_t *restrict block, elem_t *restrict dep1, elem_t *restrict dep2, int ld);

#endif
