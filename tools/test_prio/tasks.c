#include "utils.h"

void low_io(elem_t *block, elem_t *dep1, elem_t *dep2, int ld)
{
    for (int i = 0; i < ld; i++)
        for (int j = 0; j < ld; j++) {
            block[i * ld + j] = dep1[i * ld + j] + dep2[i * ld + j];
        }

}

void medium_io(elem_t *block, elem_t *dep1, elem_t *dep2, int ld)
{
    ;
}

void high_io(elem_t *restrict block, elem_t *restrict dep1, elem_t *restrict dep2, int ld)
{
    for (int i = 0; i < ld; i++)
        for (int j = 0; j < ld; j++)
            for (int k = 0; k < ld; k++) {
                block[i * ld + j] += dep1[i * ld + k] * dep2[k * ld + j];
            }
}

