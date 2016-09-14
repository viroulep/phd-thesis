#if __STDC_VERSION__ >= 199901L
#define _XOPEN_SOURCE 600
#else
#define _XOPEN_SOURCE 500
#endif /* __STDC_VERSION__ */
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include "utils.h"
#include "tasks.h"

int main()
{
  int waves = 10;
  int matrix_size = 4096;
  int block_size = 256;
  int nrow = matrix_size / block_size;
  printf("Building matrix with size %i, block_size %i, %i blocks\n", matrix_size, block_size, nrow * nrow);
  void **A = malloc(nrow * nrow * sizeof(void *));
  for (int i = 0; i < nrow; i++)
    for (int j = 0; j < nrow; j++)
      A(i, j) = malloc(block_size * block_size * sizeof(elem_t));
#pragma omp parallel
#pragma omp single
  {
    for (int i = 0; i < nrow; i++)
      for (int j = 0; j < nrow; j++) {
#pragma omp task depend(out: A(i, j))
        {
          elem_t *block = A(i, j);
          for (int ii = 0; ii < block_size; ii++)
            for (int kk = 0; kk < block_size; kk++) {
              block[ii * block_size + kk] = ii * block_size + kk;
            }
        }
      }

  }

  printf("Doing stuff...\n");
  /*for (int i = 0; i < nrow; i++)*/
  /*for (int j = 0; j < nrow; j++) {*/
  /*if ((i+j)%2 != 0)*/
  /*printf("x ");*/
  /*else*/
  /*printf("o ");*/
  /*if (j + 1 == nrow)*/
  /*printf("\n");*/
  /*}*/
  struct timespec start;
  clock_gettime(CLOCK_MONOTONIC, &start);


#pragma omp parallel
#pragma omp single
  {
    for (int i = 0; i < nrow; i++)
      for (int j = 0; j < nrow; j++) {
        if ((i+j)%2 != 0)
          continue;
        int dep1i = i;
        int dep1j = (j + 1)%nrow;
        int dep2i = (i + 1)%nrow;
        int dep2j = j;
#pragma omp task depend(out: A(i, j))
        {
          /*printf("Writing blck A(%i, %i), dep A(%i, %i), A(%i, %i)\n", i, j, dep1i, dep1j, dep2i, dep2j);*/
          high_io(A(i, j), A(dep1i, dep1j), A(dep2i, dep2j), block_size);
        }
      }
  }

  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &end);
  time_t diff_sec = end.tv_sec - start.tv_sec;
  long diff_nsec = end.tv_nsec - start.tv_nsec;
  if (diff_nsec < 0)
    diff_nsec = -diff_nsec;
  printf("Time: %li sec, %li nsec\n", diff_sec, diff_nsec);

#if 0
  for (int i = 0; i < matrix_size; i++)
    for (int j = 0; j < matrix_size; j++) {
      int blocki = i / block_size;
      int blockj = j / block_size;
      int ii = i % block_size;
      int kk = j % block_size;
      elem_t *block = A(blocki, blockj);
      /*printf("%.2i ", block[ii * block_size + kk]);*/
      /*printf("%p ", &block[ii * block_size + kk]);*/
      if (j == matrix_size - 1)
        printf("\n");
    }
#endif
  printf("\n");

}
