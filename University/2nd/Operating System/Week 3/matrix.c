#include <stdlib.h>
#include <time.h>
#include <stdio.h>
#include <errno.h>

typedef struct {
  int * elems;
  int n_cols;
} matrix_t;

int * get(matrix_t M, int row, int col) {
  return M.elems + (row * M.n_cols) + col;
}

int main() {
  const int n_rows = 10000, n_cols = 10000;

  matrix_t M = { .n_cols = n_cols };
  M.elems = (int *) malloc(sizeof(int)*n_rows*n_cols);

  if (M.elems == NULL)
    exit(errno);

  clock_t start, end;

  // iteration by row
  start = clock();
  
  for (int i = 0; i < n_rows; ++i) {
    for (int j = 0; j < n_cols; ++j) {
      int * e = get(M, i, j);
      *e = 42;
    }
  }

  end = clock();
  {
    int msec = (end - start) * 1000 / CLOCKS_PER_SEC;
    printf("By row it took %d.%03ds\n", msec/1000, msec%1000);
  }

  // iteration by col
  start = clock();

  for (int j = 0; j < n_cols; ++j) {
    for (int i = 0; i < n_rows; ++i) {
      int * x = get(M, i, j);
      *x = 24;
    }
  }

  end = clock();
  {
    int msec = (end - start) * 1000 / CLOCKS_PER_SEC;
    printf("By column it took %d.%03ds\n", msec/1000, msec%1000);
  }

  free(M.elems);

  return 0;
}
