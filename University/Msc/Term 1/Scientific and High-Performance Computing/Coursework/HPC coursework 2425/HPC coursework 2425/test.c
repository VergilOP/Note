#include "C:\Program Files (x86)\Microsoft SDKs\MPI\Include\mpi.h"
#include <stdio.h>

int main(int argc, char** argv) {
    MPI_Init(&argc, &argv);

    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    printf("Hello from rank %d out of %d processors!\n", rank, size);

    MPI_Finalize();
    return 0;
}
