#include <stdio.h>
#include <math.h>
#include <mpi.h>
#include "params.h"

// Initialize local grid for each process
void init(double u[N][N], double v[N][N]) {
    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    int rows_per_proc = N / size;
    int start_row = rank * rows_per_proc;
    int end_row = start_row + rows_per_proc;

    for (int i = start_row; i < end_row; i++) {
        for (int j = 0; j < N; j++) {
            u[i][j] = ulo + (uhi - ulo) * 0.5 * (1.0 + tanh((i - N / 2) / 16.0));
            v[i][j] = vlo + (vhi - vlo) * 0.5 * (1.0 + tanh((j - N / 2) / 16.0));
        }
    }
}

// Exchange boundary rows between processes
void exchange_boundaries(double u[N][N], double v[N][N]) {
    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    int rows_per_proc = N / size;
    int start_row = rank * rows_per_proc;
    int above = (rank == 0) ? MPI_PROC_NULL : rank - 1;
    int below = (rank == size - 1) ? MPI_PROC_NULL : rank + 1;

    MPI_Sendrecv(u[start_row], N, MPI_DOUBLE, above, 0,
                 u[start_row + rows_per_proc], N, MPI_DOUBLE, below, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

    MPI_Sendrecv(v[start_row], N, MPI_DOUBLE, above, 1,
                 v[start_row + rows_per_proc], N, MPI_DOUBLE, below, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

    MPI_Sendrecv(u[start_row + rows_per_proc - 1], N, MPI_DOUBLE, below, 2,
                 u[start_row - 1], N, MPI_DOUBLE, above, 2, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

    MPI_Sendrecv(v[start_row + rows_per_proc - 1], N, MPI_DOUBLE, below, 3,
                 v[start_row - 1], N, MPI_DOUBLE, above, 3, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
}

void dxdt(double du[N][N], double dv[N][N], double u[N][N], double v[N][N]) {
    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    int rows_per_proc = N / size;
    int start_row = rank * rows_per_proc;
    int end_row = start_row + rows_per_proc;

    for (int i = start_row; i < end_row; i++) {
        for (int j = 0; j < N; j++) {
            int up = (i == 0) ? i : i - 1;
            int down = (i == N - 1) ? i : i + 1;
            int left = (j == 0) ? j : j - 1;
            int right = (j == N - 1) ? j : j + 1;

            double lapu = u[up][j] + u[down][j] + u[i][left] + u[i][right] - 4.0 * u[i][j];
            double lapv = v[up][j] + v[down][j] + v[i][left] + v[i][right] - 4.0 * v[i][j];

            du[i][j] = DD * lapu + f(u[i][j], v[i][j]) + R * stim(i, j);
            dv[i][j] = d * DD * lapv + g(u[i][j], v[i][j]);
        }
    }
}

void step(double du[N][N], double dv[N][N], double u[N][N], double v[N][N]) {
    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    int rows_per_proc = N / size;
    int start_row = rank * rows_per_proc;
    int end_row = start_row + rows_per_proc;

    for (int i = start_row; i < end_row; i++) {
        for (int j = 0; j < N; j++) {
            u[i][j] += dt * du[i][j];
            v[i][j] += dt * dv[i][j];
        }
    }
}

double norm(double x[N][N]) {
    double local_norm = 0.0, global_norm = 0.0;
    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    int rows_per_proc = N / size;
    int start_row = rank * rows_per_proc;
    int end_row = start_row + rows_per_proc;

    for (int i = start_row; i < end_row; i++) {
        for (int j = 0; j < N; j++) {
            local_norm += x[i][j] * x[i][j];
        }
    }

    MPI_Reduce(&local_norm, &global_norm, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
    return global_norm;
}

int main(int argc, char** argv) {
    MPI_Init(&argc, &argv);

    double t = 0.0, nrmu, nrmv;
    double u[N][N], v[N][N], du[N][N], dv[N][N];

    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    FILE *fptr = NULL;
    if (rank == 0) {
        fptr = fopen("nrms.txt", "w");
        fprintf(fptr, "#t\t\tnrmu\t\tnrmv\n");
    }

    init(u, v);

    for (int k = 0; k < M; k++) {
        t = dt * k;
        exchange_boundaries(u, v);
        dxdt(du, dv, u, v);
        step(du, dv, u, v);

        if (k % m == 0) {
            nrmu = norm(u);
            nrmv = norm(v);

            if (rank == 0) {
                printf("t = %2.1f\tu-norm = %2.5f\tv-norm = %2.5f\n", t, nrmu, nrmv);
                fprintf(fptr, "%f\t%f\t%f\n", t, nrmu, nrmv);
            }
        }
    }

    if (rank == 0) fclose(fptr);

    MPI_Finalize();
    return 0;
}