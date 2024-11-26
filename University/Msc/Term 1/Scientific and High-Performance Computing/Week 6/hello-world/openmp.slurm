#!/bin/bash

# 1 node
#SBATCH --nodes=1
#SBATCH --cores=2
#SBATCH --job-name="hello-openmp"
#SBATCH -o hello-openmp.%J.out
#SBATCH -e hello-openmp.%J.err
#SBATCH -t 00:01:00
#SBATCH -p shared

module load gcc

# unnecessary, since we've specified --cores=2 above
export OMP_NUM_THREADS=2
./hello-openmp
