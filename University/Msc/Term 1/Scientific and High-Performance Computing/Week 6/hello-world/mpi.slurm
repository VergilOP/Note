#!/bin/bash

# 1 node
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=2
#SBATCH --job-name="hello-mpi"
#SBATCH -o hello-mpi.%J.out
#SBATCH -e hello-mpi.%J.err
#SBATCH -t 00:01:00
#SBATCH -p shared

module load gcc
module load openmpi

mpirun -np 2 ./hello-mpi
