#!/bin/bash

# Request resources:
#SBATCH -N 1		# number of compute nodes. 
#SBATCH -n 4		# number of MPI ranks (1 per CPU core)
#SBATCH --mem=1G	# memory required per node, in units M, G or T
#SBATCH --time=0:15:0	# time limit for job (format:  days-hours:minutes:seconds)

# Run in the 'shared' queue (job may share node with other jobs)
#SBATCH -p shared 

# Modules necessary for job:
module purge
module load gcc openmpi

# compile part1.c into part1
mpicc -lm part1.c -o part1

# run part1 with 4 processes (from #SBATCH -n, above)
mpirun ./part1
