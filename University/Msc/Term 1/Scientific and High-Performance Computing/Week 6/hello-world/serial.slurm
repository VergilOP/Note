#!/bin/bash

# 1 core
#SBATCH -n 1
#SBATCH --job-name="hello-serial"
#SBATCH -o hello-serial.%J.out
#SBATCH -e hello-serial.%J.err
#SBATCH -t 00:01:00
#SBATCH -p test

module load gcc

./hello-serial
