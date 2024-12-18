#!/bin/bash

#SBATCH -p std
#SBATCH --ntasks=1 
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --time=4-00:00:00
#SBATCH -J indicatorRoutineJob
#SBATCH -o log.txt

export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}

module load R/4.4.1

R --save -f /scratch/bartholu/indicatorroutine/make.R

exit 0
