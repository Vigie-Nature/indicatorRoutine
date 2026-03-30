#!/bin/bash

#SBATCH -p std
#SBATCH -n 1
#SBATCH -N 1
#SBATCH -c 12 # nombre de coeurs
#SBATCH --time=2-00:00:00
#SBATCH -J indicatorRoutineJob
#SBATCH -o log.txt

export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}
export RENV_CONFIG_SANDBOX_ENABLED=FALSE

module load R/4.4.1

R --no-save -f /scratch/petitf/testv3/make.R
#mpirun -np 24 /bin/bash $(which R) CMD BATCH --no-save make.R
# mpirun /bin/bash $(which R) CMD BATCH --no-save make.R

exit 0


