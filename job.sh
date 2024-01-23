#!/bin/bash
#PBS -q beta
#PBS -l select=1:ncpus=24:mpiprocs=24
#PBS -l walltime=24:00:00
#PBS -N indicatorRoutine
#PBS -e stderr.txt
#PBS -o result.txt

cd $PBS_O_WORKDIR

module load texlive/2023
module load R/4.3.1

export RENV_CONFIG_SANDBOX_ENABLED=FALSE

mpirun -n 24 /bin/bash $(which R) CMD BATCH --no-save make.R

exit 0

