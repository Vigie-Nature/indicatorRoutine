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

# i couldn't manage to renv::install("doMPI") on the cluster,
# but it is installed in the global R/4.3.1 environment :
# setting sandbox to false allows to library() globally installed
# packages
export RENV_CONFIG_SANDBOX_ENABLED=FALSE

# with more than -n 24 the parallel loop crashes on start
# (with a segmentation fault)
mpirun -n 24 /bin/bash $(which R) CMD BATCH --no-save make.R

exit 0

