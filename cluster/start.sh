#!/bin/bash

#SBATCH --array=1-190
#SBATCH --account=def-dgravel
#SBATCH -t 02:00:00
#SBATCH --mem=2048
#SBATCH --job-name=SPI
#SBATCH --mail-user=victor.cameron@usherbrooke.ca
#SBATCH --mail-type=ALL

echo $SLURM_ARRAY_TASK_ID

module load r/4.2.1 StdEnv/2020

Rscript cluster/01-run_SPI_cluster.r
