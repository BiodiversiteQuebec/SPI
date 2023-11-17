#!/bin/bash

#SBATCH --account=def-dgravel
#SBATCH -t 03:00:00
#SBATCH --mem=5120
#SBATCH --job-name=union_prot_areas
#SBATCH --mail-user=victor.cameron@usherbrooke.ca
#SBATCH --mail-type=ALL

module load r/4.2.1 StdEnv/2020

Rscript src/utils-union.r