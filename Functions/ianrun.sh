#!/bin/bash -l
#SBATCH --cpus-per-task=50
#SBATCH --mem=500G
#SBATCH --time=1-00:15:00     # 1 day and 15 minutes
#SBATCH --mail-user=email@address.com
#SBATCH --mail-type=ALL
#SBATCH --job-name="just_a_test"
#SBATCH -p amd # This is the default partition
#SBATCH --output=log_filename.log #if you do not use this, it will create a slurm-[jobid].out as the log

#your other command down here
module load r


R CMD BATCH ianrun.R

