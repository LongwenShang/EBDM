#!/bin/bash
#SBATCH -J EBDM_sim
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH -t 06:00:00
# SBATCH --account=your-account-name  # (Optional) specify your SLURM account

# Load R module (adjust according to your cluster environment)
module load r

# Optional: set R_LIBS if required by your cluster
# mkdir -p ~/.local/R/$EBVERSIONR/
# export R_LIBS=~/.local/R/$EBVERSIONR/

# Get the setting index
j=$1

# Run 1000 replicates for the specified setting
for (( i=1; i<=1000; i++ )); do
  Rscript --max-ppsize=500000 ./main.R $i $j
done
