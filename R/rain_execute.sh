#!/bin/bash
#SBATCH --time=12:00:00
#SBATCH --account=def-sgulati
#SBATCH --mem=100G
#SBATCH --mail-user=madhok.raahil@gmail.com
#SBATCH --mail-type=END

module load r/4.0.0
module load gcc/7.3.0

cd /home/rmadhok/projects/def-sgulati/rmadhok/ebird_lockdown/scripts
Rscript weather_process_gpm.R