#!/bin/bash
## Example of R job
#SBATCH --job-name=USA_BFT
#SBATCH --partition=haswell
#SBATCH --mail-type=ALL
#SBATCH --mail-user=katelynn.lankowicz@maine.edu
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=4gb
#SBATCH --time=23:59:59
#SBATCH --output=USA_BFT_%j.log

## Load application environment
module load geos gdal proj/5.2.0 libxml2 sqlite3
module load R/4.2.2

## Change working directory
## cd VAST_BFT/VAST_runs/tuna10_usonly

## Run application commands
Rscript --vanilla BFT_nx300_ncell3000_Large_US.R
