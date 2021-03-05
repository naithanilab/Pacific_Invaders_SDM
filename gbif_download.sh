#PBS -N gbif_download
#PBS -l ncpus=24,mem=40GB,walltime=1000:00:00
#PBS -q maoekq
#PBS -o ./gbif_download_output.txt
#PBS -e ./gbif_download_error.txt
cd $PBS_O_WORKDIR
Rscript --vanilla ~/PacificInvadersSDM/scripts/1_download_gbif.R

