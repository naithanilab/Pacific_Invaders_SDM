#PBS -N pacific_invaders
#PBS -l ncpus=5,mem=100GB,walltime=1000:00:00
#PBS -q maoekq
#PBS -o ./fit_models_output.txt
#PBS -e ./fit_models_error.txt
cd $PBS_O_WORKDIR
Rscript --vanilla ~/PacificInvadersSDM/scripts/5_model_fitting.R	

