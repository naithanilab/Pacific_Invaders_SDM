library(tidyverse)
library(BIEN)
library(foreach)
library(doParallel)

rm(list = ls())
setwd("~/ownCloud/Projects/Berlin/10_Pacific_invaders")

# -------------------------------------------------- #
#              Define download function           ####
# -------------------------------------------------- #
download_species = function(spec_name){
  # Download BIEN occurrence data for 'spec_name'
  occ_df = BIEN_occurrence_species(spec_name, natives.only = F, cultivated = T, native.status = T, political.boundaries = T) 
  if(nrow(occ_df) == 0){ # if no occurrences available --> return NULL
    return(NULL)
  } else { # else --> return only relevant columns
    return(occ_df[,colnames(occ_df) %in% c("scrubbed_species_binomial", "latitude", "longitude", 
                                                   "date_collected", "country", "datasource", "dataset",
                                                   "is_introduced", "native_status", "native_status_country", "native_status_state_province")])
  }
}

# -------------------------------------------------- #
#          Loop over species and download         ####
# -------------------------------------------------- #
# Read in invasive species from USDA noxious weeds list
usda_weeds = read_csv("data/USDA_Hawaii_noxious_weeds.csv") %>% # Downloaded from https://plants.usda.gov
  pull("Scientific Name") %>%   # Extract vector of scientific names
  taxize::gbif_parse() %>%      # run it through gbif name parser
  dplyr::filter(type == "SCIENTIFIC", !is.na(rankmarker)) %>% # And put back together
  mutate(Species = paste(genusorabove, specificepithet)) %>% 
  dplyr::select(Species) %>% 
  distinct() %>%   # Keep only unique species names
  drop_na() %>% 
  pull(Species)

# Read in invasive species from Michael's Pacific invaders list
inv_specs = read.csv("data/Pacific_Invaders_GIFT_22_01.csv", sep = ";", dec = ".") %>% 
  dplyr::select(Species) %>% 
  distinct() %>% # Keep only unique species names
  drop_na() %>% 
  pull(Species)

# Collect names of already downloaded species
inv_specs_dl = list.files("data/download_bien/") %>% str_remove(".RData") %>% str_replace("_", " ")

# Create list of still to-be-downloaded species
inv_specs_final = setdiff(c(inv_specs, usda_weeds), inv_specs_dl)

# Set up cluster and download species (I ran this on calc9)
cl = makeCluster(8)
registerDoParallel(cl)

# Download data, retry if not successful
foreach(spec_name = inv_specs_final, .packages = c("tidyverse", "BIEN")) %dopar% {
  download_successful = F
  iter = 0
  occ_df = NULL
  while(!(download_successful) & iter < 5){
    tryCatch({  # Sometimes the connection to server is unstable, so wrap occurrence download in tryCatch()
      occ_df = download_species(spec_name) # call above-defined download function with current species
      download_successful = T
      save(occ_df, file = paste0("data/download_bien/", str_replace_all(spec_name, " ", "_"), ".RData")) # save downloaded object
    }, error = function(e){
      cat("download error:", spec_name, "\n")
      Sys.sleep(15)
      iter <<- iter + 1
    })
  }  
}  

stopCluster(cl)