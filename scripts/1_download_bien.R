library(tidyverse)
library(BIEN)
library(parallel)

rm(list = ls())
setwd("~/ownCloud/Projects/Berlin/10_Pacific_invaders")

# -------------------------------------------------- #
#              Define download function           ####
# -------------------------------------------------- #
download_species = function(spec_name){
  occ_df = BIEN_occurrence_species(spec_name, natives.only = F, cultivated = T, native.status = T, political.boundaries = T)
  if(nrow(occ_df) == 0){
    return(NULL)
  } else {
    return(occ_df[,colnames(occ_df) %in% c("scrubbed_species_binomial", "latitude", "longitude", 
                                                   "date_collected", "country", "datasource", "dataset",
                                                   "is_introduced", "native_status", "native_status_country", "native_status_state_province")])
  }
  return(occ_df)
}

# -------------------------------------------------- #
#          Loop over species and download         ####
# -------------------------------------------------- #s
usda_weeds = read_csv("data/USDA_Hawaii_noxious_weeds.csv") %>% # Where are those species in Michael's list??
  pull("Scientific Name") %>% 
  taxize::gbif_parse() %>% 
  dplyr::filter(type == "SCIENTIFIC", !is.na(rankmarker)) %>% 
  mutate(Species = paste(genusorabove, specificepithet)) %>% 
  dplyr::select(Species) %>% 
  distinct() %>% 
  drop_na() %>% 
  pull(Species)

inv_specs = read.csv("data/Pacific_Invaders_GIFT_22_01.csv", sep = ";", dec = ".") %>% 
  dplyr::select(Species) %>% 
  distinct() %>% 
  drop_na() %>% 
  pull(Species)

inv_specs_dl = list.files("data/download_bien/") %>% str_remove(".RData") %>% str_replace("_", " ")
inv_specs_final = setdiff(c(inv_specs, usda_weeds), inv_specs_dl)

cl = makeCluster(8)
registerDoParallel(cl)
foreach(spec_name = inv_specs_final, .packages = c("tidyverse", "BIEN")) %dopar% {
  download_successful = F
  iter = 0
  occ_df = NULL
  while(!(download_successful) & iter < 5){
    tryCatch({
      occ_df = download_species(spec_name)
      download_successful = T
      save(occ_df, file = paste0("data/download_bien/", str_replace_all(spec_name, " ", "_"), ".RData"))
    }, error = function(e){
      cat("download error:", spec_name, "\n")
      Sys.sleep(15)
      iter <<- iter + 1
    })
  }  
}  

stopCluster(cl)