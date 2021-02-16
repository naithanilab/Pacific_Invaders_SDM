library(tidyverse)
library(BIEN)
library(parallel)

setwd("~/ownCloud/Projects/Berlin/10_Pacific_invaders")
###########################
inv_df = read.csv("data/Pacific_Invaders_GIFT_22_01.csv", sep = ";")
inv_specs = unique(inv_df$Species) 

download_bien = mclapply(inv_specs, mc.cores = 6, function(x){
  download_block = BIEN_occurrence_species(x, native.status = T, political.boundaries = T)
  if(nrow(download_block) == 0){
    return(NULL)
  } else {
    return(distinct(download_block[,colnames(download_block) %in% c("scrubbed_species_binomial", "latitude", "longitude", 
                                                                    "date_collected", "country", "datasource", "dataset",
                                                                    "is_introduced", "native_status", "native_status_country", "native_status_state_province")]))
  }
})

save(download_bien, file = "data/download_bien.RData")