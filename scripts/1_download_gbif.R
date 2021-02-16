library(tidyverse)
library(taxize)
library(rgbif)
library(foreach)
library(doParallel)

rm(list = ls())
setwd("~/ownCloud/Projects/Berlin/10_Pacific_invaders")

###########################
# Define download function
download_species = function(spec_name){
  # Prepare download
  gbif_id = taxize::get_gbifid(spec_name, rank = "species", rows = 1, phylum = "Tracheophyta") %>% as.character()
  if(is.na(gbif_id)){
    return(NA)
  }
  n_occ = rgbif::occ_count(taxonKey = gbif_id, georeferenced = T)
  n_blocks = ceiling(n_occ / 50000) # Maximum download size of 100.000 occurrences -> create chunks with safety margins for uneven latitudinal distribution (50000 occ)
  lat_ranges = str_replace_all(levels(cut_interval(-90:90, n_blocks)), "\\[|\\]|\\(|\\)", "") # create latitudinal bands for blocked download
  
  # Download data in latitudinal bands, retry of not successful
  download_list = lapply(lat_ranges, FUN = function(lat_range){  
    download_block = rgbif::occ_data(scientificName = spec_name, hasCoordinate = T, limit = 100000, decimalLatitude = lat_range)$data
    if(is.null(download_block)){
      return(NULL)
    } else {
      return(download_block[,colnames(download_block) %in% c("key", "datasetKey", "scientificName", "acceptedScientificName", "taxonKey", "acceptedTaxonKey", 
                                                             "decimalLatitude", "decimalLongitude", "year", "coordinateUncertaintyInMeters", "issues", "geodeticDatum",
                                                             "countryCode", "country")])
    }
  }) 
  occ_df = bind_rows(download_list) %>% distinct()
  return(occ_df)
}

# Loop over species and download occurrences
cl = makeCluster(7)
registerDoParallel(cl)

inv_df = read.csv("data/Pacific_Invaders_GIFT_22_01.csv", sep = ";")
inv_specs = unique(inv_df$Species) 
inv_specs_dl = list.files("data/gbif_download/") %>% str_remove(".RData") %>% str_replace("_", " ")
inv_specs_final = setdiff(inv_specs, inv_specs_dl)
foreach(spec_name = inv_specs_final, .packages = c("tidyverse", "taxize", "rgbif")) %dopar% {
  download_successful = F
  iter = 0
  occ_df = NULL
  while(!(download_successful) & iter < 5){
    cat(download_successful, iter, "\n")
    tryCatch({
      occ_df = download_species(spec_name)
      download_successful = T
    }, error = function(e){
      cat("download error:", spec_name, "\n")
      Sys.sleep(15)
      iter <<- iter + 1
    })
  }  
  save(occ_df, file = paste0("data/gbif_download/", str_replace_all(spec_name, " ", "_"), ".RData"))
}  

stopCluster(cl)
