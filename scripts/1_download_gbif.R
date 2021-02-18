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
  n_occ = rgbif::occ_count(taxonKey = gbif_id, georeferenced = T)
  if(n_occ == 0){
    return(NULL)
  }
  n_blocks = ceiling(n_occ / 50000) # Maximum download size of 100.000 occurrences -> create chunks with safety margins for uneven geogr. distribution (50000 occ).
  long_ranges = str_replace_all(levels(cut_interval(-180:180, n_blocks)), "\\[|\\]|\\(|\\)", "") # create longitudinal bands for blocked download
  
  # Download data in longitudinal bands, retry of not successful
  download_list = lapply(long_ranges, FUN = function(long_range){  
    download_block = rgbif::occ_data(scientificName = spec_name, hasCoordinate = T, limit = 100000, decimalLongitude = long_range)$data
    if(is.null(download_block)){
      return(NULL)
    } else {
      return(download_block[,colnames(download_block) %in% c("key", "datasetKey", "scientificName", "species", "acceptedTaxonKey", 
                                                             "decimalLatitude", "decimalLongitude", "year", "coordinateUncertaintyInMeters", "issues", "geodeticDatum",
                                                             "establishmentMeans", "countryCode", "country")])
    }
  }) 
  occ_df = bind_rows(download_list) %>% distinct()
  return(occ_df)
}

# Loop over species and download occurrences
cl = makeCluster(8)
registerDoParallel(cl)

inv_df = read.csv("data/Pacific_Invaders_GIFT_22_01.csv", sep = ";")
inv_specs = unique(inv_df$Species) 
inv_specs = inv_specs[!is.na(inv_specs)]
inv_specs_dl = list.files("data/download_gbif/") %>% str_remove(".RData") %>% str_replace("_", " ")
inv_specs_final = setdiff(inv_specs, inv_specs_dl)
foreach(spec_name = inv_specs_final, .packages = c("tidyverse", "taxize", "rgbif")) %dopar% {
  tryCatch({
    occ_df = download_species(spec_name)
    download_successful = T
    if(!is.null(occ_df)){
      save(occ_df, file = paste0("data/download_gbif/", str_replace_all(spec_name, " ", "_"), ".RData"))  
    }
  }, error = function(e){
    return(NULL)
  })
}  

stopCluster(cl)