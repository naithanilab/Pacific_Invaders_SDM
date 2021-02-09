library(tidyverse)
library(taxize)
library(rgbif)
library(parallel)

setwd("~/ownCloud/Projects/Berlin/10_Pacific_invaders")
###########################
inv_df = read.csv("data/Pacific_Invaders_GIFT_22_01.csv", sep = ";")
inv_specs = unique(inv_df$Species) 

download_gbif = mclapply(inv_specs, mc.cores = 4, function(x){
  gbif_id = taxize::get_gbifid(x, rank = "species", rows = 1, phylum = "Tracheophyta") %>% as.character()
  if(is.na(gbif_id)){return(NA)}
  
  n_occ = rgbif::occ_count(taxonKey = gbif_id, georeferenced = T)
  n_blocks = ceiling(n_occ / 100000) # Maximum download size of 100.000 occurrences -> create chunks
  lat_ranges = str_replace_all(levels(cut_interval(-90:90, n_blocks)), "\\[|\\]|\\(|\\)", "") # create latitudinal bands for blocked download
  
  download_list = lapply(lat_ranges, FUN = function(lat_range){
    download_block = occ_data(scientificName = x, hasCoordinate = T, limit = 99999, decimalLatitude = lat_range)$data 
    if(is.null(download_block)){
      return(NULL)
    } else {
      return(download_block[,colnames(download_block) %in% c("key", "datasetKey", "scientificName", "acceptedScientificName", "taxonKey", "acceptedTaxonKey", 
                                                             "decimalLatitude", "decimalLongitude", "year", "coordinateUncertaintyInMeters", "issues", "geodeticDatum",
                                                             "countryCode", "country")])
    }
  }) 
  occ_df = bind_rows(download_list) %>% 
    distinct()
})

save(download_gbif, file = "data/download_gbif.RData")