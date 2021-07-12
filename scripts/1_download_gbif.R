library(tidyverse)
library(taxize)
library(rgbif)
library(foreach)
library(doParallel)

rm(list = ls())
setwd("~/ownCloud/Projects/Berlin/10_Pacific_invaders")

# -------------------------------------------------- #
#              Define download function           ####
# -------------------------------------------------- #
download_species = function(spec_name){
  # find gbif_id, Prepare download
  gbif_id = taxize::get_gbifid(spec_name, rank = "species", rows = 1, phylum = "Tracheophyta") %>% as.character()
  n_occ = rgbif::occ_count(taxonKey = gbif_id, georeferenced = T)
  if(n_occ == 0){
    return(NULL)
  }
  
  # Unfortunately, there is a hard limit on the maximum download size via rgbif: 100.000 occurrences 
  # For species with more occurrences, we need to create apply additional filters to reduce the download size
  # I used longitudinal bands as additional subset on the occurrence data
  n_blocks = ceiling(n_occ / 50000) # 50000 instead of 100000, to account for uneven geographical distribution
  long_ranges = str_replace_all(levels(cut_interval(-180:180, n_blocks)), "\\[|\\]|\\(|\\)", "") # create longitudinal bands for blocked download
  
  # Download data in longitudinal bands
  download_list = lapply(long_ranges, FUN = function(long_range){  
    download_block = rgbif::occ_data(scientificName = spec_name, hasCoordinate = T, limit = 100000, decimalLongitude = long_range)$data
    if(is.null(download_block)){
      return(NULL)
    } else {
      return(download_block[,colnames(download_block) %in% c("scientificName", "species", "institutionCode", "datasetName",
                                                             "decimalLatitude", "decimalLongitude", "year", "coordinateUncertaintyInMeters", "issues", "geodeticDatum",
                                                             "establishmentMeans", "countryCode", "country")])
    }
  }) 
  
  # Bind blocked data together
  occ_df = bind_rows(download_list) %>% distinct()
  return(occ_df)
}

# -------------------------------------------------- #
#          Loop over species and download         ####
# -------------------------------------------------- #
# Read in invasive species from USDA noxious weeds list
usda_weeds = read_csv("data/USDA_Hawaii_noxious_weeds.csv") %>% # Where are those species in Michael's list??
  pull("Scientific Name") %>% 
  taxize::gbif_parse() %>% 
  dplyr::filter(type == "SCIENTIFIC", !is.na(rankmarker)) %>% 
  mutate(Species = paste(genusorabove, specificepithet)) %>% 
  dplyr::select(Species) %>% 
  distinct() %>% 
  drop_na() %>% 
  pull(Species)

# Read in invasive species from Michael's Pacific invaders list
inv_specs = read.csv("data/Pacific_Invaders_GIFT_22_01.csv", sep = ";", dec = ".") %>% 
  dplyr::select(Species) %>% 
  distinct() %>% 
  drop_na() %>% 
  pull(Species)

# Collect names of already downloaded species
inv_specs_dl = list.files("data/download_gbif/") %>% str_remove(".RData") %>% str_replace("_", " ")

# Create list of still to-be-downloaded species
inv_specs_final = setdiff(c(inv_specs, usda_weeds), inv_specs_dl) 

# Set up cluster and download species (I ran this on calc9)
cl = makeCluster(8)
registerDoParallel(cl)

# Download data
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