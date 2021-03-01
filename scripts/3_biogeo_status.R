library(tidyverse)
library(bRacatus)
library(ggmap)

rm(list = ls())
setwd("~/ownCloud/Projects/Berlin/10_Pacific_invaders")

########### Assign nativeness status ##############
load("data/occ_cleaned.RData")

nativeness_lookup = occ_cleaned %>% 
  dplyr::select(native, is_introduced) %>% 
  distinct()
  
occ_cleaned_slim = occ_cleaned %>% 
  dplyr::filter(.summary == T) %>% # Remove flagged occurrences
  dplyr::select(species, lat, lon, country, year, datasource, dataset, native) 


