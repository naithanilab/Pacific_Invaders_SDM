library(tidyverse)
library(sf)

rm(list = ls())
setwd("~/ownCloud/Projects/Berlin/10_Pacific_invaders")

########### Prepare data ##############
load("data/occ_cleaned.RData")

nativeness_lookup = occ_cleaned %>% 
  dplyr::select(native, is_introduced) %>% 
  distinct()
  
occ_cleaned_slim = occ_cleaned %>% 
  dplyr::filter(.summary == T) %>% # Remove flagged occurrences
  rowid_to_column(var = "occ_id") %>% 
  dplyr::select(occ_id, species, lon, lat, country, year, datasource, dataset, native)

########## Spatial overlay with GIFT geoentities ############
geoentities = st_read("data/geoentities_simple_2019-08-29/geoentities_simple.shp")
geoentities_slim = geoentities %>% dplyr::select(entt_ID)
occ_sf = st_as_sf(occ_cleaned_slim, coords = c("lon", "lat"), crs = st_crs(geoentities))
rm(occ_cleaned, occ_cleaned_slim)

# spatial join of occurrences with gift geoentities
occ_gift_intersect = st_join(occ_sf, geoentities_slim, st_intersects)
save(occ_gift_intersect, file = "data/occ_gift_intersect.RData")