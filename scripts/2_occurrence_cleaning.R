library(tidyverse)
library(CoordinateCleaner)
library(lubridate)
library(countrycode)

rm(list = ls())
setwd("~/ownCloud/Projects/Berlin/10_Pacific_invaders")

# -------------------------------------------------- #
#                     Check data                  ####
# -------------------------------------------------- #
# Look at 2018 download data
load("~/ownCloud/Pacific_Island_Invaders_SDM/PacificInvadersSDM/data/PacAlienSpp_GBIFBIEN_occurrences_Aug2018.RData")
table(gbif_bien_occ$occ_Source) # BIEN: 7,979,610 occurrences, GBIF: 27,251,888 occurrences
unique(gbif_bien_occ$spp_binary) # 2569 species
spp_freq_bien_old = gbif_bien_occ %>% dplyr::filter(occ_Source == "BIEN") %>% group_by(spp_binary) %>% tally() # More than 6M highly clustered junk data from stem measurements
spp_freq_gbif_old = gbif_bien_occ %>% dplyr::filter(occ_Source == "GBIF") %>% group_by(spp_binary) %>% tally() # Looks reasonable
rm(gbif_bien_occ)

# Look at fresh BIEN download
file_names = list.files("data/download_bien/", ignore.case = F, full.names = T)
occ_bien = map_dfr(file_names, function(file_name){load(file_name); return(occ_df)})
unique(occ_bien$scrubbed_species_binomial) 
spp_freq_bien = occ_bien %>% group_by(scrubbed_species_binomial) %>% tally() # 6.4 M, Less junk data, most species with considerably more records

# Look at fresh GBIF download
file_names = list.files("data/download_gbif/", ignore.case = F, full.names = T)
occ_gbif = map_dfr(file_names, function(file_name){load(file_name); return(occ_df)})
unique(occ_gbif$species) 
spp_freq_gbif = occ_gbif %>% group_by(species) %>% tally() # 

# -------------------------------------------------- #
#                      Merge datasets             ####
# -------------------------------------------------- #
occ_bien_std = occ_bien %>% 
  select(species = "scrubbed_species_binomial",
         lat = "latitude",
         lon = "longitude",
         country = "country",
         year = "date_collected",
         datasource = "datasource",
         dataset = "dataset",
         native = "native_status") %>% 
  mutate(year = lubridate::year(year), 
         country = countrycode(country, origin = "country.name", destination = "iso3c"))

occ_gbif_std = occ_gbif %>% 
  select(species = "species",
         lat = "decimalLatitude",
         lon = "decimalLongitude",
         country = "country",
         year = "year",
         datasource = "institutionCode",
         dataset = "datasetName",
         native = "establishmentMeans",
         coordinate_uncertainty = "coordinateUncertaintyInMeters") %>% 
  mutate(country = countrycode(country, origin = "country.name", destination = "iso3c"))

# -------------------------------------------------- #
#                   Clean data                    ####
# -------------------------------------------------- #
occ_cleaned = bind_rows(occ_bien_std, occ_gbif_std) %>% 
  mutate_at(vars(lon, lat), round, 4) %>%  # round to four digits, (corresponds to a maximum of 11.13m at equator)
  dplyr::filter(!(is.na(lat) | is.na(lon)), # Only records with coords
                !(lat == lon | lat == 0 | lon == 0), # Coords should not be equal
                !(year < 1900 | year > 2021), # no unrealistic years
                (is.na(coordinate_uncertainty) | coordinate_uncertainty < 10000)) %>% #  coordinate precision < 10km 
  arrange(native, coordinate_uncertainty) %>%  # Sort before distinct() to keep the most informative records 
  distinct(species, lon, lat, year, country, datasource, .keep_all = T) %>% # Remove duplicate or redundant records
  clean_coordinates(lon = "lon", lat = "lat", species = "species", countries = "country", 
                    tests = c("centroids", "capitals", "gbif", "institutions"))

save(occ_cleaned, file = "//import/calc9z/data-zurell/koenig/occ_cleaned.RData")

occ_cleaned_slim = occ_cleaned %>% 
  dplyr::filter(.summary == T) %>% # Remove occurrences that were flagged by coordinateCleaner
  rowid_to_column(var = "occ_id") %>% 
  dplyr::select(occ_id, species, lon, lat, country, year, datasource, dataset, native)

save(occ_cleaned_slim, file = "//import/calc9z/data-zurell/koenig/occ_cleaned_slim.RData")