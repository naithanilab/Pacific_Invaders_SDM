library(tidyverse)
library(sf)

rm(list = ls())
setwd("~/ownCloud/Projects/Berlin/10_Pacific_invaders")
source("scripts/utils.R")

# -------------------------------------------------- #
#                Prepare data                     ####
# -------------------------------------------------- #
# tdwg regions
tdwg = st_read("data/tdwg_lvl3.geojson")

# occurrences
load("//import/calc9z/data-zurell/koenig/occ_cleaned_slim.RData")

# invasives list
blacklist = read_delim("data/Pacific_Invaders_GIFT_22_01.csv", delim = ";", locale = locale(decimal_mark = ",")) %>%
  filter(inva_stat == "T", Islandgroup == "Hawaiian") %>% 
  distinct(Species)

# -------------------------------------------------- #
#            Assign status based on POWO          ####
# -------------------------------------------------- #
# Download status
# blacklist_status_tdwg = bind_rows(lapply(blacklist$Species, get_status_powo)) %>% 
#   arrange(desc(status)) %>% 
#   group_by(species, tdwg_lvl_3) %>% 
#   slice(1)  %>%             # if species are both native and introduced to a region (very few cases), remove introduced status
#   ungroup()
# save(blacklist_status_tdwg, file = "data/blacklist_status_tdwg.RData")
load("data/blacklist_status_tdwg.RData")

# create simple feature for blacklist occurrences
occ_blacklist_sf = occ_cleaned_slim %>% 
  filter(species %in% blacklist$Species) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(tdwg))

# Spatial join of TDWG regions and occurrences
occ_blacklist_tdwg = st_join(occ_blacklist_sf, tdwg, st_intersects)

# Check Region names
blacklist_regions = sort(unique(blacklist_status_tdwg$tdwg_lvl_3))
tdwg_regions = sort(unique(tdwg$LEVEL3_NAM))
setdiff(blacklist_regions, tdwg_regions)
setdiff(tdwg_regions, blacklist_regions)

# Harmonize POWO regions => TDWG regions
blacklist_status_harmonized = blacklist_status_tdwg %>% 
  mutate(tdwg_lvl_3 = recode(tdwg_lvl_3,
                             `Central African Repu` = "Central African Republic",
                             `Central American Pac` = "C. American Pacific Is.",
                             `Central European Rus` = "Central European Russia",
                             `Cocos (Keeling) Is.` = "Cocos (Keeling) I.",
                             `Gambia` = "Gambia, The",
                             `Kirgizstan` = "Kirgizistan",
                             `Leeward Is.`  = "Leeward Is. AB Ant",
                             `Mozambique Channel I` = "Mozambique Channel Is.",
                             `North European Russi` = "North European Russia",
                             `Northwest European R` = "Northwest European Russia",
                             `Northwest Territorie` = "Northwest Territories",
                             `Panamá` = "Panama",
                             `South European Russi` = "South European Russia",
                             `Suriname` = "Surinam"))

blacklist_regions = sort(unique(blacklist_status_harmonized$tdwg_lvl_3))
setdiff(blacklist_regions, tdwg_regions)   # Should be empty!
setdiff(tdwg_regions, blacklist_regions)

# -------------------------------------------------- #
#          Final join and data saving             ####
# -------------------------------------------------- #
occ_blacklist_status = occ_blacklist_tdwg %>% 
  st_drop_geometry() %>% 
  left_join(blacklist_status_harmonized, by = c("species", "LEVEL3_NAM" = "tdwg_lvl_3")) %>% 
  group_by(occ_id) %>% # some occurrences have been matched to 2 tdwg regions 
  arrange(desc(status)) %>%    # so use only one status (priority: native > introduced > NA)
  slice(1) %>%                 # this operation is somewhat expensive, so be careful with >> 1M records  
  ungroup() %>% 
  mutate(status = replace_na(status, "unknown")) %>% 
  dplyr::select(occ_id, status)
save(occ_blacklist_status, file = "//import/calc9z/data-zurell/koenig/occ_blacklist_status.RData")

load("/./import/calc9z/data-zurell/koenig/occ_cleaned_slim.RData")
blacklist_final = occ_cleaned_slim %>%
  dplyr::select(-dataset, -native) %>%
  right_join(occ_blacklist_status, by = "occ_id")

save(blacklist_final, file = "/./import/calc9z/data-zurell/koenig/blacklist_final.RData")
