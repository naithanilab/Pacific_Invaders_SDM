library(tidyverse)
library(sf)
library(maps)


rm(list = ls())
setwd("~/ownCloud/Projects/Berlin/10_Pacific_invaders")

# -------------------------------------------------- #
#             Get and prepare data                ####
# -------------------------------------------------- #
# # Michael's lookup table
# status_pacific = read_delim("data/Pacific_Invaders_GIFT_22_01.csv", delim = ";", locale = locale(decimal_mark = ",")) %>% 
#   dplyr::filter_all(any_vars(!is.na(.))) %>% 
#   dplyr::select(-1)
# save(status_pacific, file = "data/status_pacific.RData")
# 
# # GIFT checklists
# source("https://raw.githubusercontent.com/BioGeoMacro/GIFT-export/master/GIFT_export_tools.R")
# get_credentials() # IP: 134.76.19.22, Port: 3306
# status_gift = DB_get_checklists_conditional(native_indicated = T, complete_taxonomy = F) %>% 
#   dplyr::select(ref_ID, entity_ID, work_ID, species, native, naturalized)
# save(status_gift, file = "data/status_gift.RData")

# -------------------------------------------------- #
#          Append nativeness status               ####
# -------------------------------------------------- #
load("/./import/calc9z/data-zurell/koenig/occ_gift_intersect.RData")
load("data/status_gift.RData")
load("data/status_pacific.RData")
inv_specs = read.csv("data/Pacific_Invaders_GIFT_22_01.csv", sep = ";") %>% 
  pull(Species) %>% 
  unique()
geoentities_area = st_read("data/geoentities_simple_2019-08-29/geoentities_simple.shp") %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(entt_ID, area)

# GIFT status
status_gift_slim = status_gift %>% 
  dplyr::select(entity_ID, species, native, naturalized) %>% 
  filter(species %in% inv_specs & !is.na(native)) %>% 
  arrange(native, naturalized) %>%      # Some records may be duplicated due to aggregation of multiple checklists, so sort by status
  group_by(entity_ID, species) %>% 
  dplyr::slice(1) %>%                          # and keep records with most informative status information
  ungroup() %>% 
  mutate(invasive = NA, source = "GIFT")

# Pacific status
status_pacific_slim = status_pacific %>% 
  filter(!is.na(GIFT)) %>% 
  mutate(native = 0, # all species in this list are non-native to the corresponding island
         naturalized = recode(inva_stat, "T" = 1, "naturalized" = 1, "invasive" = 1, .default = 0), # But only some are naturalized
         invasive = recode(inva_stat, "T" = 1, "invasive" = 1, .default = 0),  # And even fewer are invasive
         source = dataset) %>% 
  dplyr::select(entity_ID = GIFT, species = Species, native, naturalized, invasive, source) %>% 
  arrange(native, naturalized, invasive) %>% # Some records may be duplicated due to aggregation of multiple checklists, so sort by status
  group_by(entity_ID, species) %>% 
  dplyr::slice(1) %>%                          # and keep records with most informative status information
  ungroup()

# Original status
status_orig_slim = occ_gift_intersect %>% 
  sf::st_drop_geometry() %>% 
  filter(native %in% c("NATIVE", "N", "I", "NATURALISED", "INVSIVE", "MANAGED")) %>% 
  mutate(invasive = recode(native, "INVASIVE" = 1, "MANAGED" = 1),
         naturalized = recode(native, "NATURALISED" = 1, "INVASIVE" = 1, "MANAGED" = 1),
         native= recode(native, "NATIVE" = 1, "N" = 1, "Ne" = 1, "P" = 1, "A" = 0, "I" = 0, "Ie" = 0), # Exact meaning of BIEN abbreviations unclear, but binary nativeness status can be inferred by is_introduced column in bien data 
         source = datasource) %>%
  dplyr::select(occ_id, native, naturalized, invasive, source) %>% 
  distinct() %>% 
  tibble()

# -------------------------------------------------- #
#              Harmonize status info              ####
# -------------------------------------------------- #
# Hamonize with following priority:
# 1. Michael's list
# 2. original status information
# 3. Gift

# 1. Join GIFT and Pacific list
status_by_entity = anti_join(status_gift_slim, status_pacific_slim, by = c("entity_ID","species")) %>% 
  bind_rows(status_pacific_slim) # prioritize status from pacific invaders list

# 2. Join occurrences with GIFT + PAC status info
occ_status = occ_gift_intersect %>% 
  sf::st_drop_geometry() %>%  
  dplyr::select(occ_id, entt_ID, species) %>% 
  inner_join(status_by_entity, by = c(entt_ID = "entity_ID", species = "species")) %>%  # leave only occs with status info
  left_join(geoentities_area, by = "entt_ID")  %>%            # join area per entity_ID
  arrange(area) %>%                                           # Sort by area
  group_by(occ_id) %>%
  dplyr::slice(1)  %>%                                        # Use status info from smallest unit per occ_ID
  dplyr::select(-entt_ID, -species, -area) %>% 
  ungroup() %>% 
  tibble()

# 3. Join back original status info
occ_status_final = occ_status %>% 
  filter(!(source == "GIFT" & occ_id %in% status_orig_slim$occ_id)) %>%  # remove GIFT (but not PAC) entries that have orig status
  bind_rows(filter(status_orig_slim, !occ_id %in% .$occ_id)) %>%         # and bind status original source
  mutate(naturalized = replace(naturalized, invasive == 1, 1),
         native = replace(native, naturalized == 1, 0),
         status = case_when(invasive == 1 ~ "invasive",
                            naturalized == 1 ~ "naturalized",
                            native == 0 ~ "non-native",
                            native == 1 ~ "native",
                            TRUE ~ "unknown")) %>% 
  rename(status_source = source) %>% 
  dplyr::select(occ_id, status, status_source)

save(occ_status_final, file = "/./import/calc9z/data-zurell/koenig/occ_status_final.RData")