library(tidyverse)

rm(list = ls())
setwd("~/ownCloud/Projects/Berlin/10_Pacific_invaders")

########## Get & prepare data ############
# Michael's lookup table
status_pacific = read_delim("data/Pacific_Invaders_GIFT_22_01.csv", delim = ";", locale = locale(decimal_mark = ",")) %>% 
  dplyr::filter_all(any_vars(!is.na(.))) %>% 
  dplyr::select(-1)
save(status_pacific, file = "data/status_pacific.RData")

# GIFT checklists
source("https://raw.githubusercontent.com/BioGeoMacro/GIFT-export/master/GIFT_export_tools.R")
get_credentials() # IP: 134.76.19.22, Port: 3306
status_gift = DB_get_checklists_conditional(native_indicated = T, complete_taxonomy = F) %>% 
  dplyr::select(ref_ID, entity_ID, work_ID, species, native, naturalized)
save(status_gift, file = "data/status_gift.RData")

########## Assign nativeness status ############
load("data/occ_gift_intersect.RData")
load("data/status_gift.RData")
load("data/status_pacific.RData")

status_gift_slim = status_gift %>% 
  dplyr::select(entity_ID, species, native_gift = native, naturalized_gift = naturalized)

status_pacific_slim = status_pacific %>% 
  mutate(native_pac = 0,
         naturalized_pac = ifelse(inva_stat %in% c("T", "naturalized", "invasive"), 1, 0),
         invasive_pac = ifelse(inva_stat %in% c("T", "invasive"), 1, 0)) %>% 
  dplyr::select(entity_ID = GIFT, species = Species, native_pac, naturalized_pac, invasive_pac)

status_merged = full_join(status_gift_slim, status_pacific_slim, by = c("entity_ID", "species"))

occ_final = left_join(occ_gift_intersect, status_merged, by = c(entt_ID = "entity_ID", species = "species"))