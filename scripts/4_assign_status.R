library(tidyverse)
library(sf)
library(maps)

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

########## Append nativeness status ############
load("/./import/calc9z/data-zurell/koenig/occ_gift_intersect.RData")
load("data/status_gift.RData")
load("data/status_pacific.RData")

status_gift_slim = status_gift %>% 
  dplyr::select(entity_ID, species, native_gift = native, naturalized_gift = naturalized) %>% 
  arrange(desc(native_gift), desc(naturalized_gift)) %>% # Some records may be duplicated due to aggregation of multiple checklists, so sort by status
  distinct(entity_ID, species, .keep_all = T) # and keep records with most informative status information

status_pacific_slim = status_pacific %>% 
  mutate(native_pac = 0, # all species in this list are non-native to the corresponding island
         naturalized_pac = recode(inva_stat, "T" = 1, "naturalized" = 1, "invasive" = 1, .default = 0), # But only some are naturalized
         invasive_pac = recode(inva_stat, "T" = 1, "invasive" = 1, .default = 0)) %>% # And even fewer are invasive
  dplyr::select(entity_ID = GIFT, species = Species, native_pac, naturalized_pac, invasive_pac) %>% 
  arrange(desc(native_pac), desc(naturalized_pac), desc(invasive_pac)) %>% # Some records may be duplicated due to aggregation of multiple checklists, so sort by status
  distinct(entity_ID, species, .keep_all = T) # and keep records with most informative status information

status_merged = full_join(status_gift_slim, status_pacific_slim, by = c("entity_ID", "species"))

occ_status = occ_gift_intersect %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>% 
  sf::st_drop_geometry() %>% 
  left_join(status_merged, by = c(entt_ID = "entity_ID", species = "species")) %>% 
  mutate(native_orig = recode(native, "NATIVE" = 1, "N" = 1, "I" = 1, .default = 0),  # Abbreviations unclear, but my guess is N = Native, I = Indigenous
         naturalized_orig = recode(native, "NATURALISED" = 1, "INVASIVE" = 1, "MANAGED" = 1, .default = 0),
         invasive_orig = recode(native, "INVASIVE" = 1, "MANAGED" = 1, .default = 0)) %>% 
  select(occ_id, species, lon, lat, entt_ID, country, year, datasource, dataset, native_orig, naturalized_orig, invasive_orig, native_gift, naturalized_gift, native_pac, naturalized_pac, invasive_pac)
save(occ_status, file = "/./import/calc9z/data-zurell/koenig/occ_status.RData")

########## Check matched dataset ##########
# Make some plots
geoentities_plot = geoentities %>% dplyr::filter(entt_ID %in% unique(status_gift$entity_ID))
map_entities_matched = ggplot(map_data("world"), aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "lightgray") +
  geom_sf(data = geoentities_plot, color = "darkblue", fill = "lightblue", size = 0.1, inherit.aes = F) +
  theme_bw() 
ggsave(filename = "geoentities_map.png", map_entities_matched, device = "png", width = unit(12, "cm"), height = unit(6, "cm"))

occ_plot = occ_status %>% 
  filter(!is.na(native_orig) | !is.na(native_gift) | !is.na(native_pac)) %>% 
  distinct(occ_id, lon, lat)

map_occ_overview = ggplot(occ_plot, aes(x = lon, y = lat)) +
  geom_hex(bins = 100) +
  scale_fill_continuous(type = "viridis", trans = "log10") +
  ggtitle("Occurrences with status information") +
  coord_fixed() +
  theme_bw() 
ggsave(filename = "occ_overview.png", map_occ_overview, device = "png", width = unit(12, "cm"), height = unit(6, "cm"))

# 1. Number of occurrence records with matched GIFT ID
n_total = occ_status %>%
  distinct(occ_id) %>% 
  nrow() # 23495390

n_total_matched = occ_status %>%
  filter(!is.na(entt_ID)) %>% 
  distinct(occ_id) %>% 
  nrow() # 22632147

entities_pac = unique(status_pacific$GIFT)
n_total_isl = occ_status %>% 
  filter(entt_ID %in% entities_pac) %>% 
  distinct(occ_id) %>%  # more than 1M pacific occurrences matched
  nrow()

# 2. Still so many unmatched islands?
matched = unique(occ_gift_intersect$entt_ID)
unmatched_old = read_csv("data/gift_not_in_occ.csv") %>% pull(entt_ID)
unmatched_new = setdiff(unmatched_old, matched) # Still 205 unmatched

geoentities = st_read("data/geoentities_simple_2019-08-29/geoentities_simple.shp")
geoentities_unmatched = dplyr::filter(geoentities, entt_ID %in% unmatched_new) 
# There are still 205 unmatched pacific islands, but this is not unexpected, because
# (1) The original species list only contains non-natives from Hawaii. It's possible that none of them occurs on a given other Pacific island
# (2) GBIF data are patchy and some islands may be very poorly sampled

# 3. How many records have info on nativeness, naturalization, and invasive status?
n_native = occ_status %>% 
  filter(!is.na(native_orig) | !is.na(native_gift) | !is.na(native_pac)) %>% 
  distinct(occ_id) %>% 
  nrow() # 21098470
n_native_isl = occ_status %>% 
  filter(entt_ID %in% entities_pac & (!is.na(native_orig) | !is.na(native_gift) | !is.na(native_pac))) %>% 
  distinct(occ_id) %>% 
  nrow() # 248339

n_naturalized = occ_status %>% 
  filter(!is.na(naturalized_orig) | !is.na(naturalized_gift) | !is.na(naturalized_pac)) %>% 
  distinct(occ_id) %>% 
  nrow() # 19164069
n_naturalized_isl = occ_status %>% 
  filter(entt_ID %in% entities_pac & (!is.na(naturalized_orig) | !is.na(naturalized_gift) | !is.na(naturalized_pac))) %>% 
  distinct(occ_id) %>% 
  nrow() # 224865

n_invasive = occ_status %>% 
  filter(!is.na(invasive_orig) | !is.na(invasive_pac)) %>% 
  distinct(occ_id) %>% 
  nrow() # 2065931
n_invasive_isl = occ_status %>% 
  filter(entt_ID %in% entities_pac & (!is.na(invasive_orig) | !is.na(invasive_pac))) %>% 
  distinct(occ_id) %>% 
  nrow() # 141243

########## Harmonize nativeness status ############
