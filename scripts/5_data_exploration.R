library(tidyverse)
library(sf)
library(maps)

rm(list = ls())
setwd("~/ownCloud/Projects/Berlin/10_Pacific_invaders") #setwd("~/PacificInvadersSDM/")
source("scripts/utils.R")

########## Load data ##########
# load("data/occ_status_final.RData") #load("/./import/calc9z/data-zurell/koenig/occ_status_final.RData")
# load("data/occ_cleaned_slim.RData") #load("/./import/calc9z/data-zurell/koenig/occ_cleaned_slim.RData")
# occ = occ_cleaned_slim %>%
#   dplyr::select(-dataset, -native) %>%
#   left_join(occ_status_final, by = "occ_id") %>%
#   mutate(status = replace_na(status, "unknown"))
# rm(occ_status_final, occ_cleaned_slim)
# save(occ, file = "data/occ.RData")

load("data/occ.RData")
pac_mask = st_read("data/pacific/pac_final.shp")
geoentities = st_read("data/geoentities_simple_2019-08-29/geoentities_simple.shp")
world = map_data("world")
inv_specs = read.csv("data/Pacific_Invaders_GIFT_22_01.csv", sep = ";") %>% pull(Species) %>% unique()

########## Explore global patterns ##########
# Frequency of status assignments
table(occ$status) # hmm...so many naturalized? -> GIFT data accurate?

# Species with most naturalized occurrences
occ_summary_global = occ %>% group_by(species, status) %>% tally(name = "n_global")

# Plot Overview of occurrence dataset
occ_overview_maps = plot_occ_overview()
ggsave(filename = "plots/occ_overview.png", occ_overview_maps, device = "png", width = unit(12, "cm"), height = unit(6, "cm"))

# Look at random species
plot_status(species = sample(inv_specs, 1))

# Suspect status information:
ggsave("plots/status_suspect/RapRap.png", plot_status("Raphanus raphanistrum"), scale = 1.5)
ggsave("plots/status_suspect/ArrEla.png", plot_status("Arrhenatherum elatius"), scale = 1.5)
ggsave("plots/status_suspect/MurPan.png", plot_status("Murraya paniculata"), scale = 1.5)
ggsave("plots/status_suspect/MatInc.png", plot_status("Matthiola incana"), scale = 1.5)

# Plausible status information
ggsave("plots/status_plausible/RivHum.png", plot_status("Rivina humilis"), scale = 1.5)
ggsave("plots/status_plausible/OpuFic.png", plot_status("Opuntia ficus-indica"), scale = 1.5)
ggsave("plots/status_plausible/HypGla.png", plot_status("Hypochaeris glabra"), scale = 1.5)
ggsave("plots/status_plausible/StaJam.png", plot_status("Stachytarpheta jamaicensis"), scale = 1.5)
ggsave("plots/status_plausible/IriDom.png", plot_status("Iris domestica"), scale = 1.5)
ggsave("plots/status_plausible/CecObt.png", plot_status("Cecropia obtusiafolia"), scale = 1.5)
ggsave("plots/status_suspect/PlaSti.png", plot_status("Platymiscium stipulare"), scale = 1.5)

########## Explore pacific islands ############
# Occ / islands * species
# occ_pac_sf = occ %>% # This requires a lot of RAM to be fast!
#   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
#   st_join(pac_mask, left = F)
# save(occ_pac_sf, file = "data/occ_pac_sf.RData")
load("data/occ_pac_sf.RData")
occ_pac = occ_pac_sf %>% 
  dplyr::mutate(lat = sf::st_coordinates(.)[,2], lon = sf::st_coordinates(.)[,1]) %>% 
  st_drop_geometry() %>% 
  select(-FID)

occ_summary_pac = occ_pac %>% group_by(species, status) %>% tally(name = "n_pacific")

ggsave("plots/status_Hawaii.png", plot_status(bbox = c(-161, 18.5,-154,22.5), title = "Hawaii", alpha = 0.1), scale = 1.5) # Hawaii
ggsave("plots/status_Galapagos.png", plot_status(bbox = c(-92,-1.5,-89,1), title = "Galapagos"), scale = 1.5) # Galapagos 
ggsave("plots/status_Fiji.png", plot_status(bbox = c(176.5,-19.5,180,-16), title = "Fiji"), scale = 1.5) # Fiji
ggsave("plots/status_Samoa.png", plot_status(bbox = c(-173,-14.5,-169,-13), title = "Samoa"), scale = 1.5) # Samoa
ggsave("plots/status_Vanuatu.png", plot_status(bbox = c(166.25,-20.35,170,-13.5), title = "Vanuatu"), scale = 1.5) # Vanuatu
ggsave("plots/status_Solomons.png", plot_status(bbox = c(154.5,-11,162.5,-5), title = "Solomons"), scale = 1.5) # Solomons
ggsave("plots/status_Kiribati.png", plot_status(bbox = c(-157.6,1.6,-157,2.1), title = "Kiribati"), scale = 1.5) # Kiribati


########## Explore Hawaii ############
occ_hawaii = occ_pac_sf %>% 
  st_join(filter(geoentities, ge_ntty == "Hawaii"), left = F) %>% 
  st_drop_geometry()
occ_summary_hawaii = occ_hawaii %>% group_by(species, status) %>% tally(name = "n_hawaii")

usda_weeds = read_csv("data/USDA_Hawaii_noxious_weeds.csv") %>% # Where are those species in Michael's list??
  pull("Scientific Name") %>% 
  taxize::gbif_parse() %>% 
  select(species = canonicalname)

plot_status("Verbascum thapsus", bbox = c(-161, 18.5,-154,22.5), alpha = 1) # Great!
plot_status("Cytisus scoparius", bbox = c(-161, 18.5,-154,22.5), alpha = 1) # no occs
plot_status("Bocconia frutescens", bbox = c(-161, 18.5,-154,22.5), alpha = 1) 
plot_status("Prosopis juliflora", bbox = c(-161, 18.5,-154,22.5), alpha = 1)
plot_status("Coccinia grandis", bbox = c(-161, 18.5,-154,22.5), alpha = 1)
plot_status("Morella faya", bbox = c(-161, 18.5,-154,22.5), alpha = 1)
plot_status("Andropogon virginicus", bbox = c(-161, 18.5,-154,22.5), alpha = 1)

AndVir = filter(occ, species == "Andropogon virginicus" & lat >19 & lat < 20 & lon > -156 & lon < -154) 
CocGra = filter(occ, species == "Coccinia grandis" & lat >19 & lat < 22 & lon > -160 & lon < -154)

### Make summary table for final species selection
decision_table = occ_summary_global %>% 
  left_join(occ_summary_pac, by = c("species", "status")) %>% 
  left_join(occ_summary_hawaii, by = c("species", "status")) %>% 
  pivot_longer(cols = starts_with("n_"), names_to = "region", names_prefix = "n_", values_to = "n") %>% 
  replace_na(list(n = 0)) %>% 
  pivot_wider(id_cols = c(species, region), names_from = status, values_from = n, values_fill = 0) %>% 
  mutate(usda_weed = species %in% usda_weeds$species)

ranking = decision_table %>% 
  filter(region == "hawaii") %>% 
  mutate(total_non_native = `non-native` + naturalized + invasive) %>% 
  arrange(desc(usda_weed), desc(total_non_native))
  