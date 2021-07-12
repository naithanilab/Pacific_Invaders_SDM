library(tidyverse)
library(sf)
library(maps)

rm(list = ls())
setwd("~/PacificInvadersSDM/")
source("scripts/utils.R")

load("data/blacklist_final.RData")
geoentities = st_read("data/geoentities_simple_2019-08-29/geoentities_simple.shp")

# -------------------------------------------------- #
#            Explore global patterns              ####
# -------------------------------------------------- #
world = map_data("world")
specs = unique(blacklist_final$species)

# Frequency of status assignments
table(blacklist_final$status) 

# Frequency of status assignments per species
blacklist_summary_global = blacklist_final %>% group_by(species, status) %>% tally(name = "n_global")

# Plot Overview of occurrence dataset
blacklist_overview_maps = ggplot(blacklist_final, aes(x = lon, y = lat)) +
    geom_map(data = world, map = world, aes(map_id = region), fill = "grey80", color = "grey80", inherit.aes = F) +
    geom_hex(bins = 100) +
    ylim(-90,90) +
    scale_fill_continuous(type = "viridis", trans = "log10") +
    facet_wrap(vars(status)) +
    coord_fixed() +
    theme_bw() 

ggsave(filename = "plots/blacklist_overview.png", blacklist_overview_maps, device = "png", width = unit(12, "cm"), height = unit(3, "cm"))

# Look at random species
plot_status(blacklist_final, species = sample(specs, 1))

# Plot some species:
ggsave("plots/status_POWO/ArrEla.png", plot_status(blacklist_final, "Arrhenatherum elatius"), scale = 1.5)
ggsave("plots/status_POWO/PsiGua.png", plot_status(blacklist_final, "Psidium guajava"), scale = 1.5)
ggsave("plots/status_POWO/PlaSti.png", plot_status(blacklist_final, "Platymiscium stipulare"), scale = 1.5)
ggsave("plots/status_POWO/StyGui.png", plot_status(blacklist_final, "Stylosanthes guianensis"), scale = 1.5)
ggsave("plots/status_POWO/AscCur.png", plot_status(blacklist_final, "Asclepias curassavica"), scale = 1.5)
ggsave("plots/status_POWO/BarLup.png", plot_status(blacklist_final, "Barleria lupulina"), scale = 1.5)

# -------------------------------------------------- #
#            Explore Pacific islands              ####
# -------------------------------------------------- #
# pac_mask = st_read("data/pacific/pac_final.shp")
# blacklist_pac_sf = blacklist_final %>% 
#   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
#   st_join(pac_mask, left = F)
# save(blacklist_pac_sf, file = "data/blacklist_pac_sf.RData")
load("data/blacklist_pac_sf.RData")

blacklist_pac = blacklist_pac_sf %>% 
  dplyr::mutate(lat = sf::st_coordinates(.)[,2], lon = sf::st_coordinates(.)[,1]) %>% 
  st_drop_geometry() %>% 
  dplyr::select(-FID)

blacklist_summary_pac = blacklist_pac %>% group_by(species, status) %>% tally(name = "n_pacific")

plot_status(blacklist_final, bbox = c(-161, 18.5,-154,22.5), title = "Hawaii") # Hawaii
plot_status(blacklist_final, bbox = c(-92,-1.5,-89,1), title = "Galapagos") # Galapagos 
plot_status(blacklist_final, bbox = c(176.5,-19.5,180,-16), title = "Fiji") # Fiji
plot_status(blacklist_final, bbox = c(-173,-14.5,-169,-13), title = "Samoa") # Samoa
plot_status(blacklist_final, bbox = c(166.25,-20.35,170,-13.5), title = "Vanuatu") # Vanuatu
plot_status(blacklist_final, bbox = c(154.5,-11,162.5,-5), title = "Solomons") # Solomons
plot_status(blacklist_final, bbox = c(-157.6,1.6,-157,2.1), title = "Kiribati") # Kiribati

# -------------------------------------------------- #
#                   Explore Hawaii                ####
# -------------------------------------------------- #
blacklist_hawaii = blacklist_pac_sf %>% 
  st_join(filter(geoentities, ge_ntty == "Hawaii"), left = F) %>% 
  st_drop_geometry()
blacklist_summary_hawaii = blacklist_hawaii %>% group_by(species, status) %>% tally(name = "n_hawaii")

# -------------------------------------------------- #
#                  Summarize data                 ####
# -------------------------------------------------- #
summary_table = blacklist_summary_global %>% 
  left_join(blacklist_summary_pac, by = c("species", "status")) %>% 
  left_join(blacklist_summary_hawaii, by = c("species", "status")) %>% 
  pivot_longer(cols = starts_with("n_"), names_to = "region", names_prefix = "n_", values_to = "n") %>% 
  replace_na(list(n = 0)) %>% 
  pivot_wider(id_cols = species, names_from = c(status, region), values_from = n, values_fill = 0) %>% 
  arrange(desc(introduced_hawaii), desc(introduced_global), desc(native_global))

save(summary_table, file = "data/summary_table.RData")

plot_status(blacklist_final, summary_table$species[1])
plot_status(blacklist_final, summary_table$species[2])
plot_status(blacklist_final, summary_table$species[3])
plot_status(blacklist_final, summary_table$species[4])
plot_status(blacklist_final, summary_table$species[5])
plot_status(blacklist_final, summary_table$species[6])
plot_status(blacklist_final, summary_table$species[7])
plot_status(blacklist_final, summary_table$species[8])
plot_status(blacklist_final, summary_table$species[9])