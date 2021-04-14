library(tidyverse)
library(sf)
library(maps)

rm(list = ls())
setwd("~/ownCloud/Projects/Berlin/10_Pacific_invaders")
#setwd("~/PacificInvadersSDM/")

########## Global Overview ##########
#load("/./import/calc9z/data-zurell/koenig/occ_status_final.RData")
#load("/./import/calc9z/data-zurell/koenig/occ_cleaned_slim.RData")
load("data/occ_status_final.RData")
load("data/occ_cleaned_slim.RData")
world = map_data("world")
pac_mask = st_read("data/pacific/pac_final.shp")
inv_specs= read.csv("data/Pacific_Invaders_GIFT_22_01.csv", sep = ";") %>% 
  pull(Species) %>% 
  unique()

occ = occ_cleaned_slim %>% 
  dplyr::select(-dataset, -native) %>% 
  left_join(occ_status_final, by = "occ_id") %>% 
  mutate(status = replace_na(status, "unknown"))
rm(occ_status_final, occ_cleaned_slim)

# Frequency of status assignments
table(occ$status) # hmm...so many naturalized? -> GIFT data accurate?

# Species with most naturalized occurrences
freq_not_native = occ %>% filter(!status %in% c("native", "unknown")) %>% group_by(species) %>% tally() %>% arrange(desc(n))
freq_non_native = occ %>% filter(status == "non-native") %>% group_by(species) %>% tally() %>% arrange(desc(n))
freq_naturalized = occ %>% filter(status == "naturalized") %>% group_by(species) %>% tally() %>% arrange(desc(n))
freq_invasive = occ %>% filter(status == "invasive") %>% group_by(species) %>% tally() %>% arrange(desc(n))

# Plot Overview of occurrence dataset
occ_overview_maps = ggplot(occ, aes(x = lon, y = lat)) +
  geom_map(data = world, map = world, aes(map_id = region), inherit.aes = F) +
  geom_hex(bins = 100) +
  ylim(-90,90) +
  scale_fill_continuous(type = "viridis", trans = "log10") +
  facet_wrap(vars(status)) +
  coord_fixed() +
  theme_bw() 
ggsave(filename = "plots/occ_overview.png", occ_overview_maps, device = "png", width = unit(12, "cm"), height = unit(6, "cm"))

# Plot a single species
plot_status_spec = function(species_name, status = c("native", "naturalized", "invasive")){
  df_plot = dplyr::filter(occ, species == species_name & status %in% status)
  if(nrow(df_plot) == 0){
    return()
  } else {
    ggplot(df_plot, aes(x = lon, y = lat, color = status)) +
      geom_map(data = world, map = world, aes(map_id = region), fill = "grey80", color = "grey80", inherit.aes = F) +
      geom_point(shape = 1, alpha = 1/log10(nrow(df_plot))) +
      scale_color_manual(values = c(native = "#038cfc", "non-native" = "#ffff52", naturalized = "#ffc252", invasive = "#ff5e52", unknown = "black")) +
      ggtitle(species_name) +
      ylim(-60,80) +
      xlim(-180, 180) +
      coord_fixed() +
      guides(colour = guide_legend(override.aes = list(alpha = 1))) +
      theme_bw()
  }
}

# Look at random species
plot_status_spec(sample(inv_specs, 1))

# Suspect status information:
ggsave("plots/status_suspect/RapRap.png", plot_status_spec("Raphanus raphanistrum"), scale = 1.5)
ggsave("plots/status_suspect/ArrEla.png", plot_status_spec("Arrhenatherum elatius"), scale = 1.5)
ggsave("plots/status_suspect/MurPan.png", plot_status_spec("Murraya paniculata"), scale = 1.5)
ggsave("plots/status_suspect/MatInc.png", plot_status_spec("Matthiola incana"), scale = 1.5)

# Plausible status information
ggsave("plots/status_plausible/RivHum.png", plot_status_spec("Rivina humilis"), scale = 1.5)
ggsave("plots/status_plausible/OpuFic.png", plot_status_spec("Opuntia ficus-indica"), scale = 1.5)
ggsave("plots/status_plausible/HypGla.png", plot_status_spec("Hypochaeris glabra"), scale = 1.5)
ggsave("plots/status_plausible/StaJam.png", plot_status_spec("Stachytarpheta jamaicensis"), scale = 1.5)
ggsave("plots/status_plausible/IriDom.png", plot_status_spec("Iris domestica"), scale = 1.5)
ggsave("plots/status_plausible/CecObt.png", plot_status_spec("Cecropia obtusiafolia"), scale = 1.5)

########## Plotting ############
# # 3. Occurrences in the pacific
occ_pac_sf = occ %>% # This requires a lot of RAM!
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_join(pac_mask, left = F)
save(occ_pac_sf, file = "data/occ_pac_sf.RData")

freq_pac_not_native = occ_pac_sf %>% st_drop_geometry() %>% filter(!status %in% c("native", "unknown")) %>% group_by(species) %>% tally() %>% arrange(desc(n))
freq_pac_non_native = occ_pac_sf %>% st_drop_geometry() %>% filter(status == "non-native") %>% group_by(species) %>% tally() %>% arrange(desc(n))
freq_pac_naturalized = occ_pac_sf %>% st_drop_geometry() %>% filter(status == "naturalized") %>% group_by(species) %>% tally() %>% arrange(desc(n))
freq_pac_invasive = occ_pac_sf %>% st_drop_geometry() %>% filter(status == "invasive") %>% group_by(species) %>% tally() %>% arrange(desc(n))

occ_pac = occ_pac_sf %>% 
  dplyr::mutate(lat = sf::st_coordinates(.)[,2], lon = sf::st_coordinates(.)[,1]) %>% 
  st_drop_geometry() %>% 
  select(-FID)

plot_status_bbox = function(xmin, xmax, ymin, ymax, alpha = 1, status = c("native", "non-native", "naturalized", "invasive"), title = ""){
  st = status
  ggplot(filter(occ_pac, status %in% st), aes(x = lon, y = lat, color = status)) +
    geom_map(data = world, map = world, aes(map_id = region), fill = "grey80", color = "grey80", inherit.aes = F) +
    geom_point(shape = 1, alpha = alpha) +
    scale_color_manual(values = c(native = "#038cfc", "non-native" = "#ffff52", naturalized = "#ffc252", invasive = "#ff5e52", unknown = "black")) +
    ylim(ymin, ymax) +
    xlim(xmin,xmax) +
    ggtitle(title) +
    coord_fixed() +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme_bw()
}

plot_status_bbox(-161, -154, 18.5, 22.5, title = "Hawaii") # Hawaii
plot_status_bbox(-92, -89, -1.5, 1, title = "Galapagos") # Galapagos
plot_status_bbox(176.5, 180, -19.5, -16, title = "Fiji") # Fiji
plot_status_bbox(-173, -169, -14.5, -13, title = "Samoa") # Samoa
plot_status_bbox(166.25,170,-20.35,-13.5, title = "Vanuatu") # Vanuatu
plot_status_bbox(154.5,162.5,-11,-5, title = "Solomons") # Solomons
plot_status_bbox(-157.6,-157,1.6, 2.1, title = "Kiribati") # Kiribati
