library(tidyverse)
library(sf)
library(maps)

rm(list = ls())
#setwd("~/ownCloud/Projects/Berlin/10_Pacific_invaders")
setwd("~/PacificInvadersSDM/")

########## Plotting ##########
load("/./import/calc9z/data-zurell/koenig/occ_status_final.RData")
load("/./import/calc9z/data-zurell/koenig/occ_cleaned_slim.RData")
inv_specs= read.csv("data/Pacific_Invaders_GIFT_22_01.csv", sep = ";") %>% 
  pull(Species) %>% 
  unique()

occ = occ_cleaned_slim %>% 
  dplyr::select(-dataset, -native) %>% 
  left_join(occ_status_final, by = "occ_id") %>% 
  mutate(status = replace_na(status, "unknown"))

# Plot Overview of occurrence dataset
world = map_data("world")
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
plot_species = function(species_name, status = c("native", "naturalized", "invasive")){
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
plot_species(sample(inv_specs, 1))

# Suspect status information:
ggsave("plots/status_suspect/RapRap.png", plot_species("Raphanus raphanistrum"), scale = 1.5)
ggsave("plots/status_suspect/ArrEla.png", plot_species("Arrhenatherum elatius"), scale = 1.5)
ggsave("plots/status_suspect/MurPan.png", plot_species("Murraya paniculata"), scale = 1.5)
ggsave("plots/status_suspect/MatInc.png", plot_species("Matthiola incana"), scale = 1.5)

# Plausible status information
ggsave("plots/status_plausible/RivHum.png", plot_species("Rivina humilis"), scale = 1.5)
ggsave("plots/status_plausible/OpuFic.png", plot_species("Opuntia ficus-indica"), scale = 1.5)
ggsave("plots/status_plausible/HypGla.png", plot_species("Hypochaeris glabra"), scale = 1.5)
ggsave("plots/status_plausible/StaJam.png", plot_species("Stachytarpheta jamaicensis"), scale = 1.5)
ggsave("plots/status_plausible/IriDom.png", plot_species("Iris domestica"), scale = 1.5)
ggsave("plots/status_plausible/CecObt.png", plot_species("Cecropia obtusiafolia"), scale = 1.5)

########## Plotting ############
# 1. Frequency of status assignments
table(occ$status) # hmm...so many naturalized? -> GIFT data accurate?

# 2. Species with most naturalized occurrences
freq_nonnative = occ %>% filter(status == "non-native") %>% group_by(species) %>% tally() %>% arrange(desc(n))
freq_naturalized = occ %>% filter(status == "naturalized") %>% group_by(species) %>% tally() %>% arrange(desc(n))

