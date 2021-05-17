library(tidyverse)
library(sf)
library(mapview)
library(sfheaders)

rm(list = ls())
setwd("~/ownCloud/Projects/Berlin/10_Pacific_invaders")

########### Prepare Pacific polygon ##############
# pacific islands
pac_simple = st_read("data/pacific/iho.shp") %>% 
  filter(name %in% c("North Pacific Ocean", "South Pacific Ocean", "Coral Sea",
                     "Philippine Sea", "Tasman Sea", "Solomon Sea", "Bismarck Sea")) %>%
  st_simplify(preserveTopology = T, dTolerance = 0.1) %>%
  st_union()

# continents / world
world_buffered = st_read("data/world_continents/v106/continent.gdb") %>% 
  st_buffer(dist = 0.1) %>% 
  st_union()

plot(st_geometry(pac_simple)) 
plot(st_geometry(world))

# Crop pacific and remove holes
pac_cleaned = pac_simple %>% 
  st_difference(world_buffered) %>% # clip with buffered continents
  sfheaders::sf_remove_holes(close = T) # remove holes

plot(st_geometry(pac_cleaned)) # Still some stray polygons and odd nodes

st_write(pac_cleaned, "data/pacific/pac_cleaned.shp", delete_layer = T) # Still some remaining issues (overlapping island polygons, misplaced vertices) 
# --> fix in QGIS
# save as pac_final.shp