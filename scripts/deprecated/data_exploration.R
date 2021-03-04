library(tidyverse)
library(bRacatus)
library(CoordinateCleaner)
library(ggmap)

occ = readRDS("output/data-clean/occurrence_clean_with_alien_status.rds")
tmp = head(occ)
table(occ$inva_stat, useNA = "ifany")

bRacatus::signalCalculation()

dt_cleaned = clean_coordinates(occ[1:1000,], lon = "long", lat = "lat", species = "spp_binary")

# that Windows does not interpret properly).
bot_inst <- read_delim("data/botanical_gardens/BGCI_garden_coordinates.csv", delim = "\t") %>% 
  mutate(Longitude = as.numeric(Longitude))

tmp = ggplot(map_data("world"), aes(x=long, y = lat, group = group)) +
  geom_point(data = occ, aes(x = long, y = lat), size = 0.2, inherit.aes = F) +
  geom_point(data = bot_inst, aes(x = Longitude, y = Latitude), size = 0.2, col = "red", inherit.aes = F ) +
  xlim(19.5, 15) + 
  ylim(46, 56)


ggsave(tmp, filename = "~/Pictures/tmp.png", device = "png")
