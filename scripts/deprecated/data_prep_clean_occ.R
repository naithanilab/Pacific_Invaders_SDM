# /////////////////////////////////////////////////////////////////////////
#
# Clean occurrence dataset. Activate the document outline (CTRL + SHIFT + O) to
# see the main and intermediary steps.
#
# Runs for 12-13 min.
#
# /////////////////////////////////////////////////////////////////////////


# measure CPU time when running the script in one go
start_time <- Sys.time()


# Prepare working environment ---------------------------------------------

rm(list = ls(all.names = TRUE)); gc()

# Create path for saving output.
save_path <- "output/data-clean/temp/"


# Load and/or install packages.
source("scripts/helper_functions/load_packages.R")


# Read data ---------------------------------------------------------------

dt_oc_raw <- readRDS(file = "output/data-clean/temp/occ_gadm.rds")
# 1.5-2 minutes

# Take a copy of the occurences object for testing reasons (do this only if you
# have RAM to spare).
dt_oc <- copy(dt_oc_raw)
# rm(dt_oc_raw); gc() # otherwise remove it

# For tracking species loss due to cleaning. sp_rest will update constantly with
# remaining species after each test.
dt_oc[, spp_binary := as.character(spp_binary)]
sp_orig <- sp_rest <- dt_oc[, sort(unique(spp_binary))]

# For tracking records loss due to cleaning. n_rest will update constantly
n_orig <- n_rest <- dim(dt_oc)[1]


# Country codes & names ---------------------------------------------------

# ~ Non-terrestrial -------------------------------------------------------

# Occurrences outside of the 1 km buffer zone from coastlines:
dt_oc[is.na(gadm_iso3), .N] # 753027
test <- dt_oc[is.na(gadm_iso3), .N, by = .(Country_name_iso2, Country_name)][order(-N)]
test_sp <- dt_oc[is.na(gadm_iso3), .N, by = spp_binary][order(-N)]
# Drop all these cases. Further investigations can try to enlarge the buffer
# zone from the coastlines. Worth asking – could GADM have also missing polygons
# for some small islands/island parts? How to actually find that? Perhaps check
# occurrences against both Natural Earth and GADM?
dt_oc <- dt_oc[!is.na(gadm_iso3)]
n_rest - dim(dt_oc)[1L]; n_rest <- dim(dt_oc)[1L] # 753027 deleted

# Are species lost?
sp_lost <- sp_rest[! sp_rest %in% unique(dt_oc$spp_binary)]; sp_lost
# [1] "Alocasia sanderiana" "Nesogenes rotensis"
sp_rest <- sp_rest[! sp_rest %in% sp_lost]

rm(test, test_sp); gc()


# ~ ISO2 vs. ISO3 issues --------------------------------------------------

# Convert country code from ISO2c to ISO3c & check if reported values are ok.
dt_oc[, countrycode_iso3 := countrycode(sourcevar = Country_name_iso2,
                                        origin =  'iso2c', 
                                        destination = 'iso3c')]
dt_oc[Country_name_iso3 != countrycode_iso3, .N] # 0 - all ok
dt_oc[, countrycode_iso3 := NULL]

# However, there are issues with some cases where ISO2 is XK, ZZ
dt_oc[Country_name_iso2 %in% c("XK", "ZZ"), .N,
      by = .(Country_name_iso2, Country_name, Country_name_iso3, gadm_iso3, gadm_name)][order(-N)]
# I think XK (Kosovo) are legit cases. I cannot vouch for the ZZ cases though
# (unknown); they need to be checked manually. But even then, how could we know
# for sure that they are not just random coordinates?

# Fix the Kosovo & Serbia cases.
dt_oc[Country_name_iso2 == "XK", unique(gadm_iso3)]
dt_oc[Country_name_iso2 == "XK", ':=' (Country_name_iso3 = gadm_iso3,
                                       Country_name = gadm_name)]


# ~ Missing country info --------------------------------------------------

# Delete records without declared country name or ISO3.
dt_oc[is.na(Country_name_iso3) | is.na(Country_name), .N, 
      by = .(Country_name_iso3, Country_name)]
# All these cases need to be dropped.
dt_oc <- dt_oc[!is.na(Country_name_iso3)]
n_rest - dim(dt_oc)[1L]; n_rest <- dim(dt_oc)[1L] # 1412 deleted

# Are species lost?
sp_lost <- sp_rest[! sp_rest %in% unique(dt_oc$spp_binary)]; sp_lost # no
sp_rest <- sp_rest[! sp_rest %in% sp_lost]


# ~ Reported vs. GADM ISO3 ------------------------------------------------

# A table of counts of cases where the reported ISO3 differs from the GADM one.
test_iso3 <- dt_oc[Country_name_iso3 != gadm_iso3, .N, 
      by = .(Country_name, Country_name_iso3, gadm_iso3, gadm_name)][order(-N)]
# Note that, many cases where the GADM ISO3 doesn’t match the reported ISO3
# happen at the border between neighboring countries, or where there are some
# administrative complications.

# Some obvious cases are:
# | Wiki Name             | Declared ISO3 | GADM ISO3  |
# |:----------------------|:------------- |:-----------|
# | Åland Islands         | FIN           | ALA        |
# | Isle of Man           | GBR           | IMN        |
# | Northern Cyprus       | CYP           | XNC        |
test_iso3[gadm_iso3 %in% c("ALA", "IMN", "XNC")]

# Fix those cases so that they do not get deleted. Use a lookup table and update
# by reference (see ?data.table, about "on").
iso_tbl <- data.table(iso3_gadm = c("ALA", "IMN", "XNC"),
                      iso3_decl = c("FIN", "GBR", "CYP"))
dt_oc[iso_tbl, on = c(gadm_iso3 = "iso3_gadm", 
                      Country_name_iso3 = "iso3_decl"),
      ':=' (Country_name_iso3 = gadm_iso3,
            Country_name = gadm_name)]

# Now delete all mismatches (keep only the matching iso3 cases)
dt_oc <- dt_oc[Country_name_iso3 == gadm_iso3]
n_rest - dim(dt_oc)[1L]; n_rest <- dim(dt_oc)[1L] # 204728 deleted

# Are species lost?
sp_lost <- sp_rest[! sp_rest %in% unique(dt_oc$spp_binary)]; sp_lost # no
sp_rest <- sp_rest[! sp_rest %in% sp_lost]


# Equal coord -------------------------------------------------------------

dt_oc[decimalLongitude == decimalLatitude, .N]           # 158
dt_oc[abs(decimalLongitude) == abs(decimalLatitude), .N] # 506
dt_oc[decimalLongitude == 0 & decimalLatitude == 0, .N]  # 0


# Plot data to get an overview
wm <- borders("world", colour = "gray50", fill = "black")
ggplot() + coord_fixed() + wm +
  geom_point(data = unique(dt_oc[abs(decimalLongitude) == abs(decimalLatitude)],
                           by = c("decimalLongitude", "decimalLatitude")),
             aes(x = decimalLongitude, y = decimalLatitude), 
             colour = "red", alpha = 0.2)
ggsave(filename = paste0(save_path, "plot-occ-equal-coord.png"),
       width = 15, height = 10, dpi = 300)

# I think these are legitimate cases since the declared country and the GADM
# confirm the same ISO3.


# Coordinate uncertainty --------------------------------------------------

hist(dt_oc[, coordinateUncertaintyInMeters/1000], breaks = 100, 
     xlab = "Uncertainty (km)")
range(dt_oc[, coordinateUncertaintyInMeters/1000], na.rm = TRUE)

# Check coordinate uncertainty
dt_oc[coordinateUncertaintyInMeters > 10^3, .N]   # 12097233 with >   1 km uncertainty
dt_oc[coordinateUncertaintyInMeters > 10^4, .N]   #   207214 with >  10 km uncertainty
dt_oc[coordinateUncertaintyInMeters > 10^5/2, .N] #    17205 with >  50 km uncertainty
dt_oc[coordinateUncertaintyInMeters > 10^5, .N]   #     6231 with > 100 km uncertainty

# Delete locations with high uncertainty (keep those with uncertainty <= 50 km)
dt_oc <- dt_oc[coordinateUncertaintyInMeters <= 10^4 | is.na(coordinateUncertaintyInMeters)]
n_rest - dim(dt_oc)[1L]; n_rest <- dim(dt_oc)[1L] # 207214 deleted
# Are species lost?
sp_lost <- sp_rest[! sp_rest %in% unique(dt_oc$spp_binary)]; sp_lost
# [1] "Zephyranthes minuta"
sp_rest <- sp_rest[! sp_rest %in% sp_lost]


# Round coords ------------------------------------------------------------

# Rounding coordinates to 4th decimal - this corresponds to a precision of 4-11 m; 
# Usual commercial GPS can be trusted up to 4th decimal e.g.:
# https://gis.stackexchange.com/questions/8650/measuring-accuracy-of-latitude-and-longitude
# NOTE - this can be considered as a first spatial thinning step.
dt_oc[, x := round(decimalLongitude, digits = 4)]
dt_oc[, y := round(decimalLatitude, digits = 4)]
setkey(dt_oc, x, y)

# How many overlaps are created by rounding (in unique coords)?
dim(unique(dt_oc, by = c("decimalLongitude", "decimalLatitude")))[1] -
  dim(unique(dt_oc, by = c("x", "y")))[1] 
# 2919644 - 2742904 = 176740
dt_oc[, c("decimalLongitude", "decimalLatitude") := NULL]


# Old records -------------------------------------------------------------

# How many records from before 1945?
dt_oc[Year <= 1945, .N] # 1122169

# Check them on map - they seem to be all over the world. Is that an indication
# that they should not be deleted?
ggplot() + coord_fixed() + wm +
  geom_point(data = unique(dt_oc[Year <= 1945], by = c("x", "y")),
             aes(x = x, y = y), colour = "red", size = 0.5, alpha = 0.2)
ggsave(filename = paste0(save_path, "plot-occ-older-than-1945.png"),
       width = 15, height = 10, dpi = 300)

# Delete them
dt_oc <- dt_oc[Year > 1945 | is.na(Year)]
n_rest - dim(dt_oc)[1L]; n_rest <- dim(dt_oc)[1L] # 1122169 deleted

# Are species lost?
sp_lost <- sp_rest[! sp_rest %in% unique(dt_oc$spp_binary)]; sp_lost
sp_rest <- sp_rest[! sp_rest %in% sp_lost]
# "Blumea sessiliflora"


# Coordinates in vicinity of country and province centroids ---------------

# Delete occurrences in vicinity of country and province centroids. Use a buffer
# of 10^3 m. Speeds up to 10 fold if using unique pairs of coordinates.
unq_xy <- unique(dt_oc[, .(x, y)])
idx_unq <- cc_cen(x = unq_xy, lon = "x", lat = "y", buffer = 10^3, value = "flagged")
# ~ 2-3 min; Flagged 2617 records.
# From the unique pairs of coordinates, remove those within 1 km of country
# centroids, then apply a inner join with the big table so that the deletion
# has effect there as well.
unq_xy_ok <- unq_xy[idx_unq]
# data.table inner join when both tables have the same keys (here "x" "y")
dt_oc <- dt_oc[unq_xy_ok]

n_rest - dim(dt_oc)[1L]; n_rest <- dim(dt_oc)[1L] # 40424 deleted

# Overview map of such cases
ggplot() + coord_fixed() + wm +
  geom_point(data = unq_xy[!idx_unq], aes(x = x, y = y), 
             colour = "red", size = 0.5)
ggsave(filename = paste0(save_path, "plot-occ-close-to-country-province-centr.png"),
       width = 15, height = 10, dpi = 300)

rm(unq_xy, idx_unq, unq_xy_ok)

# Are species lost?
sp_lost <- sp_rest[! sp_rest %in% unique(dt_oc$spp_binary)]; sp_lost
sp_rest <- sp_rest[! sp_rest %in% sp_lost]
# "Homalomena rubescens"


# Coordinates in vicinity of country capitals -----------------------------

# Delete occurrences in vicinity of country capitals. Use a buffer of 10^4 m.
# Speeds up to 10 fold if using unique pairs of coordinates.
unq_xy <- unique(dt_oc[, .(x, y)])
idx_unq <- cc_cap(x = unq_xy, lon = "x", lat = "y", buffer = 10^4, value = "flagged")
# ~ 20 sec; Flagged 30918 records.
unq_xy_ok <- unq_xy[idx_unq]
dt_oc <- dt_oc[unq_xy_ok]
n_rest - dim(dt_oc)[1L]; n_rest <- dim(dt_oc)[1L] # 521158 deleted

# Overview map of such cases
ggplot() + coord_fixed() + wm +
  geom_point(data = unq_xy[!idx_unq], aes(x = x, y = y), 
             colour = "red", size = 0.5)
ggsave(filename = paste0(save_path, "plot-occ-close-to-country-capitals.png"),
       width = 15, height = 10, dpi = 300)

rm(unq_xy, idx_unq, unq_xy_ok)

# Are species lost?
sp_lost <- sp_rest[! sp_rest %in% unique(dt_oc$spp_binary)]; sp_lost
sp_rest <- sp_rest[! sp_rest %in% sp_lost]
# "Endospermum macrophyllum"   "Spathoglottis micronesiaca"


# Records in the vicinity of biodiversity institutions --------------------

# ~ using BGCI ------------------------------------------------------------

# Botanic Gardens Conservation International (BGCI)
bgci_sp <- readRDS(file = "data/botanical_gardens/buffer/rds/botanical_gardens_buffers.rds")
source("scripts/helper_functions/remove_occ_gardens.R")
dt_oc <- remove_occ_gardens(dt = dt_oc, gardens = bgci_sp)
# 3255972 points inside botanical gardens were discarded
# Elapsed CPU time: 1-2 min
n_rest - dim(dt_oc)[1L]; n_rest <- dim(dt_oc)[1L] # 3255972 deleted

# Are species lost?
sp_lost <- sp_rest[! sp_rest %in% unique(dt_oc$spp_binary)]
sp_rest <- sp_rest[! sp_rest %in% sp_lost]
length(sp_lost) # 9
cat(sp_lost, sep = "\n")
# Cassia roxburghii
# Crinum bakeri
# Dichorisandra reginae
# Ficus macrophylla
# Fitchia speciosa
# Hippeastrum johnsonii
# Meterostachys sikokianus
# Platycerium grande
# Vanda tricolor

rm(remove_occ_gardens, bgci_sp, sp_lost)


# ~ using CoordinateCleaner ref -------------------------------------------

# Keys need to be rest after using remove_occ_gardens() function. Need to fix
# that later.
setkey(dt_oc, x, y)

unq_xy <- unique(dt_oc[, .(x, y)])
idx_unq <- cc_inst(x = unq_xy, lon = "x", lat = "y", buffer = 10^4, value = "flagged")
# ~ 3 min; Flagged 144859 records.
unq_xy_ok <- unq_xy[idx_unq]
dt_oc <- dt_oc[unq_xy_ok]

n_rest - dim(dt_oc)[1L]; n_rest <- dim(dt_oc)[1L] # 1922487 deleted

# Overview map of such cases
ggplot() + coord_fixed() + wm +
  geom_point(data = unq_xy[!idx_unq], aes(x = x, y = y), 
             colour = "red", size = 0.5)
ggsave(filename = paste0(save_path, "plot-occ-close-to-biodiversity-institutions-cc_inst.png"),
       width = 15, height = 10, dpi = 300)

rm(unq_xy, idx_unq, unq_xy_ok)

# Are species lost?
sp_lost <- sp_rest[! sp_rest %in% unique(dt_oc$spp_binary)]; sp_lost
sp_rest <- sp_rest[! sp_rest %in% sp_lost]
# "Amherstia nobilis"  "Peperomia caperata"

# Total removed records so far from raw data
n_orig - n_rest # 8028591


# Removes duplicated records ----------------------------------------------

# First select the unique records by coordinates, species and coordinate
# uncertainty. Further, when there are multiple records of the same species in
# the same location, then select that one that has the lowest coordinate
# uncertainty. Syntax note: because "coordinateUncertaintyInMeters" can be NA,
# then which.min(NA) returns integer(0) and then the row is not selected,
# therefore the trick with min(which.min(NA), 1), which returns the first (and
# the single) row in such situations, avoiding its deletion.

cols_by <- c("x", "y", "spp_binary", "coordinateUncertaintyInMeters")
cols_by2 <- cols_by[-4]
z <- unique(x = dt_oc, by = cols_by)
# Inspired from: https://stackoverflow.com/a/24558696/5193830
dt_oc <- z[z[, .I[min(which.min(coordinateUncertaintyInMeters), 1)], by = cols_by2]$V1]
# ~ 30 sec

rm(z, cols_by, cols_by2); gc()

n_rest - dim(dt_oc)[1L]; n_rest <- dim(dt_oc)[1L] # 16922206 deleted duplicates
# Total removed records from raw data
n_orig - n_rest # 24950797

# Total species lost
length(setdiff(sp_orig, sp_rest)) # 18
cat(setdiff(sp_orig, sp_rest), sep = "\n")
# Alocasia sanderiana
# Amherstia nobilis
# Blumea sessiliflora
# Cassia roxburghii
# Crinum bakeri
# Dichorisandra reginae
# Endospermum macrophyllum
# Ficus macrophylla
# Fitchia speciosa
# Hippeastrum johnsonii
# Homalomena rubescens
# Meterostachys sikokianus
# Nesogenes rotensis
# Peperomia caperata
# Platycerium grande
# Spathoglottis micronesiaca
# Vanda tricolor
# Zephyranthes minuta


# Save temporary cleaned data ---------------------------------------------

# Save as binary rds object.
saveRDS(object = dt_oc, file = paste0(save_path, "occ_clean.rds"))


# Elapsed CPU time:
round(Sys.time() - start_time, digits = 1)
# ~ 12-13 min
