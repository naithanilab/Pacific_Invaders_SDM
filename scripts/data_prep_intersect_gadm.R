# /////////////////////////////////////////////////////////////////////////
#
# Intersect occurrence data with the GADM shapefile (www.gadm.org).
#
# The intersection with GADM allows to flag occurrences with coordinates outside
# their reported country or with non-terrestrial coordinates (in water bodies).
# Instead of GADM, could use the Natural Earth datasets, but they are less
# accurate (also our focus is on islands, so we need accurate coastlines).
#
# All in all, what this script does is to spatially intersect occurrence data
# with the GADM world shapefile and extract country ISO and name. Then saves the
# occurrence table as RDS binary object for further cleaning with other scripts.
# Warning: The RDS object will take only ~90 Mb of space, but once loaded it
# consumes ~3 Gb of RAM.
#
# Note that the spatial intersection is an expensive operation (both RAM and CPU
# time) because of the size of data and GADM shapefile. Therefore, can only be
# carried in parallel on a cluster with at least 200 Gb RAM.
#
# Activate the document outline (CTRL + SHIFT + O) to see the main steps.
#
# /////////////////////////////////////////////////////////////////////////


# Prepare working environment ---------------------------------------------

# Clean environment, including hidden objects (which begin with a .) & release
# RAM to the Operating System if applicable.
rm(list = ls(all.names = TRUE)); gc()

# When working on the RStudio server, switches to the "I" drive path. Note that,
# when running the RStudio proj file from "I" drive directly, the following line
# do not alter the working directory path.
if ( ! grepl(pattern = "PacificInvaders", x = getwd()) )
  setwd("/data/sie-group-share/_data_VS/_Proj_share/PacificInvaders")

# Create path for saving output. "~/winhome/temp/" is for RStudio server.
save_path <- 
  if (grepl("sie-group-share", getwd())) "~/winhome/temp/" else "sdm/output/data-clean/temp/"
# Create also the temp directory.
if (! dir.exists(file.path(save_path))) dir.create(file.path(save_path))


# Load and/or install packages.
source("sdm/scripts/helper_functions/load_packages.R")

# Load helper functions for:
# - running spatial intersection in parallel.
source("sdm/scripts/helper_functions/over_parallel.R")
# - geodesic buffer
source("sdm/scripts/helper_functions/make_geodesic_buffer.R")
# - closest polygon within buffer distance
source("sdm/scripts/helper_functions/closest_within_dist.R")


# Read data ---------------------------------------------------------------

load(file = "sdm/data/PacAlienSpp_GBIFBIEN_occurrences_Aug2018.RData")
setDT(gbif_bien_occ)
gadm <- readRDS(file = "sdm/data/gadm/rds/gadm36_0.rds")


# Are coord within expected range? ----------------------------------------

# Are there any NA-s in the coordinates?
gbif_bien_occ[is.na(decimalLongitude) | is.na(decimalLatitude), .N] # 0

# Check if the coordinates are within the expected ranges. Note that, range
# alone does not guarantee 100% that things are correct. For example, longitudes
# in the range of -90, 90 can easily pass as latitudes.
gbif_bien_occ[, range(decimalLongitude) %between% c(-180, 180)] # TRUE TRUE
gbif_bien_occ[, range(decimalLongitude)] # [1] -179.9700  179.9838
gbif_bien_occ[, range(decimalLatitude) %between% c(-90, 90)]    # TRUE TRUE
gbif_bien_occ[, range(decimalLatitude)] # [1] -89.9994  90.0000


# Intersect with GADM -----------------------------------------------------

# Setting keys on a data.table improves speed down the road.
keys <- c("decimalLongitude", "decimalLatitude")
setkeyv(gbif_bien_occ, keys) 

# Speeds up to at least 10 fold if using unique pairs of coordinates.
# "..keys" is a data.table syntax shortcut for selecting the columns in keys.
# See more in ?data.table
unq_xy <- unique(gbif_bien_occ[, ..keys])

# Get the GADM polygon indices intersected by each unique pair of coordinates.
# Is tricky to set it as array job because it consumes more than available RAM
# per core since the GADM object is big and also it seems that unnecessary
# copies of it are created on the fly within a single run (core). Worth
# exploring though, but seems to take up 23-24 Gb RAM per core when 3*10^5
# points are intersected with GADM in a run.
ids <- over_parallel(xy = unq_xy, ref = gadm, cpu_frac = .5)
# 2.7 hours on RStudio Server, 10 cores, ~230 Gb RAM.

# Save/load intermediary results:
# saveRDS(object = ids, file = paste0(save_path, "gadm_ids.rds"))
# ids <- readRDS(file = paste0(save_path, "gadm_ids.rds"))

# Make vector from list of vectors
ids <- unlist(ids, use.names = FALSE)
length(ids) == dim(unq_xy)[1] # must be TRUE

# Join the intersected polygon attributes/data to each unique pair of coordinates.
unq_xy[, ':=' (gadm_iso3 = gadm@data[ids, "GID_0"],
               gadm_name = gadm@data[ids, "NAME_0"])]


# Salvage some non-terrestrial occurrences --------------------------------

# After the spatial intersection, the points without country information indicate
# that they fall in non-terrestrial areas (water bodies). Some of them fall
# within couple of km from the coast though, so will try to salvage those within
# 1km from the coastlines.

# Get all non-terrestrial occurrences (unique pair of coordinates).
unq_xy_na <- unq_xy[is.na(gadm_iso3), ..keys]

# Get the GADM polygon indices that are the closest to occurrences within 1 km
# from coastlines.
ids_water <- closest_within_dist(xy = unq_xy_na, ref = gadm, dist = 1000)
# Elapsed CPU time: 17.3 mins (single core)
# saveRDS(object = ids_water, file = paste0(save_path, "gadm_ids_water.rds"))
# ids_water <- readRDS(file = paste0(save_path, "gadm_ids_water.rds"))

# Get the GADM polygon data of the retrieved indices. Where data is still NA, it
# means that those occurrences are farther than 1 km in the oceans.
unq_xy_na[, ':=' (gadm_iso3_near = gadm@data[ids_water, "GID_0"],
                  gadm_name_near = gadm@data[ids_water, "NAME_0"],
                  gadm_in_water = TRUE)] # mark them all as non-terrestrial

# Update by reference the unique pairs of coordinates with the new information.
unq_xy[unq_xy_na, on = keys, 
       ':=' (gadm_iso3 = gadm_iso3_near,
             gadm_name = gadm_name_near,
             gadm_in_water = gadm_in_water)]

# OBS: if there is a floating point precision problem, then the next two counts
# of NA are different. I have set setNumericRounding(2) in load_packages.R,
# which should take care of such an issue (see comments there).
unq_xy_na[is.na(gadm_iso3_near), .N] # 20081 unique pairs outside of the 1 km buffer
unq_xy[is.na(gadm_iso3), .N]         # 20081


# Join GADM data to occurrence data ---------------------------------------

# Join back to the raw occurrence table the GADM data extracted for each unique
# pair of coordinates. This will add two new columns: gadm_iso3 & gadm_name
gbif_bien_occ <- merge(gbif_bien_occ, unq_xy, by = keys)

# Mark the non-terrestrial (in water = TRUE) vs terrestrial (FALSE) occurrences.
# Note that, the cases where gadm_in_water == TRUE and have a country ISO code
# (gadm_iso3) or gadm_name, are within the 1 km buffer zone from coastlines.
# Those where gadm_iso3 or gadm_name is NA, indicate that they are outside the
# buffer zone and will be dropped in the cleaning process.
gbif_bien_occ[is.na(gadm_in_water), gadm_in_water := FALSE]

# Example:
gbif_bien_occ[is.na(gadm_iso3), .N]
# 753027 occurrences outside of the 1 km buffer
gbif_bien_occ[gadm_in_water == TRUE & !is.na(gadm_iso3), .N]
# 476284 occurrences within the 1 km buffer

# Save the occurrence table updated with GADM country ISO and name. The RDS
# object will take only approx 90 Mb of space.
saveRDS(object = gbif_bien_occ, file = paste0(save_path, "occ_gadm.rds"))

# Save the species list separately as well as it is a bit more convenient.
saveRDS(object = spp_list, file = paste0(save_path, "spp_list.rds"))
