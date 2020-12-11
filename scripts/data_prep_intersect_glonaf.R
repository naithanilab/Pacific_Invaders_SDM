# /////////////////////////////////////////////////////////////////////////
#
# Intersect occurrence data with the GLONAF shapefile.
#
# This spatial intersection allows the retrieval of the geoentity ID-s for each
# occurrence. That is useful because these geoentity ID-s can be further used as
# keys to retrieve alien status information. 

# Note that the spatial intersection is an expensive operation (both RAM and CPU
# time).
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


# Load and/or install packages.
source("sdm/scripts/helper_functions/load_packages.R")

# Load helper functions for:
# - running spatial intersection in parallel.
source("sdm/scripts/helper_functions/over_parallel.R")


# Read data ---------------------------------------------------------------

# Read cleaned occurrence data
dt_oc <- readRDS(file = paste0(save_path, "occ_clean.rds"))
# ~ 25 sec

# Read geoentities shapefile from Patrick Weigelt
g_entt <- readRDS(file = "sdm/data/geoentities_2018-05-09/rds/geoentities_2018-05-09.rds")
# ~ 5 sec


# Intersect with GLONAF ---------------------------------------------------

# Workflow:

# First get the geoentity polygon indices. Then, because an occurrence can
# intersect several overlapping polygons, select the polygon with the smallest
# area. Only then select the corresponding entt_ID from the shapefile’s data
# (attribute table).

# Speeds up to at least 4-5 fold if using unique pairs of coordinates.
unq_xy <- unique(dt_oc[,.(x, y)])

# Get the geonetity polygon indices (not entt_ID yet) intersected by each unique
# pair of coordinates. Because there are overlapping polygons, use returnList =
# TRUE to get all intersected polygons. Will return a list of lists. Note that,
# the length of the main ids list is equal with the number of cores used.
system.time({
  ids <- over_parallel(xy = unq_xy, ref = g_entt, returnList = TRUE, cpu_frac = .5)
})
# ~ 2h on 6 cores
# Save/load intermediary results:
# saveRDS(object = ids, file = paste0(save_path, "geo_entt_ids.rds"))
# ids <- readRDS(file = paste0(save_path, "geo_entt_ids.rds"))

ids <- unlist(ids, recursive = FALSE)
# The recursive = FALSE will not drop the integer(0) cases, that is, the cases
# when a point does not intersect any polygon. They must be kept in, otherwise
# we lose the link to the row indices in unq_xy.

# Points that do not intersect polygons:
test <- sapply(ids, function(x) length(x) == 0)
ids_water <- which(test)
sum(test) # 76398 unique pairs of coordinates have no geoentity ID

# For each unique pair of coordinates, get the entt_ID of the smallest polygon:
areas <- g_entt@data$area
ids_i_min <- lapply(ids, function(x) which.min(areas[x]))
ids_min_area <- Map('[', ids, ids_i_min)
unq_xy[, entt_ID := g_entt@data$entt_ID[as.integer(ids_min_area)]]

# Join back to the main occurrence table the extracted entt_ID.
dt_oc <- merge(dt_oc, unq_xy, by = c("x", "y"))

# Clean environment.
rm(unq_xy, ids_min_area, ids_i_min, areas, test, ids_water, ids); gc()

# How many occurrences do not intersect the geoentity dataset?
dt_oc[is.na(entt_ID), .N] # 317670
# Do we lose any species because of this spatial issue?
setdiff(dt_oc[is.na(entt_ID), spp_binary],
        dt_oc[!is.na(entt_ID), spp_binary])
# [1] "Portulaca lutea"

saveRDS(object = dt_oc, file = paste0(save_path, "occ_clean_glonaf.rds"))
