# /////////////////////////////////////////////////////////////////////////
#
# Intersect occurrence data with the GIFT shapefile.
#
# This spatial intersection allows the retrieval of the geoentity ID-s for each
# occurrence. That is useful because these geoentity ID-s can be further used as
# keys to retrieve alien status information. 

# Note that the spatial intersection is an expensive operation (both RAM and CPU
# time). One might need over 100 Gb of RAM.
#
# Activate the document outline (CTRL + SHIFT + O) to see the main steps.
#
# /////////////////////////////////////////////////////////////////////////


# Prepare working environment ---------------------------------------------

rm(list = ls(all.names = TRUE)); gc()

# Create path for saving output.
save_path <- "output/data-clean/temp/"


# Load and/or install packages.
source("scripts/helper_functions/load_packages.R")

# Load helper functions for:
# - running spatial intersection in parallel.
source("scripts/helper_functions/over_parallel.R")


# Read data ---------------------------------------------------------------

# Read cleaned occurrence data
dt_oc <- readRDS(file = paste0(save_path, "occ_clean.rds"))
# ~ 25 sec

# Read geoentities shapefile from Patrick Weigelt
g_entt <- readRDS(file = "data/geoentities_2018-05-09/rds/geoentities_2018-05-09.rds")
# ~ 5 sec


# Intersect with GIFT -----------------------------------------------------

# Workflow:

# First get the geoentity polygon indices. Because an occurrence can intersect
# several overlapping polygons, mark the intersected polygon with the smallest
# area. Select the corresponding entt_ID from the shapefile’s data (attribute
# table).

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

poly_ids <- unlist(ids, recursive = FALSE)
# The recursive = FALSE will not drop the integer(0) cases, that is, the cases
# when a point does not intersect any polygon. They must be kept in, otherwise
# we lose the link to the row indices in unq_xy.

# Points that do not intersect polygons:
test <- sapply(poly_ids, function(x) length(x) == 0)
ids_water <- which(test)
sum(test) # 76398 unique pairs of coordinates have no geoentity ID

# For each unique pair of coordinates, get the entt_ID of the smallest polygon:
areas <- g_entt@data$area
ids_i_min <- lapply(poly_ids, function(x) which.min(areas[x]))
ids_min_area <- Map('[', poly_ids, ids_i_min)
unq_xy[, min_area_entt_ID := g_entt@data$entt_ID[as.integer(ids_min_area)]]

# Get also the GIFT entity names of the smallest polygon.
g_entt_dt <- as.data.table(g_entt@data)
unq_xy[g_entt_dt, on = .(min_area_entt_ID = entt_ID), min_area_entt_ID_name := ge_ntty]

# For each unique pair of coordinates, get all corresponding entt_IDs (all
# intersected GIFT polygons)
entt_ids <- lapply(poly_ids, function(x) g_entt@data$entt_ID[x])
entt_ids_t <- data.table::transpose(entt_ids)
cols <- paste("entt_ID", 1:length(entt_ids_t), sep = "_")
unq_xy[, (cols) := entt_ids_t]

# Join back to the main occurrence table the extracted entt_ID.
dt_oc <- merge(dt_oc, unq_xy, by = c("x", "y"))


# How many occurrences do not intersect the geoentity dataset?
dt_oc[is.na(min_area_entt_ID), .N] # 317670
# Do we lose any species because of this spatial issue?
setdiff(dt_oc[is.na(min_area_entt_ID), spp_binary],
        dt_oc[!is.na(min_area_entt_ID), spp_binary])
# [1] "Portulaca lutea"

saveRDS(object = dt_oc, file = paste0(save_path, "occ_clean_gift.rds"))

write.csv(g_entt_dt, file = "data/geoentities_2018-05-09/geoentities_attribute_table.csv",
          row.names = FALSE)
