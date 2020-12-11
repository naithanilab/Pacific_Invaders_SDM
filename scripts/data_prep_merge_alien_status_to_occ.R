# /////////////////////////////////////////////////////////////////////////
#
# Get alien status for each occurrence.
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
source("sdm/scripts/helper_functions/crop_mapview.R")

# Read data ---------------------------------------------------------------

# Read cleaned occurrence data
dt_oc <- readRDS(file = "sdm/output/data-clean/temp/occ_clean_glonaf.rds")
# ~ 25 sec

dt_pi <- fread("sdm/output/data-clean/temp/pi_alien.csv")

glonaf <- readRDS(file = "sdm/data/geoentities_2018-05-09/rds/geoentities_2018-05-09.rds")
gadm <- readRDS(file = "sdm/data/gadm/rds/gadm36_0.rds")
load(file = "sdm/data/PacAlienSpp_GBIFBIEN_occurrences_Aug2018.RData"); setDT(gbif_bien_occ)


# Some species from the occurrence dataset do not exist in the
# `Pacific_Invaders_0707.csv` (cleaned version). See also:
# https://github.com/idiv-biodiversity/PacificInvaders/issues/7
setdiff(dt_oc$spp_binary, dt_pi$species)

sort(setdiff(dt_oc$entt_ID, dt_pi$entt_id_island))
sort(setdiff(dt_pi$entt_id_island, dt_oc$entt_ID))
sort(intersect(dt_oc$entt_ID, dt_pi$entt_id_island))

is_intrs_id <- glonaf@data$entt_ID %in% intersect(dt_oc$entt_ID, dt_pi$entt_id_island)
test_intrs <- glonaf@data[is_intrs_id, c("entt_ID", "ge_ntty", "entty_c")]
test_intrs[order(test_intrs$ge_ntty),]

not_in_occ <- glonaf@data$entt_ID %in% setdiff(dt_pi$entt_id_island, dt_oc$entt_ID)
test_out <- glonaf@data[not_in_occ, c("entt_ID", "ge_ntty", "entty_c")]
test_out[order(test_out$ge_ntty),]

system.time({
  maps_lst <- vector(mode = "list", length = nrow(test_out))
  for (i in 1:nrow(test_out)){
    tryCatch({
      maps_lst[[i]] <- crop_mapview_id(id = test_out$entt_ID[i],
                                       occ = gbif_bien_occ, 
                                       occ_long = "decimalLongitude", 
                                       occ_lat = "decimalLatitude",
                                       occ_clean = dt_oc, 
                                       occ_clean_long = "x", 
                                       occ_clean_lat = "y",
                                       basemaps = list(glonaf = glonaf, 
                                                       gadm = gadm),
                                       dist = 1) # degrees left & right
    }, error = function(err){
      maps_lst[[i]] <- NA
    })
  }
})
saveRDS(object = maps_lst, file = paste0(save_path, "maps_check_occ.rds"))
# user  system elapsed 
# 5794.81  925.81 6359.87 




dt_pi_unq_sp_isl <- unique(dt_pi[!is.na(entt_id_island)], 
                           by = c("species", "entt_id_island"))
dt_pi_unq_sp_isl[is.na(species), .N] # 0

dt_oc <- merge(x = dt_oc,
               y = dt_pi_unq_sp_isl[, .(species, entt_id_island, alien_island)],
               by.x = c("spp_binary", "entt_ID"),
               by.y = c("species", "entt_id_island"),
               all.x = TRUE)
nrow(dt_oc) - dt_oc[is.na(alien_island), .N]


# dt_oc[dt_status_info, 
#       on = c("spp_binary" = "species", "entt_ID" = "ID_1"),
#       ':=' (entt_island = island,
#             entt_invasive = invasive)]
# 
# setnames(x = dt_oc, 
#          old = c("x", "y", "coordinateUncertaintyInMeters"),
#          new = c("long", "lat", "coord_uncert_m"))
# 
# # Save as binary rds object.
# saveRDS(object = dt_oc, file = paste0(save_path, "occ_clean_glonaf.rds"))
