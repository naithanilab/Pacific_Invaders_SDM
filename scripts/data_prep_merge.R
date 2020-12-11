# /////////////////////////////////////////////////////////////////////////
#
# Get alien status for each cleaned occurrence.
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
  setwd("/data/sie-group-share/10_data_VS/0_proj_share/PacificInvaders")

# Create path for saving output. "~/winhome/temp/" is for RStudio server.
save_path <- 
  if (grepl("sie-group-share", getwd())) "~/winhome/temp/" else "sdm/output/data-clean/"


# Load and/or install packages.
source("sdm/scripts/helper_functions/load_packages.R")

# Read data ---------------------------------------------------------------

# Read cleaned occurrence data
dt_oc <- readRDS(file = "sdm/output/data-clean/temp/occ_clean_glonaf.rds")
# ~ 25 sec

dt_pi <- fread("sdm/output/data-clean/temp/pi_alien.csv")


# Get unique combinations of species and geoentity id
dt_pi_unq_sp_isl <- unique(dt_pi[!is.na(entt_id_island)], 
                           by = c("species", "entt_id_island"))
dt_pi_unq_sp_isl[is.na(species), .N] # 0

dt_oc <- merge(x = dt_oc,
               y = dt_pi_unq_sp_isl[, .(species, entt_id_island, alien_island)],
               by.x = c("spp_binary", "entt_ID"),
               by.y = c("species", "entt_id_island"),
               all.x = TRUE)
# How many records got alien status information?
dt_oc[!is.na(alien_island), .N] 
# 10928

# How many species got alien status information?
dt_oc[!is.na(alien_island), spp_binary] %>% unique %>% length
# 1006

# How many species in total in the occurence dataset?
dt_oc[, spp_binary] %>% unique %>% length
# 2540

# For how many species do we have alien status info?
dt_pi[!is.na(alien_island), species] %>% unique %>% length
# 2761

# For how many geoentity ids do we have alien status info?
dt_pi[!is.na(alien_island), entt_id_island] %>% unique %>% length
# 448

# How many geoentity ids from the occurence dataset got alien status information?
dt_oc[!is.na(alien_island), entt_ID] %>% unique %>% length
# 128
# Out of how many?
dt_oc[, entt_ID] %>% unique %>% length
# 1702

# Change some column names
setnames(x = dt_oc,
         old = c("x", "y", "coordinateUncertaintyInMeters"),
         new = c("long", "lat", "coord_uncert_m"))

# Total records
nrow(dt_oc)
# 10280701

# Save as binary rds object.
saveRDS(object = dt_oc, file = paste0(save_path, "occ_clean_glonaf.rds"))
