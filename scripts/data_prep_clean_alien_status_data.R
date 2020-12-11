# /////////////////////////////////////////////////////////////////////////
#
# This script attempt to have a cleaner version of alien status data. The main
# idea is to combine the two files Pacific_Invaders_0707.csv and
# googlenames_gift.csv, correct some island and species names mismatches, and
# check/remove duplicated records.
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
  if (grepl("sie-group-share", getwd())) "~/winhome/temp/" else "sdm/output/data-clean/temp/"


# Load and/or install packages.
source("sdm/scripts/helper_functions/load_packages.R")


# Read data ---------------------------------------------------------------

# Read data containing alien-status information. All records in this file are
# invasive/alien.
dt_pi <- fread("Original_Data/Pacific_Invaders_0707.csv",
               select = c("X", "species", "island", "is_group", "state"))

# Read data containing island names and their GLONAF id-s
googlenames <- fread("sdm/data/googlenames_gift.csv", 
                     select = c("island", "is_group", "state", "ID_Source", "ID_1"))
# All other ID_ columns in googlenames do not carry useful info.
# See https://github.com/idiv-biodiversity/PacificInvaders/issues/3


# Clean data: -------------------------------------------------------------

# Clean some species names.

dt_pi[is.na(species), .N] # 0
# Remove names like character "NA"
dt_pi[species == "NA", .N] # 161
dt_pi <- dt_pi[species != "NA"]

# Catch any species names that contain any punctuation except -, or starts or
# ends with any kind of space (space, tab, etc)
dt_pi[grepl(pattern = '(?!-)[[:punct:]]|^\\s|\\s$', x = species, perl = TRUE)]
# Delete extra "\t" symbol from species names in dt_pi table.
dt_pi[, species := gsub(pattern = "\t", replacement = "", x = species)]

# Fix some island name differences.
# See https://github.com/idiv-biodiversity/PacificInvaders/issues/6
sort(setdiff(dt_pi$island, googlenames$island))
# [1] "'Atata" "'Eua"   "'Uiha" 
sort(setdiff(googlenames$island, dt_pi$island))
# [1] "Atata"   "Eua"     "Gaioio"  "Gardner" "Puohine" "Ta'una"  "Tapahi"  "Uiha" 

dt_pi[island == "'Atata", island := "Atata"]
dt_pi[island == "'Eua", island := "Eua"]
dt_pi[island == "'Uiha", island := "Uiha"]

googlenames[island == "Gaioio", island := "Tarauru Roa"]
googlenames[island == "Gardner", island := "Nikumaroro"]
googlenames[island == "Puohine", island := "Raiatea"]
googlenames[island == "Ta'una", island := "Tauna"]
googlenames[island == "Tapahi", island := "Tahiti"]


# a) island/group of islands level info -----------------------------------

# There are some duplicates in googlenames, so before merging select the unique
# records. Have a look at the duplicates (all of them).
dup1 <- duplicated(googlenames, by = "island")
dup2 <- duplicated(googlenames, by = "island", fromLast = TRUE)
googlenames[dup1 | dup2][order(island)]
# Keep unique islands only.
googlenames <- unique(googlenames, by = "island")
rm(dup1, dup2)

# Are there differences in island names between the two datasets?
setdiff(googlenames$island, dt_pi$island) # character(0)
setdiff(dt_pi$island, googlenames$island) # there is an NA in dt_pi$island

# Some islands in googlenames have no geoentity ID, see also:
# https://github.com/idiv-biodiversity/PacificInvaders/issues/8
googlenames[is.na(ID_1)]
# Remove records without geoentity ID. They are currently unsolvable.
googlenames <- googlenames[!is.na(ID_1)]

# For 1:N join, in googlenames it should be as many unique islands as rows.
googlenames[, uniqueN(island)] == nrow(googlenames) # TRUE

# Switch "Malakal" to "Koror" in dt_pi as per
# https://github.com/idiv-biodiversity/PacificInvaders/issues/8#issuecomment-460230935
dt_pi[island == "Malakal", island := "Koror"]

# Merge
dt_pi_update <- merge(x = dt_pi, 
                      y = googlenames[, .(island, ID_Source, ID_1)],
                      by = "island",
                      all.x = TRUE)

# All records in the merged table that have a geoentity ID refer to alien
# species at island or group of islands level. The remaining ones could get a
# status at state level information.
dt_pi_update[!is.na(ID_1), alien_island := 1L]


# b) state level info -----------------------------------------------------

state_id <- fread("sdm/data/State_ID.csv")

# Check state name mismatches with dt_pi_update table
setdiff(dt_pi_update[, unique(state)], state_id$State) # character(0)
setdiff(state_id$State, dt_pi_update[, unique(state)]) # character(0)
# No differences detected.

# For correct 1:N join, in state_id it should be as many unique states as rows.
state_id[, uniqueN(State)] == nrow(state_id) # TRUE

dt_pi_update <- merge(x = dt_pi_update, 
                      y = state_id,
                      by.x = "state",
                      by.y = "State",
                      all.x = TRUE)

dt_pi_update[!is.na(Geo_ID_state), alien_state := 1L]
# Actually, all records got a state ID (no NA-s)
dt_pi_update[is.na(Geo_ID_state), .N] # 0


# Remove duplicates -------------------------------------------------------

# Check duplicates by species, state and island id-s
dup1 <- duplicated(dt_pi_update, by = c("species", "Geo_ID_state", "ID_1"))
dup2 <- duplicated(dt_pi_update, by = c("species", "Geo_ID_state", "ID_1"), fromLast = TRUE)
dt_pi_dup_isl_state <- dt_pi_update[dup1 | dup2]
setorder(dt_pi_dup_isl_state, state, island, Geo_ID_state, ID_1, species)

# Keep unique records by species, state and island id-s
dt_pi_update_unq_isl_state <- unique(dt_pi_update, by = c("species", "Geo_ID_state", "ID_1"))
setorder(dt_pi_update_unq_isl_state, state, island, Geo_ID_state, ID_1, species)


# Save data ---------------------------------------------------------------

# Rename columns for clarity
setnames(dt_pi_update_unq_isl_state, 
         old = c("ID_1", "Geo_ID_state", "X"), 
         new = c("entt_id_island", "entt_id_state", "idx_X"))

write.csv(dt_pi_update_unq_isl_state,
          file = paste0(save_path, "pi_alien.csv"),
          row.names = FALSE)
