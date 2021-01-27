# /////////////////////////////////////////////////////////////////////////
#
# Merge available native/cultivation status information to cleaned occurrence
# dataset.
#
# Activate the document outline (CTRL + SHIFT + O) to see the main steps.
#
# /////////////////////////////////////////////////////////////////////////


# Prepare working environment ---------------------------------------------

rm(list = ls(all.names = TRUE)); gc()

# Create path for saving output.
save_path <- "output/data-clean/"


# Load and/or install packages.
source("scripts/helper_functions/load_packages.R")

# Read data ---------------------------------------------------------------

# Read cleaned occurrence data.
dt_oc <- readRDS(file = "output/data-clean/temp/occ_clean_gift.rds")

# Data about invasive status, cultivation status & natural score. Data from
# Michael Wohlwend. I call this table the "status" table below.
dt_pi <- read.csv2("data/Pacific_Invaders_GIFT_22_01.csv",
                   stringsAsFactors = FALSE,
                   na.strings = c("NA", ""), # pay attention to what actually can be interpreted as NA
                   strip.white = TRUE)
setDT(dt_pi) # convert to data.table object


# Check data --------------------------------------------------------------

# There should be no NA-s in the species names of the cleaned occurrence dataset.
dt_oc[is.na(spp_binary), .N]
# But there are NA-s in the species names of the status table. We delete those NA-s.
dt_pi[is.na(orginal_name), .N] # 664
dt_pi <- dt_pi[!is.na(orginal_name)]

# How many species are in the status table?
length(unique(dt_pi$Species))      # 2674
length(unique(dt_pi$orginal_name)) # 2807
# How many species are in the cleaned occurrence table?
length(unique(dt_oc$spp_binary))   # 2540

# How many species from the occurrence dataset are matched in the status table?
sum(unique(dt_oc$spp_binary) %in% unique(dt_pi$Species))      # 2343
sum(unique(dt_oc$spp_binary) %in% unique(dt_pi$orginal_name)) # 2387
# I will use the orginal_name column. Is unclear why there are two columns, need
# to check with Michael.


# Note that some records in the status table do not have a GIFT id.
dt_pi[is.na(GIFT), .N] # 227 NAs
# And that affects 189 species
dt_pi[is.na(GIFT), uniqueN(orginal_name)] # 189

# Eliminate the rows without GIFT id from the status table
dt_pi <- dt_pi[!is.na(GIFT)]


# Get unique combinations of species and GIFT geoentity id. This is important
# for the merging process.
dt_pi_unq <- unique(dt_pi, by = c("orginal_name", "GIFT"))


# Check species differences between the occurrence and the status table.
dif_sp_1 <- setdiff(sort(unique(dt_pi_unq$orginal_name)),
                    sort(unique(dt_oc$spp_binary)))
length(dif_sp_1)
# 420 species are in the status table (unique) but not in the occurrence table.
# This is not necessarily a problem.

dif_sp_2 <- setdiff(sort(unique(dt_oc$spp_binary)),
                    sort(unique(dt_pi_unq$orginal_name)))
length(dif_sp_2)
# 153 species are in the occurrence table but not in the status table (unique);
# these will not get any status info.

# Select only those species from the the status table that match the occurrence
# dataset ones
dt_pi_unq <- dt_pi_unq[orginal_name %in% unique(dt_oc$spp_binary)]

dt_pi_unq[, c("X", "Species") := NULL] # delete extra columns

# Note the NAs in inva_stat column
dt_pi_unq[is.na(inva_stat), .N] # 5484
sort(unique(dt_pi_unq$inva_stat))
# "alien", "F", "invasive", "naturalized", "naturalized_archeophyte", "T" 

# There are no NAs in cult_stat and nat_score
dt_pi_unq[is.na(cult_stat), .N] # 0
sort(unique(dt_pi_unq$cult_stat))
# 0.0 0.5 1.0
dt_pi_unq[is.na(nat_score), .N] # 0
sort(unique(dt_pi_unq$nat_score))
# from 0 to 1


# Merge -------------------------------------------------------------------

# Because an occurrence can intersect several GIFT polygons, we first melt the
# occurrence data table into a long format so that we get all GIFT IDs in one
# column. Then we merge the cleaned status table to this long format occurrence
# table. We keep only the records that got status information, discard
# uninformative records and then merge this table to the original occurrence
# table.


# ~ melt ------------------------------------------------------------------

dt_oc[, row_id := 1:.N]

dt_oc_melt <- melt(dt_oc,
                   id.vars = c("spp_binary", "row_id"),
                   measure.vars = c("entt_ID_1", "entt_ID_2", "entt_ID_3",
                                    "entt_ID_4", "entt_ID_5", "entt_ID_6"),
                   variable.name = "id_name",
                   value.name = "entt_ID")
dt_oc_melt[is.na(entt_ID), .N] # about half do not have GIFT id
dt_oc_melt <- dt_oc_melt[! is.na(entt_ID)]


#  ~ some checks ----------------------------------------------------------

length(unique(dt_pi_unq$GIFT)) # 433
sum(unique(dt_pi_unq$GIFT) %in% unique(dt_oc_melt$entt_ID))
# 165 out of 433 GIFT ids from status table are among the GIFT ids from the
# occurrence table. Why so little? The problem might be here.

# hist(unique(dt_oc_melt$entt_ID))
# hist(unique(dt_pi_unq$GIFT))

gift_tbl <- fread("data/geoentities_2018-05-09/geoentities_attribute_table.csv")
length(unique(gift_tbl$entt_ID)) # 2868 total GIFT ids
sum(unique(dt_pi_unq$GIFT) %in% unique(gift_tbl$entt_ID)) # 432
# hist(unique(gift_tbl$entt_ID))

unique(dt_pi_unq$GIFT)[! unique(dt_pi_unq$GIFT) %in% unique(gift_tbl$entt_ID)]
# GIFT ID 698 doesn't exist in the GIFT dataset. This is strange... could be
# because I used an older GIFT dataset?
698 %in% unique(gift_tbl$entt_ID) # FALSE

# All occurrence GIFT IDs exist in the GIFT dataset
length(unique(dt_oc_melt$entt_ID)) # 1789
sum(unique(dt_oc_melt$entt_ID) %in% unique(gift_tbl$entt_ID)) # 1789
sum(unique(gift_tbl$entt_ID) %in% unique(dt_oc_melt$entt_ID)) # 1789


# ~ merge -----------------------------------------------------------------

# Attach status info to the molten occurrence table
dt_oc_melt <- merge(x = dt_oc_melt,
                    y = dt_pi_unq,
                    by.x = c("spp_binary", "entt_ID"),
                    by.y = c("orginal_name", "GIFT"),
                    all.x = TRUE)

dt_oc_melt[!is.na(cult_stat),.N] # 10924 (or 10870 if use Species instead of orginal_name)

# Keep only the records that got status information (use cult_stat and not
# inva_stat)
dt_oc_melt_2 <- dt_oc_melt[!is.na(cult_stat)]

# If there are duplicates in row_id then drop those that are uninformative
any(duplicated(dt_oc_melt_2$row_id))
dt_oc_melt_2[duplicated(row_id)]
dt_oc_melt_2[row_id == 968]

dt_oc_melt_2 <- dt_oc_melt_2[! (row_id == 968 & entt_ID == 1695) ]

# Attach status information to the original occurrence table.
dt_oc_2 <- merge(x = dt_oc,
                 y = dt_oc_melt_2[, .(row_id, nat_score, cult_stat, inva_stat)],
                 by = "row_id",
                 all.x = TRUE)
# How many records got alien status information?
dt_oc_2[!is.na(inva_stat), .N] # 9718
dt_oc_2[!is.na(cult_stat), .N] # 10923
dt_oc_2[!is.na(nat_score), .N] # 10923


# Change some column names
setnames(x = dt_oc_2,
         old = c("x", "y", "coordinateUncertaintyInMeters"),
         new = c("long", "lat", "coord_uncert_m"))

# Total records
nrow(dt_oc_2)
# 10280701

# Save as binary rds object.
saveRDS(object = dt_oc_2, file = paste0(save_path, "occurrence_clean_with_alien_status.rds"))
