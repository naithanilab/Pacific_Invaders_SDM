# /////////////////////////////////////////////////////////////////////////
#
# Convert GADM shapefile to binary RDS format.
#
# /////////////////////////////////////////////////////////////////////////


# Read shpefile format.
gadm <- rgdal::readOGR(dsn   = "data/gadm/gadm36_levels_shp/gadm36_0.shp",
                       layer = "gadm36_0", 
                       stringsAsFactors = FALSE)
# ~ 30 sec

# Save as binary RDS object.
saveRDS(object = gadm, file = "data/gadm/rds/gadm36_0.rds")
