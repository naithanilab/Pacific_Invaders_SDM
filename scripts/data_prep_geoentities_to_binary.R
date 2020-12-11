# /////////////////////////////////////////////////////////////////////////
#
# Convert geoentities shapefile to binary RDS format.
#
# /////////////////////////////////////////////////////////////////////////


# Read shpefile format from Patrick Weigelt
g_entt <- rgdal::readOGR(dsn   = "sdm/data/geoentities_2018-05-09/geoentities.shp",
                         layer = "geoentities", 
                         stringsAsFactors = FALSE)
# ~ 30 sec

# Save as binary RDS object.
saveRDS(object = g_entt, file = "sdm/data/geoentities_2018-05-09/rds/geoentities_2018-05-09.rds")
