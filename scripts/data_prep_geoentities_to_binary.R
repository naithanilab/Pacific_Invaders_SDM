# /////////////////////////////////////////////////////////////////////////
#
# Convert geoentities GIFT shapefile to binary RDS format.
#
# /////////////////////////////////////////////////////////////////////////


# Read shpefile format from Patrick Weigelt
g_entt <- rgdal::readOGR(dsn   = "data/geoentities_2018-05-09/geoentities.shp",
                         layer = "geoentities", 
                         stringsAsFactors = FALSE)
# ~ 30 sec

# Save as binary RDS object.
saveRDS(object = g_entt, file = "data/geoentities_2018-05-09/rds/geoentities_2018-05-09.rds")
