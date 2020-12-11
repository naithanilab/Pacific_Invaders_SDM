# /////////////////////////////////////////////////////////////////////////
#
# Helper function to get an interactive map view of several strata (shapefiles)
# at once. Since mapview cannot handle huge datasets like GLONAF and GADM or all
# occurrence data at once, then this function:
#
# -	takes a center point and creates and extent around that point (e.g. dist = 2
# degrees) to crop with;
# -	crops the big shapefiles GLONAF and GADM with the extent;
# -	also selects occurrences within that extent;
# -	plots everything in an interactive way for visual inspection.

# /////////////////////////////////////////////////////////////////////////

crop_mapview_pt <- function(centr_long, centr_lat, 
                            occ, occ_long, occ_lat,
                            occ_clean, occ_clean_long, occ_clean_lat,
                            basemaps = list(glonaf = glonaf, gadm = gadm), 
                            dist = 1){
  x_min <- centr_long - dist
  x_max <- centr_long + dist
  y_min <- centr_lat - dist
  y_max <- centr_lat + dist
  
  pt_ext <- raster::extent(c(x_min, x_max, y_min, y_max))
  crops <- lapply(basemaps, FUN = raster::crop, y = pt_ext)
  
  occ_raw <- occ[get(occ_long) %between% c(x_min, x_max) & 
                   get(occ_lat) %between% c(y_min, y_max)]
  occ_raw_sf <- st_as_sf(x = occ_raw, 
                         coords = c(occ_long, occ_lat),
                         crs = "+proj=longlat +datum=WGS84")
  
  occ_clean <- occ_clean[get(occ_clean_long) %between% c(x_min, x_max) & 
                           get(occ_clean_lat) %between% c(y_min, y_max)]
  occ_clean_sf <- st_as_sf(x = occ_clean, 
                           coords = c(occ_clean_long, occ_clean_lat),
                           crs = "+proj=longlat +datum=WGS84")
  
  center_sf <- st_as_sf(x = data.frame(lon = centr_long, lat = centr_lat), 
                        coords = c("lon", "lat"),
                        crs = "+proj=longlat +datum=WGS84")
  
  mapview(crops[[1]], alpha.regions = 0.3, col.regions = "gray") + 
    mapview(crops[[2]], alpha.regions = 0.3) + 
    tryCatch({mapview(occ_raw_sf,
                      color = "red", 
                      col.regions = "red",
                      alpha.regions = 0.6, 
                      alpha = 0.6)}, error = function(e){
                        mapview(center_sf, cex = 10)
                      }) +
    tryCatch({mapview(occ_clean_sf, 
                      color = "green", 
                      col.regions = "green",
                      alpha.regions = 0.6, 
                      alpha = 0.6)}, error = function(e){
                        mapview(center_sf, cex = 10)
                      })
}


crop_mapview_id <- function(id, 
                            occ, occ_long, occ_lat,
                            occ_clean, occ_clean_long, occ_clean_lat,
                            basemaps = list(glonaf = glonaf, gadm = gadm), 
                            dist = 1){
  
  centr_long <- basemaps[[1]]@data[basemaps[[1]]$entt_ID == id, "point_x"]
  centr_lat = basemaps[[1]]@data[basemaps[[1]]$entt_ID == id, "point_y"]
  
  centr_poly <- basemaps[[1]][basemaps[[1]]$entt_ID == id,]
  
  crop_mapview_pt(centr_long, centr_lat, occ, occ_long, occ_lat,
                  occ_clean, occ_clean_long, occ_clean_lat,
                  basemaps = basemaps, dist = dist) +
    mapview(centr_poly, col.regions = "yellow", alpha.regions = 0.4)
}


# # Usage example:
# gadm <- readRDS(file = "sdm/data/gadm/rds/gadm36_0.rds")
# glonaf <- readRDS(file = "sdm/data/geoentities_2018-05-09/rds/geoentities_2018-05-09.rds")
# load(file = "sdm/data/PacAlienSpp_GBIFBIEN_occurrences_Aug2018.RData"); setDT(gbif_bien_occ)
# occ_clean <- readRDS(file = "sdm/output/data-clean/temp/occ_clean.rds")
# 
# system.time({
#   test <- crop_mapview(centr_long = glonaf@data[glonaf$entt_ID == 1617, "point_x"],
#                        centr_lat = glonaf@data[glonaf$entt_ID == 1617, "point_y"], 
#                        occ = gbif_bien_occ, occ_long = "decimalLongitude", occ_lat = "decimalLatitude",
#                        occ_clean = occ_clean, occ_clean_long = "x", occ_clean_lat = "y",
#                        basemaps = list(glonaf = glonaf, gadm = gadm))
# })
# test
# 
# system.time({
#   test <- crop_mapview_id(id = 1617,
#                           occ = gbif_bien_occ, occ_long = "decimalLongitude", occ_lat = "decimalLatitude",
#                           occ_clean = occ_clean, occ_clean_long = "x", occ_clean_lat = "y",
#                           basemaps = list(glonaf = glonaf, gadm = gadm))
# })
# test
