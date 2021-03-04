# /////////////////////////////////////////////////////////////////////////
#
# Makes 10 km geodesic buffers around botanical gardens. Save them as a
# shapefile for further use in removing occurrences around botanical gardens.
# The buffers should be geodesic so that distortions across various latitudes
# are avoided. Distortions increase towards the poles.
#
# /////////////////////////////////////////////////////////////////////////


# Clean environment, including hidden objects (which begin with a .)
rm(list = ls(all.names = TRUE))

# Load and/or install packages.
source("scripts/helper_functions/load_packages.R")

# Source make_GeodesicBuffer() function
source("scripts/helper_functions/make_geodesic_buffer.R")

# Use read.delim(), because it deals with encoding issues (File
# BGCI_garden_coordinates.csv was edited under Linux and contains characters
# that Windows does not interpret properly).
botanical_gardens_orig <- read.delim("data/botanical_gardens/BGCI_garden_coordinates.csv", 
                                     stringsAsFactors = FALSE,
                                     strip.white = TRUE,
                                     encoding = "UTF-8")
setDT(botanical_gardens_orig) # convert to data.table object

# Rename the 1st column name
setnames(botanical_gardens_orig, old = "X.U.FEFF.gardenid", new = "garden_id")

# Transform coordinates to numeric
botanical_gardens_orig[, ':=' (lat = as.numeric(Latitude),
                               lon = as.numeric(Longitude))]

# The warning message NAs introduced by coercion indicates that some values
# cannot be directly converted to numeric or coordinates are missing:
botanical_gardens_orig[is.na(lat) | is.na(lon)]

# The record garden_id==5054 needs to be converted from DMS to decimal degrees.
# Check it out:
botanical_gardens_orig[garden_id == 5054, .(Longitude, Latitude)]
# Conversion tool used: https://www.pgc.umn.edu/apps/convert/
# Update values in place (by reference)
botanical_gardens_orig[garden_id == 5054, lat := 40.6975]
botanical_gardens_orig[garden_id == 5054, lon := 73.999167]

# Delete gardens without coordinates (eliminate the NA-s from long-lat)
botanical_gardens_clean <- botanical_gardens_orig[!is.na(lon) | !is.na(lat)]

# Transform to SpatialPoints object
proj_wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
botanical_gardens_pts <- 
  sp::SpatialPointsDataFrame(coords      = botanical_gardens_clean[, c("lon","lat")],
                             data        = botanical_gardens_clean,
                             proj4string = CRS(proj_wgs84))

# Create geodesic buffer around gardens.
botanical_gardens_sp <- make_geodesic_buffer(xy = botanical_gardens_pts,
                                             dist = 10^4, # 10 km (modify it if needed)
                                             crs  = proj_wgs84,
                                             step = 5)    # can be left as such

# Save as binary rds object.
saveRDS(object = botanical_gardens_sp, 
        file = "data/botanical_gardens/buffer/rds/botanical_gardens_buffers.rds")
