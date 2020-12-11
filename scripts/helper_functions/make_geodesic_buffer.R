# /////////////////////////////////////////////////////////////////////////
#
# Function to make a circle-buffer around given points (long-lat coordinates).
# Is different from rgeos::gBuffer() by the fact that it allows the user to
# create a geodesic buffer with a width expressed in metric units. An Euclidian
# buffer with rgeos::gBuffer(), would introduce distortions that vary greatly
# with latitude and the radius of the circle buffer. 
# See also my question addressed here:
# https://gis.stackexchange.com/questions/250389/euclidean-and-geodesic-buffering-in-r
# 
# ARGUMENTS
# - xy:
#       matrix or spatial point object (SpatialPointsDataFrame) with long-lat
#       coordinates (column 1 = longitude, column 2 = latitude);
# - step:
#       bearing (direction) step in degrees; dictates the point density of the
#       circle-buffer edge;
# - dist:
#       distance (radious of circle buffer) in meters;
# - crs:
#       string of class CRS-class (CRS-class {sp})
#       e.g. "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0";
# 
#
# VALUE/RETURNS
#
# Circle buffers as SpatialPolygons for the give xy points.
#
# /////////////////////////////////////////////////////////////////////////


make_geodesic_buffer <- function(xy, dist, crs, step = 5){
  
  # Check for valid input  --------------------------------------------------
  
  # Check if xy argument is of expected class
  is.class.ok <- inherits(xy, c("SpatialPoints",
                                "SpatialPointsDataFrame",
                                "matrix"))
  if (!is.class.ok) stop("For xy argument expecting 'SpatialPoints', 'SpatialPointsDataFrame' or 'matrix' class")
  # Depending on class of xy, get number of points
  if( inherits(xy, c("SpatialPoints","SpatialPointsDataFrame")) ){
    N.points <- length(xy) # if is an sp object, take length
    # Also if spatial object is projected then stop with error
    if (is.projected(xy)) stop("Spatial object xy is projected; expects unprojected coordinates (long-lat)")
  } else {
    # if matrix - some routine checking of coordinates values
    if (dim(xy)[2] != 2) stop("Expecting xy to be a 2 columns matrix (column1=longitude, column2=latitude)")
    if ( ! all(range(xy[,1]) %between% c(-180, 180) & 
               range(xy[,2]) %between% c(-90, 90)) )
      stop("Expects unprojected coordinates in xy (longitude between -180 & 180, latitude between -90 & 90)")
    N.points <- dim(xy)[1] # if is a matrix, take number of rows
  }
  
  
  # Construct buffers as points at given distance and bearing ---------------
  
  # a vector of bearings (fallows a circle)
  dg <- seq(from = 0, to = 360, by = step)
  # Construct equidistant points defining circle shapes (the "buffer points").
  # Inspired from section "Point at distance and bearing" from Robert J. Hijmans
  # in Introduction to the ”geosphere” package at:
  # https://cran.r-project.org/web/packages/geosphere/vignettes/geosphere.pdf
  buff.XY <- geosphere::destPoint(p = xy, 
                                  b = rep(dg, each = N.points), 
                                  d = dist)
  
  
  # Make SpatialPolygon from the points above -------------------------------
  
  buff.XY <- data.table(buff.XY)
  # Group (split) "buffer points" by id. Add column which indicates to which
  # point ID from N.points each circle-buffer point belongs to.
  buff.XY[, id := rep(1:N.points, times = length(dg))]
  # Split in a list of data tables with two columns: lon and lat.
  lst <- split(buff.XY[,.(lon,lat,id)], by = "id", keep.by = FALSE)
  
  # Make SpatialPolygons out of the list of coordinates
  poly   <- lapply(lst, sp::Polygon, hole = FALSE)
  polys  <- lapply(list(poly), sp::Polygons, ID = NA)
  spolys <- sp::SpatialPolygons(Srl = polys, proj4string = CRS(crs))
  # Disaggregate (split in unique polygons)
  spolys.buff <- sp::disaggregate(spolys)
  
  return(spolys.buff)
}

## EXAMPLE:
#
# require(rgeos)
# require(sp)
# require(plotKML)
# require(data.table)
# 
## Generate a random grid-points for a (almost) global bounding box
# b.box <- as(raster::extent(120, -120, -60, 60), "SpatialPolygons")
# proj4string(b.box) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# set.seed(2017)
# pts <- sp::spsample(b.box, n=100, type="regular")
# buf1000km.geodesic <- make_GeodesicBuffer(xy=pts, 
#                                           step=5, 
#                                           dist=10^6, 
#                                           crs=CRS(as.character("+proj=longlat +ellps=WGS84 +datum=WGS84")))
# plot(buf1000km.geodesic)
## save as KML 
# plotKML::kml(buf1000km.geodesic, file.name = "buf1000km.geodesic.kml")
