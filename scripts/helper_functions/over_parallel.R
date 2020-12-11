# /////////////////////////////////////////////////////////////////////////
#
# Spatial intersection in parallel.
#
# ARGUMENTS
#
# - xy:
#     a two column data.table (long, lat), preferably with unique pairs of
#     coordinates, to avoid coordinate redundancy (also improves CPU time by
#     several order of magnitudes).
#
# - ref:
#     a SpatialPolygonsDataFrame or SpatialPolygons (SpatialPolygonsDataFrame
#     will be converted to SpatialPolygons anyways)
#
# - returnList:
#     logical; defaults to FALSE, in accordance with sp::over; see
#     help(sp::over) for details. Here, it helps with returning all intersected
#     polygons if set to TRUE when ref contains overlapping polygons.
#
# - cpu_frac:
#     What fraction / proportion of total CPU-s to use
#
#
# VALUE/RETURNS 
#
# A list of indices as numeric vectors. See details in help(over) for the case
# where x = "SpatialPoints", y = "SpatialPolygons": <<returns a numeric vector
# of length equal to the number of points; the number is the index (number) of
# the polygon of y in which a point falls; NA denotes the point does not fall in
# a polygon; if a point falls in multiple polygons, the last polygon is
# recorded.>>
#
# /////////////////////////////////////////////////////////////////////////


over_parallel <- function(xy, ref, returnList = FALSE, cpu_frac = .5) {
  
  start_time <- Sys.time() # will measure CPU time
  
  # Locations are split in approx. equal chunks based on the number of cores.
  cpu_n <- round(cpu_frac * parallel::detectCores())
  chunks_idx <- parallel::splitIndices(nx = dim(xy)[1], ncl = cpu_n)
  # Create a list of spatial points from the chunks of coordinates.
  crs <- ref@proj4string
  chunks_pts <- lapply(seq_along(chunks_idx), 
                       function(i){
                         sp::SpatialPoints(coords = xy[chunks_idx[[i]]],
                                           proj4string = crs)
                       })
  # Transform ref (GADM) from SpatialPolygonsDataFrame to SpatialPolygons object
  # because the sp::over() function runs faster when intersecting SpatialPoints
  # with SpatialPolygons.
  ref_sp <- as(ref, "SpatialPolygons")
  
  # Prepare cluster (works both on Linux & Windows)
  cl <- parallel::makeCluster(cpu_n)
  doParallel::registerDoParallel(cl)
  
  # Intersect the chunks of points with the ref polygons.
  # Run spatial intersection in parallel.
  ids <- foreach(i = 1:cpu_n) %dopar% {
    sp::over(x = chunks_pts[[i]],
             y = ref_sp,
             returnList = returnList)
  }
  
  # End cluster
  parallel::stopCluster(cl)
  
  message("Elapsed CPU time: ", format(round(Sys.time() - start_time, 
                                             digits = 1)))
  
  return(ids)
}
