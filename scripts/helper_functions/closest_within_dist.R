# /////////////////////////////////////////////////////////////////////////
#
# ARGUMENTS
#
# - xy: data.table with unique pairs of coordinates; must have only two columns,
# longitude and latitude; - ref: SpatialPolygonsDataFrame object. This is used
# to detect the closest polygons within given buffer distance - dist: buffer
# distance in meters - step: circle point density of buffer (e.g. a vertex at
# each 10 dg, the default)
#
#
# VALUE/RETURNS
#
# An integer vector with the ref polygon indices representing the closest
# polygon to the given coordinates within the give buffer distance. NA-s mean
# that within the give buffer, not match could be found.
#
# /////////////////////////////////////////////////////////////////////////


closest_within_dist <- function(xy, ref, dist, step = 10){
  
  start_time <- Sys.time() # will measure CPU time
  
  ref@data$ref_id <- 1:nrow(ref@data)
  
  # Create geodesic buffer arround points. The make_geodesic_buffer() function
  # must be loaded.
  buf <- make_geodesic_buffer(xy   = as.matrix(xy),
                              step = step,
                              dist = dist,
                              crs  = proj4string(ref))
  
  # Intersect polygons from ref with polygons from buffers. Returns a list of
  # buffer indices - for each polygon in ref returns the indices of the
  # intersected buffers. Order of arguments matters for CPU time (gIntersects
  # can be an expensive operation). The list’s names are the polygon ids from
  # ref. Note that the naming of elements in the list starts from 0 (this is C
  # or C++ way of indexing used in gdal library), while R indices in ref start
  # with 1. However, the buffer indices are ok - correspond to the row indices
  # from xy.
  intrsct <- rgeos::gIntersects(spgeom1 = ref,
                                spgeom2 = buf,
                                byid = TRUE,
                                returnDense = FALSE)
  
  # Prepare the intersection results. NULL elements will be ignored (therefore
  # the warning message).
  intrsct <- utils::stack(intrsct)
  setDT(intrsct)
  setnames(intrsct, c("buf_id", "ref_id"))
  # Note that ref_id is a factor. Convert it to integer and add 1 because of C
  # vs. R indexing differences mentioned above.
  intrsct[, ref_id := as.integer(levels(ref_id))[ref_id] + 1L]
  
  # Get buffer id-s that intersected multiple polygons from ref. For these
  # cases, need to detect which ref polygon (from those intersected) is the
  # closest.
  intrsct_m <- intrsct[, is_mult := .N > 1, by = buf_id][is_mult == TRUE]
  intrsct_m[, is_mult := NULL]
  intrsct[, is_mult := NULL]
  # see https://stackoverflow.com/a/19406921/5193830;
  
  if (dim(intrsct_m)[1] > 0) {
    # Select those buffers that intersected more than one polygon from ref and
    # crop ref with the selected buffers. Also select from ref only those
    # polygons that are intersected by the buffers. Subsetting and cropping a
    # large and dense ref object reduces significantly from RAM & CPU time when
    # computing dist2Line (because it reduces the number of total vertices to
    # test for the shortest distance).
    ref_crop <- raster::crop(x = ref[unique(intrsct_m$ref_id),],
                             y = buf[unique(intrsct_m$buf_id),])
    # Compute the shortest distance between the center of buffers and the
    # cropped ref; Gets the ID-s of the closest polygons from ref.
    dist_mat <- geosphere::dist2Line(p = xy[unique(intrsct_m$buf_id),],
                                     line = ref_crop,
                                     distfun = distHaversine)
    if (dim(dist_mat)[1] != length(unique(intrsct_m$buf_id)))
      stop("Multiple shortest distances; please report this case, sorry :(")
    # Bind in a table the problematic buffer id-s with closest ref polygons' ids
    # (idexed using the ID-s from dist_mat)
    closest_ref <- data.table(buf_id = unique(intrsct_m$buf_id),
                              ref_id = ref_crop@data$ref_id[dist_mat[,"ID"]])
    # Update the problematic records. First, eliminate them, then replace with
    # the corrected ones.
    intrsct <- intrsct[!(buf_id %in% closest_ref$buf_id)] # eliminate
    intrsct <- rbindlist(list(intrsct, closest_ref)) # bind
  }
  
  res <- merge(x = data.table(buf_id = 1:dim(xy)[1]),
               y = intrsct,
               by = "buf_id",
               all.x = TRUE)
  
  message("Elapsed CPU time: ", format(round(Sys.time() - start_time, 
                                             digits = 1)))
  
  return(res$ref_id)
}

# xy <- unq_xy_na[1:1000, .(decimalLongitude, decimalLatitude)]
# ref <- gadm
# dist <- 1000
# step <- 10
# 
# system.time({
#   test <- closest_within_dist(xy, ref, dist)
# })
# Elapsed CPU time: 2.7 mins
# user  system elapsed 
# 109.99   50.83  160.84 
# Warning message:
#   In stack.default(intrsct) : non-vector elements will be ignored
