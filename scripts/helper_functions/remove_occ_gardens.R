# /////////////////////////////////////////////////////////////////////////
#
# Function to remove occurrences that are within a certain range from botanical
# gardens.
#
# /////////////////////////////////////////////////////////////////////////

remove_occ_gardens <- function(dt, gardens){
  start_time <- Sys.time() # will measure CPU time
  
  # Get only the unique pairs of coordinates from the occurence dataset. Working
  # with the unique locations is faster.
  unq_xy <- unique(dt[, .(x, y)])
  unq_xy_pts <- sp::SpatialPoints(coords      = unq_xy,
                                  proj4string = gardens@proj4string)
  
  # NOTE: garden buffers must be SpatialPolygons and not
  # SpatialPolygonsDataFrame. Is important because, in this way, sp::over()
  # function gives a vector of buffer ID-s corresponding to each intersected
  # occurrence; NA-s mean that points do NOT intersect any garden buffer
  # polygon. This is then further used for indexing the occurrences. If
  # SpatialPolygonsDataFrame is used instead of SpatialPolygons, then sp::over()
  # returns a large data.frame instead of simple vector (which consumes more RAM
  # & CPU time).
  ovr_gardens <- sp::over(x = unq_xy_pts, 
                          y = gardens)
  
  # Mark the occurrences within gardens.
  # - get the table of unique coordinate pairs inside botanical gardens.
  unq_xy_in_gardens <- unq_xy[!is.na(ovr_gardens)]
  # - left join using coordinates as composite key and for the coordinate pairs
  # inside botanical gardens, mark longitude as NA.
  dt[unq_xy_in_gardens, on = .(x, y), x := NA]
  # An inner join with nomatch = 0 could suffice as well, but I wanted to have a
  # RAM efficient way to count how many records were deleted from the raw data.
  
  message(dt[is.na(x), .N], " points inside botanical gardens were discarded")
  
  # Keep only occurences outside of botanical gardens.
  dt <- dt[!is.na(x)]
  message("Elapsed CPU time: ", round(Sys.time() - start_time, digits = 1))
  return(dt)
}
