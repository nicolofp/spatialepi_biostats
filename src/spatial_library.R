# Mapping function:
# This function allows us to map the geographic coordinates 
# of each point within a specified polygon. It proves 
# particularly valuable for visualizing the locations of 
# points within spatial regions on a map.
#
# Parameters
# space   : class "SpatialPolygonsDataFrame"
# dt_point: data.frame (or data.table) with "lat" and "lng" columns
# out_name: name of the column with mapped index

map_point = function(space, dt_point, out_name){
  x_coords = as.numeric(dt_point$lng)
  y_coords = as.numeric(dt_point$lat)
  c_projection = space@proj4string@projargs
  mPoints = SpatialPoints(coords = cbind(x_coords,y_coords),
                               proj4string = CRS(paste0(c_projection)))
  ids = stack(over(space,mPoints,returnList = TRUE))
  dt_point[[paste0(out_name)]] = ids[order(ids$values),"ind"]
  return(dt_point)
}

# Find the elements that are not in common
# between 2 (or more) vectors

outersect = function(x, y, ...) {
  big_vec = c(x, y, ...)
  duplicates = big_vec[duplicated(big_vec)]
  setdiff(big_vec, unique(duplicates))
}

# Given n coordinate points check if the 
# points are within 2*r (in Km)
#
# Parameters
# points  : data.frame (or data.table) with "lat" and "lng" columns
# r       : max distance between points (2 at the times)

library(geosphere)
n_points_in_circle <- function(points, r) {
  # Input: points is a matrix or data frame with columns (longitude, latitude)
  n <- nrow(points)
  if (n < 2) {
    return(TRUE)  # Trivially true for 0 or 1 point
  }
  
  # Compute pairwise distance matrix using distm (in meters, convert to km)
  dist_matrix <- distm(points[, c(2, 1)], fun = distVincentyEllipsoid) / 1000
  
  # Find maximum pairwise distance (excluding diagonal)
  max_dist <- max(dist_matrix[upper.tri(dist_matrix)])
  
  # Check if maximum distance is at most 2 * r
  return(max_dist <= r)
}


