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

