# library(geosphere)
# distHaversine(c(45.45536,9.192274), c(45.45532,9.192986))
# distVincentyEllipsoid(c(45.45536,9.192274), c(45.45532,9.192986))
# 
# Compute the distance in meters between two coordinates 
#
# m1 = as.matrix(test_coordinates |> select(Longitudine,Latitudine))
# dist_matrix <- distm(m1, fun = distVincentyEllipsoid) / 1000
# colnames(dist_matrix) <- test_coordinates |> pull(Nome) 
# rownames(dist_matrix) <- test_coordinates |> pull(Nome)
# 
# dist_matrix[1:3,1:3] |> View()  

test = cbind(c(45.47604,45.47612,45.47611),        
             c(9.138718,9.138884,9.138870))

n_points_in_circle(test,0.1)
n_points_in_circle <- function(points, r) {
  # Input: points is a matrix or data frame with columns (longitude, latitude)
  n <- nrow(points)
  if (n < 2) {
    return(TRUE)  # Trivially true for 0 or 1 point
  }
  
  # Compute pairwise distance matrix using distm (in meters, convert to km)
  dist_matrix <- distm(points[, c(2, 1)], fun = distHaversine) / 1000
  
  # Find maximum pairwise distance (excluding diagonal)
  max_dist <- max(dist_matrix[upper.tri(dist_matrix)])
  
  # Check if maximum distance is at most 2 * r
  return(max_dist <= 2 * r)
}


