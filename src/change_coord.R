library(terra)
library(data.table)
library(sf)
library(sp)
library(geojson)
library(geojsonsf)

# Transform coordinates 
# Water quality - sotterranee
WQ = fread("https://www.dati.lombardia.it/resource/46wy-4ydd.csv?$limit=5000000")
WQ1 = fread("https://www.dati.lombardia.it/resource/beda-kb7b.csv?$limit=5000000")

crs = "+proj=utm +zone=32"
p1 = vect(WQ, geom=c("coord_est", "coord_nord"), crs=crs)
p2 = project(p1, "+proj=longlat")
p2 = st_as_sf(p2)

p2 = as.data.table(p2)
p2[,c("lat") := lapply(1:NROW(p2), function(i) geometry[[i]][2])]
p2[,c("lng") := lapply(1:NROW(p2), function(i) geometry[[i]][1])]
p2[, geometry := NULL]
