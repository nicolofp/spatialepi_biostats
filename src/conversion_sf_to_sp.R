library(data.table)
library(sf)
library(sp)
library(geojson)
library(geojsonsf)
library(rgdal)

shape = read_sf(dsn = "C:/Users/nicol/Downloads/spatial_statistics/data/SQA_GW_download/SQA_GW_downloadPoint.shp")
tmp = st_transform(shape, crs = 4326)

tmp = data.table(tmp)
tmp_cast = dcast(tmp, cod_Staz ~ sostanza, value.var = "conc_media")

summary(tmp_cast)



length(unique(wq[PARAMETRO == "Arsenico"]$codice_comune))
##
x_coords = sapply(1:length(tmp$geometry), function(i) as.numeric(tmp$geometry[[i]][2]))
y_coords = sapply(1:length(tmp$geometry), function(i) as.numeric(tmp$geometry[[i]][1]))
station_coord = cbind(x_coords,y_coords)
station_coord[!duplicatestation_coord]

comuniPoints = SpatialPoints(coords = cbind(x_coords,y_coords),
                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
tmp$geometry[[1]][2]


trash = lapply(1:NROW(comuni), function(i){
  x_coords = as.numeric(comuni$lng)[i]
  y_coords = as.numeric(comuni$lat)[i]
  
  comuniPoints = SpatialPoints(coords = cbind(x_coords,y_coords),
                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  comuni$distretto[i] <<- as.numeric(which(!is.na(over(distretti,comuniPoints))))
})
