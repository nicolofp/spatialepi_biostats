library(data.table)
library(geojsonsf)
library(lubridate)
library(geojsonio)
library(ggplot2)
library(viridis)
library(readxl)
library(spdep)
library(arrow)
library(broom)
library(sp)
library(sf)

# Health district 
district =  geojson_read("https://www.dati.lombardia.it/resource/9n45-7bpc.geojson",
                         what = "sp")
dt_district = tidy(district)
dt_district = data.table(dt_district)
dt_district = merge(dt_district,
                    district@data[,c("objectid_1","codice_ats","distretto","descrizion")],
                    by.x = "id", by.y = "objectid_1")
dt_district$id = as.numeric(dt_district$id)

# Air pollution
x = fread(paste0("https://www.dati.lombardia.it/resource/g2hp-ar79.csv?$limit=100000000&$",
                 "where=data>'2018-01-01T00:00:00.000'"))

x = x[year(data) == 2019] 
x[,':='(data_day = as.Date(substr(as.character(data),1,10)))]
y = x[stato == "VA",.(mean_value = mean(valore, na.rm = T)),
      by = .(data_day,idsensore)]

sensori = fread("https://www.dati.lombardia.it/resource/ib47-atvt.csv")
sensori$ID = 1:NROW(sensori)

x_coords = as.numeric(sensori$lng)
y_coords = as.numeric(sensori$lat)
c_projection = district@proj4string@projargs
mPoints = SpatialPoints(coords = cbind(x_coords,y_coords),
                        proj4string = CRS(paste0(c_projection)))
ids = stack(over(district,mPoints,returnList = TRUE))
sensori = merge(sensori,ids,by.x = "ID", by.y = "values", all = T)
sensori = sensori[!is.na(ind)]

length(unique(sensori$nometiposensore))

View(sensori[,.(list_exposure = list(c(nometiposensore))), by = .(idstazione)])
length(unique(sensori$ind))


sensori[nometiposensore %like% "PM10"]




