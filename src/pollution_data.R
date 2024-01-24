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

source("src/spatial_library.R")
source("src/key_amazon.R")

# Health district 
district =  geojson_read("https://www.dati.lombardia.it/resource/9n45-7bpc.geojson",
                         what = "sp")
dt_district = tidy(district)
dt_district = data.table(dt_district)
dt_district = merge(dt_district,
                    district@data[,c("objectid_1","codice_ats","distretto","descrizion")],
                    by.x = "id", by.y = "objectid_1")
dt_district$id = as.numeric(dt_district$id)

# INEMAR data and aggregate per municipalities
inemar = data.table(read_feather("s3://envbran/maps/inemar.arrow"))
inemar = inemar[,lapply(.SD, sum, na.rm=TRUE), by= .(`Istat comune`), 
                    .SDcols = names(inemar)[6:36]]

# Load municipalities geo-coodrdinates
municipalities = data.table(read_feather("s3://envbran/maps/municipalities.arrow"))
municipalities = municipalities[istat %in% inemar$`Istat comune`]

pollution = merge(inemar,municipalities, 
                  by.x = "Istat comune",
                  by.y = "istat")

# Map each municipality in health district
pollution = map_point(district,pollution,"district_id")
pollution = merge(pollution[,lapply(.SD, sum, na.rm=TRUE), by= .(district_id),
                            .SDcols = names(pollution)[2:32]],
                  district@data[,c("objectid_1","codice_ats","distretto","descrizion")],
                  by.x = "district_id", by.y = "objectid_1", all = T)

# Milano is a unique municipality but has 9 health district
# so we don't have data for each district. We can fill the NA's
# with a weighted average between the data of Milano municipality
# and the contiguous heath district but it's emission so just split
# the value equally over all districts

pollution[is.na(As), c(names(pollution)[2:32]) := pollution[district_id == 78,2:32,with=F]/9]
pollution[district_id == 78, c(names(pollution)[2:32]) := pollution[district_id == 78,2:32,with=F]/9]

