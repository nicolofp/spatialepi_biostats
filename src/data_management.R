library(data.table)
library(readxl)
library(dplyr)
library(arrow)
library(sf)

source("src/key_amazon.R")
get_bucket("envbran")

# List of municipalities
comuni = read_xlsx("C:/Users/nicol/Documents/spatial_statistics/data/2_location_data/italy_geo.xlsx",
                   sheet = 2, range = "A1:D7979")
write_feather(comuni,"C:/Users/nicol/Documents/spatial_statistics/data/2_location_data/municipalities.arrow") 
municipalities = list.files("C:/Users/nicol/Documents/spatial_statistics/data/2_location_data",
                            pattern = "municipalities.arrow",full.names = T)

# INEMAR data
inemar = fread(paste0("C:/Users/nicol/Documents/spatial_statistics/data/3_pollutant_data/",
                      "Dati_INEMAR_2019_richiesti_il_18-Nov-2023_16.40.18/",
                      "Dati_INEMAR_2019_richiesti_il_18-Nov-2023_16.40.18.csv"),
               fill = TRUE, nrows = 34859)

# Sum the pollutant by distretto
inemar = inemar[-c(1),-c("V37"),with=F]
inemar[,c(names(inemar)[6:36]) := lapply(.SD, function(i) as.numeric(i)),
       .SDcols = names(inemar)[6:36]]
write_feather(inemar,"C:/Users/nicol/Documents/spatial_statistics/data/3_pollutant_data/inemar.arrow") 
inemar_data = list.files("C:/Users/nicol/Documents/spatial_statistics/data/3_pollutant_data/",
                            pattern = ".arrow",full.names = T)

write_on_s3 = c(municipalities,inemar_data)

lapply(1:NROW(write_on_s3),function(i){
  put_object(file = paste0(write_on_s3[i]),
             bucket = 'envbran/maps',
             multipart = TRUE)
})

tmp = list.files("C:/Users/nicol/Documents/", pattern = ".arrow",full.names = T)
lapply(1:3,function(i){
  put_object(file = paste0(tmp[i]),
             bucket = 'envbran/AQ',
             multipart = TRUE)
})

# WQ - link
#https://sinacloud.isprambiente.it/portal/apps/sites/?fromEdit=true#/portalepesticidi/pages/area-download
#https://www.lemonde.fr/en/les-decodeurs/article/2023/02/23/forever-pollution-explore-the-map-of-europe-s-pfas-contamination_6016905_8.html

# Create duckdb connection
duckdb_con = DBI::dbConnect(duckdb::duckdb())

DT = open_dataset("s3://envbran/AQ/",format = "arrow") 
DT = DT |> to_duckdb(table_name = "test", 
                     con = duckdb_con)

DT |>
  filter(data == "2024-01-01")

dbGetQuery(duckdb_con, "SELECT Count(DISTINCT data) AS Ndays FROM test;")

pfas = fread("C:/Users/nicol/Documents/full.csv")
pfas[country == "Italy" & !is.na(pfas_sum) & source_text == "ARPA Lombardia" & matrix == "Groundwater"]




R = fread("https://www.dati.lombardia.it/resource/fwpe-xzv8.csv?$limit=5000000")
G = fread("https://www.dati.lombardia.it/resource/6n7g-5p5e.csv")

length(unique(E$codice_ente))
length(unique(R$codice_ente))
base::intersect(unique(R$codice_ente),
                unique(E$codice_ente))

outersect <- function(x, y, ...) {
  big.vec <- c(x, y, ...)
  duplicates <- big.vec[duplicated(big.vec)]
  setdiff(big.vec, unique(duplicates))
}

outersect(unique(R$codice_ente),
          unique(E$codice_ente))



library(data.table)
library(sf)
library(sp)
library(geojson)
library(geojsonsf)

shape = read_sf(dsn = "C:/Users/nicol/Documents/WQ_Lombardia/SQA_GW_download_2021/SQA_GW_downloadPoint.shp")
shape = read_sf(dsn = "C:/Users/nicol/Downloads/SQA_GW_download/SQA_GW_downloadPoint.shp")
tmp = st_transform(shape, crs = 4326)

tmp = data.table(tmp)
tmp_cast = dcast(tmp, cod_Staz ~ sostanza, value.var = "conc_media")

summary(tmp_cast)






