library(data.table)
# library(geojsonsf)
# library(lubridate)
# library(geojsonio)
# library(ggplot2)
# library(viridis)
# library(readxl)
library(duckdb)
library(arrow)
library(dplyr)
# library(broom)
# library(sp)
library(sf)

source("src/key_amazon.R")

ats = read_sf(dsn = paste0("C:/Users/nicol/Downloads/ATS/",
                           "geo_export_733e3559-0967-425a-b898-0e7df40fad94.shp"))
# Hospitalization data
duckdb_con = DBI::dbConnect(duckdb::duckdb())
url_visits = paste0("s3://envbran/maps/visit.arrow")

visits = open_dataset(url_visits, format = "arrow") 
visits = visits |> to_duckdb(table_name = "visits_table",
                             con = duckdb_con)

visits_2019 = visits |>
  filter(anno == "2019" & cod_fare_liv_1 == "V") |>
  as.data.table() 

unique(visits_2019[,.(cod_class_priorita,desc_classe_priorita)])
table(visits_2019$desc_branca)
branca = c("PNEUMOLOGIA","PSICHIATRIA","ENDOCRINOLOGIA",
           "CARDIOLOGIA","NEUROLOGIA","NEUROPSICHIATRIA INFANTILE")

visits_2019[,priority := ifelse(cod_class_priorita %in% c("U","B"),"H","N")]
vs2019 = visits_2019[desc_branca %in% branca,
                     .(N_visits = sum(qta_tot,na.rm = T)),
                     by = .(cod_ats_erogazione,
                            priority,
                            desc_branca)]
vs2019[,T_visits := sum(N_visits,na.rm = T),
       by = .(cod_ats_erogazione)]


