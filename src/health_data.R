library(data.table)
library(geojsonsf)
library(lubridate)
library(geojsonio)
library(ggplot2)
library(viridis)
library(readxl)
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

# Ricoveri per erogatore - api
hospitalization = fread("https://www.dati.lombardia.it/resource/fwpe-xzv8.csv?$limit=1000000&anno=2019")

lungs_hospitalization = hospitalization[descrizione_acc_diagnosi %in% c("MALATTIE DEL SISTEMA RESPIRATORIO") &
                                        descrizione_acc_intervento %in% c("PROCEDURE DIAGNOSTICHE E TERAPEUTICHE VARIE") &
                                        codice_mdc == "04"][, .(h_lungs = sum(ricoveri_dh + ricoveri_do)),
                                                            by = .(latitudine_norm,longitudine_norm)]
all_hospitalization = hospitalization[descrizione_acc_intervento %in% c("PROCEDURE DIAGNOSTICHE E TERAPEUTICHE VARIE"), 
                                      .(h_all = sum(ricoveri_dh + ricoveri_do)),
                                      by = .(latitudine_norm,longitudine_norm)]

# Fill NA's value with 0 (we assume that not reported hospitalization means no hospitalization) 
# Change name to use the function "map_point"
DT = merge(lungs_hospitalization,all_hospitalization, 
           by = c("latitudine_norm","longitudine_norm"), 
           all = T)
DT[is.na(h_lungs), h_lungs := 0]
setnames(DT,c("latitudine_norm","longitudine_norm"),c("lat","lng"))

# Apply the function "map_point"
DT = map_point(district, DT, "district_id")


DT_district = DT[,.(h_all  = sum(h_all),
                    h_lungs  = sum(h_lungs)),
                 by = "district_id"]
DT_district[, ratio := 100*(h_lungs/h_all)]
DT_health = merge(DT_district,
                  district@data[,c("objectid_1","codice_ats","distretto","descrizion")],
                  by.x = "district_id", by.y = "objectid_1", all = T)
DT_health[is.na(ratio), c("ratio","h_lungs","h_all") := 0]

