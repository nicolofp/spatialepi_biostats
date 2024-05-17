library(data.table)
library(geojsonsf)
library(lubridate)
library(geojsonio)
library(ggplot2)
library(viridis)
library(readxl)
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

# Hospitalization data
url_hospit = paste0("s3://envbran/maps/hospitalization.arrow")
hospit = arrow::read_ipc_file(url_hospit)

lh = hospit[descrizione_acc_diagnosi %in% c("MALATTIE DEL SISTEMA RESPIRATORIO") &
              descrizione_acc_intervento %in% c("PROCEDURE DIAGNOSTICHE E TERAPEUTICHE VARIE") &
                                          codice_mdc == "04"][, .(h_lungs = sum(ricoveri_do)),
                                                              by = .(latitudine_norm,longitudine_norm)]
hh = hospit[descrizione_acc_diagnosi %in% c("MALATTIE DEL SISTEMA CIRCOLATORIO") &
              descrizione_acc_intervento %in% c("PROCEDURE DIAGNOSTICHE E TERAPEUTICHE VARIE") &
              codice_mdc == "05"][, .(h_heart = sum(ricoveri_do)),
                                  by = .(latitudine_norm,longitudine_norm)]
eh = hospit[descrizione_acc_diagnosi %in% c("MALATTIE E DISTURBI ENDOCRINI, NUTRIZIONALI, METABOLICI E IMMUNITARI") &
              descrizione_acc_intervento %in% c("PROCEDURE DIAGNOSTICHE E TERAPEUTICHE VARIE") &
              codice_mdc == "10"][, .(h_endo = sum(ricoveri_do)),
                                  by = .(latitudine_norm,longitudine_norm)]
h = hospit[descrizione_acc_intervento %in% c("PROCEDURE DIAGNOSTICHE E TERAPEUTICHE VARIE"), 
                                      .(h_all = sum(ricoveri_do)),
                                      by = .(latitudine_norm,longitudine_norm)]

DTh = Reduce(function(...) merge(..., all = TRUE, by = c("latitudine_norm",
                                                         "longitudine_norm")), 
             list(h,eh,hh,lh))
setnames(DTh,c("latitudine_norm","longitudine_norm"), c("lat","lng"))
DTh = map_point(district, DTh, "district_id")
DTh = DTh[,.(h_all = sum(h_all,na.rm = T),
             h_endo = sum(h_endo,na.rm = T),
             h_heart = sum(h_heart,na.rm = T),
             h_lungs = sum(h_lungs,na.rm = T)), by = district_id]

# Water quality
library(data.table)
library(duckdb)
library(arrow)

# Create duckdb connection
duckdb_con = DBI::dbConnect(duckdb::duckdb())

DT = open_dataset("s3://envbran/WQ/WQ_underground.arrow",format = "arrow") 
DT = DT |> to_duckdb(table_name = "test", 
                     con = duckdb_con)

WC = DT |>
  filter(anno == "2019") |>
  as.data.table() 

unique(WC$parametro)
metals = c("Arsenio","Cadmio","Manganese","Mercurio","Piombo")
pfas = c("PFBA (Perfluoro Butanoic Acid)","PFBS (Perfluoro Butane Sulfonate)",
         "PFDA (Perfluoro Decanoic Acid)","PFDoA (Perfluoro Dodecanoic Acid)",
         "PFHpA (Perfluoro Heptanoic Acid)","PFHxA (Perfluoro Hexanoic Acid)",
         "PFHxS (Perfluoro Hexane Sulfonate)","PFNA (Perfluoro Nonanoic Acid)",
         "PFOA (Perfluoro Octanoic Acid)","PFOS (Perfluoro Octane Sulfonate)",
         "PFPeA (Perfluoro Pentanoic Acid)","PFUdA (Perfluoro Undecanoic Acid)")
pcbs = c("PCB 101","PCB 105","PCB 110","PCB 114","PCB 118","PCB 123","PCB 126",
         "PCB 128","PCB 138","PCB 146","PCB 149","PCB 151","PCB 153","PCB 156",
         "PCB 157","PCB 167","PCB 169","PCB 170","PCB 177","PCB 180","PCB 183",
         "PCB 187","PCB 189","PCB 28","PCB 52","PCB 77","PCB 81","PCB 95","PCB 99")
WC[, lat := unlist(WC$lat)]
WC[, lng := unlist(WC$lng)]

WC = map_point(district, WC, "district_id")
WC[, valore_numerico := as.numeric(gsub(",",".",valore_numerico))]

WC[, valore_numerico := ifelse(segno == "<", 
                               valore_numerico/sqrt(2), valore_numerico)]

WC = WC[parametro %in% c("Manganese","Piombo","Solfati")
   ,.(valore_numerico = mean(valore_numerico,na.rm=T)), by = .(parametro,district_id)]

wq = dcast(WC, district_id ~ parametro)






