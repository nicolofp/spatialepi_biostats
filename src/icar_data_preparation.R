library(data.table)
library(duckdb)
library(arrow)
library(dplyr)
library(sf)

source("src/key_amazon.R")
distretti = read_sf(dsn = paste0("C:/Users/nicol/Downloads/DISTRETTI/",
                                 "geo_export_1aed6579-bb7e-4f89-a616-cc0af5ebb0f1.shp"))

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

DTh = st_as_sf(DTh,coords = c('longitudine_norm',"latitudine_norm")) #make points spatial
st_crs(DTh) = st_crs(distretti$geometry)#4326 # Give the points a coordinate reference system (CRS)
DTh = st_transform(DTh, crs = st_crs(distretti$geometry)) # Match 
DTh$objectid_1 = apply(st_intersects(distretti$geometry, DTh, sparse = FALSE), 2, 
                                function(col) {distretti[which(col), ]$objectid_1}) 
DTh = data.table(DTh)
DTh = DTh[,.(h_all = sum(h_all,na.rm = T),
             h_endo = sum(h_endo,na.rm = T),
             h_heart = sum(h_heart,na.rm = T),
             h_lungs = sum(h_lungs,na.rm = T)), by = objectid_1]

# Water quality
# Create duckdb connection
duckdb_con = DBI::dbConnect(duckdb::duckdb())

DT = open_dataset("s3://envbran/WQ/WQ_underground.arrow",format = "arrow") 
DT = DT |> to_duckdb(table_name = "test", 
                     con = duckdb_con)

parameters = c("1,2,3-Triclorobenzene","1,2,4-Triclorobenzene",
               "Antimonio","Arsenico","Cianuri liberi","Cromo VI",
               "Etilbenzene","Manganese","Piombo",
               "Tetracloruro di carbonio","Tricloroetilene",
               "Triclorometano","sommatoria fitofarmaci")

WC = DT |>
  filter(anno == "2019" & parametro %in% parameters) |>
  as.data.table() 

WC = st_as_sf(WC,coords = c('lng','lat')) #make points spatial
st_crs(WC) = st_crs(distretti$geometry)#4326 # Give the points a coordinate reference system (CRS)
WC = st_transform(WC, crs = st_crs(distretti$geometry)) # Match 
WC$objectid_1 = apply(st_intersects(distretti$geometry, WC, sparse = FALSE), 2, 
                       function(col) {distretti[which(col), ]$objectid_1})
WC = data.table(WC)
WC[, valore_numerico := as.numeric(gsub(",",".",valore_numerico))]
WC[, valore_numerico := ifelse(segno == "<", 
                               valore_numerico/sqrt(2), valore_numerico)]

WC = WC[,.(value = mean(valore_numerico,na.rm = T)),
        by = .(parametro,objectid_1)]

wq = dcast(WC, objectid_1 ~ parametro, value.var = "value")
dbDisconnect(duckdb_con)

DF = Reduce(function(...) merge(..., all = TRUE, by = c("objectid_1")), 
                  list(distretti,DTh,wq))
DF = as.data.table(DF)
DF[,c(names(DF)[8:24]) := lapply(.SD, function(i) ifelse(is.na(i),median(i,na.rm = T),i)),
   .SDcols = names(DF)[8:24]]
DF[, ratio_endo := round(100*h_endo/h_all)]
DF[, ratio_heart := round(100*h_heart/h_all)]
DF[, ratio_lungs := round(100*h_lungs/h_all)]
DF[,c(names(DF)[12:24]) := lapply(.SD, function(i) 10*ecdf(i)(i)),
   .SDcols = names(DF)[12:24]]


