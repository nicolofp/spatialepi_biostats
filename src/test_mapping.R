library(data.table)
library(stringdist)
library(dplyr)
library(tidyr)
library(furrr)
library(purrr)
library(sf)
source("src/spatial_library.R")

spec_erogatore |> 
  filter(anno == 2019 & desc_fare_liv_1 == "VISITE") |> View()
  write.csv("ricoveri_erogatore_20250701.csv",row.names = F)

anagrafica = readxl::read_xlsx("G:/My Drive/Temporary/Anagrafica+Strutture+Sanitarie_20191231.xlsx",
                               skip = 1)
strutture = read_sf(dsn = "data/REGIONE_LOMBARDIA/Strutture_sanitarie.shp")
spec_erogatore = fread("https://www.dati.lombardia.it/resource/qm4z-s92m.csv?$limit=5000000")

strutture_red = strutture |>
  select(NOME_STRUT,CD_ATS,CD_ASST) |>
  distinct() |>
  st_drop_geometry() 

erogatore_red = spec_erogatore |>
  select(desc_struttura_erogazione,cod_struttura_erogazione,
         cod_ats_erogazione,desc_ats_erogazione,
         codice_ente,tipo_ente) |>
  filter(tipo_ente != "DATO ERRATO") |>
  mutate(codice_ente = paste0("030",sprintf("%03d", as.numeric(codice_ente))),
         cod_ats_erogazione = paste0("030",
                                     sprintf("%03d", as.numeric(cod_ats_erogazione)))) |>
  distinct() 


ID_anagrafica = spec_erogatore |>
  filter(desc_fare_liv_1 == "VISITE" & anno == 2019 & 
           desc_branca == "PNEUMOLOGIA") |>
  distinct(cod_struttura_erogazione) #|>
  inner_join(anagrafica |> select(`Codice Struttura`,Località),
             by = c("cod_struttura_erogazione" = "Codice Struttura")) #|>
  pull(desc_struttura_erogazione)
# ID_anagrafica: id mappati tramite anagrafica

erogatore_red = erogatore_red |> filter(!(desc_struttura_erogazione %in% ID_anagrafica))

# Set up parallel processing
plan(multisession, workers = availableCores() - 1)  # Use all cores except one

result_exact <- future_map_dfr(seq_along(erogatore_red$desc_struttura_erogazione), 
                               function(i){
  print(i)
  strutture_red |>
    filter(erogatore_red$desc_struttura_erogazione[i] == NOME_STRUT &
             CD_ATS == erogatore_red$cod_ats_erogazione[i]) |>
    mutate(original = erogatore_red$desc_struttura_erogazione[i])
}, .options = furrr_options(seed = TRUE))

result_exact |> distinct() |> View()
ID_mapped = result_exact |> 
  distinct() |> pull(NOME_STRUT)
  
erogatore_red = erogatore_red |> filter(!(desc_struttura_erogazione %in% ID_mapped))

#NROW(erogatore_red)
X = lapply(1:NROW(erogatore_red), function(i){
  print(i)
  ID = which(grepl(erogatore_red$desc_struttura_erogazione[i],
                   strutture_red$NOME_STRUT, ignore.case = TRUE) &
               erogatore_red$cod_ats_erogazione[i] == strutture_red$CD_ATS)
  
  if(length(ID) >= 1){
    data.table(strutture_red |> slice(ID),
               desc_struttura_erogazione = erogatore_red$desc_struttura_erogazione[i])
  }else{
    ID2 = which(agrepl(erogatore_red$desc_struttura_erogazione[i], 
                       strutture_red$NOME_STRUT, ignore.case = TRUE,
                       max.distance = 0.2) &
                  erogatore_red$cod_ats_erogazione[i] == strutture_red$CD_ATS)
    if(length(ID2) > 1){
      data.table(strutture_red |> slice(ID2),
                 desc_struttura_erogazione = erogatore_red$desc_struttura_erogazione[i])
    }else{
      NULL
    }  
  }
})

X = rbindlist(X)
X[,n_rep := .N, by = desc_struttura_erogazione]
X |> select(desc_struttura_erogazione) |> distinct()
X = X |>
  inner_join(strutture |>
               select(NOME_STRUT,CD_ATS,CD_ASST,LAT,LONG_),
             by = c("NOME_STRUT","CD_ATS","CD_ASST"))
x1 = X |> filter(n_rep == 1) 
Y = X |> filter(n_rep > 1) 
Y1 <- future_map_dfr(unique(Y$desc_struttura_erogazione), function(i) {
  print(i)
  Y |>
    st_drop_geometry() |>
    filter(desc_struttura_erogazione == i) |>
    mutate(check = n_points_in_circle(cbind(LAT,LONG_),10))
}, .options = furrr_options(seed = TRUE))
Y1 |> write.csv("partial_mapping.csv")

erogatore_red = erogatore_red |> 
  filter(!(desc_struttura_erogazione %in% unique(Y1$desc_struttura_erogazione)))


Z = lapply(1:NROW(erogatore_red), function(i){
  print(i)
  ID = which(grepl(erogatore_red$desc_struttura_erogazione[i],
                   strutture_red$NOME_STRUT, ignore.case = TRUE) &
               erogatore_red$cod_ats_erogazione[i] == strutture_red$CD_ATS)
  
  if(length(ID) >= 1){
    data.table(strutture_red |> slice(ID),
               desc_struttura_erogazione = erogatore_red$desc_struttura_erogazione[i])
  }else{
    ID2 = which(agrepl(erogatore_red$desc_struttura_erogazione[i], 
                       strutture_red$NOME_STRUT, ignore.case = TRUE,
                       max.distance = 0.2) &
                  erogatore_red$cod_ats_erogazione[i] == strutture_red$CD_ATS)
    if(length(ID2) > 1){
      data.table(strutture_red |> slice(ID2),
                 desc_struttura_erogazione = erogatore_red$desc_struttura_erogazione[i])
    }else{
      NULL
    }  
  }
})
Z = rbindlist(Z)
Z |> select(desc_struttura_erogazione) |> distinct()
Z |> write.csv("final_problems.csv")


W = read.csv("partial_mapping_done.csv")
final1 = unique(c(ID_anagrafica,ID_mapped,Z$desc_struttura_erogazione,
           unique(W$desc_struttura_erogazione)))


strutture |>
  select(NOME_STRUT,COMUNE,PROV,LAT,LONG_) |>
  View()

erogatore_red |>
  filter(!(desc_struttura_erogazione %in% final1)) |>
  distinct() |> write.csv("G:/My Drive/Temporary/last_call.csv")



library(arrow)
comuni = read_feather("data/municipalities.arrow")
# 020072 - BORGO MANTOVANO - Pieve di Coriano, Revere and Villa Poma
# 013254 - CENTRO VALLE INTELVI - Casasco d'Intelvi, Castiglione d'Intelvi and San Fedele Intelvi

comuni = comuni |>
  mutate(comune = toupper(comune)) |>
  mutate(comune = ifelse(comune == "MUGGIÒ","MUGGIO'",comune),
         comune = ifelse(comune == "VILLA D'ALMÈ","VILLA D'ALME'",comune),
         comune = ifelse(comune == "BIGARELLO","SAN GIORGIO BIGARELLO",comune),
         comune = ifelse(comune == "RODENGO SAIANO","RODENGO-SAIANO",comune),
         comune = ifelse(comune == "SALÒ","SALO'",comune),
         comune = ifelse(comune == "CANTÙ","CANTU'",comune),
         comune = ifelse(comune == "VIGGIÙ","VIGGIU'",comune)) |>
  add_row(istat = c(20072),
          comune = c("BORGO MANTOVANO"),
          lng = c("11.122"),
          lat = c("45.0265")) |>
  add_row(istat = c(13254),
          comune = c("CENTRO VALLE INTELVI"),
          lng = c("9.0637"),
          lat = c("45.9505"))
  
# All anagrafica mapped into municipalities 

first_slot = spec_erogatore |>
  select(desc_struttura_erogazione,cod_struttura_erogazione,
         desc_ats_erogazione) |>
  distinct() |>
  inner_join(anagrafica |>
               select(`Codice Struttura`,ATS,Località),
             by = c("cod_struttura_erogazione" = "Codice Struttura",
                    "desc_ats_erogazione" = "ATS")) |>
  inner_join(comuni, by = c("Località" = "comune"))


second_slot = spec_erogatore |>
  filter(tipo_ente != "DATO ERRATO") |>
  select(desc_struttura_erogazione,cod_struttura_erogazione,
         desc_ats_erogazione,cod_ats_erogazione) |>
  mutate(cod_ats_erogazione = paste0("030",sprintf("%03d", as.numeric(cod_ats_erogazione)))) |>
  distinct() |>
  inner_join(result_exact |> distinct(),
             by = c("desc_struttura_erogazione" = "original",
                    "cod_ats_erogazione" = "CD_ATS")) |>
  inner_join(strutture, by = c("desc_struttura_erogazione" = "NOME_STRUT",
                               "desc_ats_erogazione" = "DS_ATS"))

third_slot = x1 

fourth_slot = W |>
  group_by(desc_struttura_erogazione) |>
  mutate(ID_struttura = row_number()) |>
  filter(ID_struttura == 1)

fifth_slot = Z |>
  inner_join(strutture,
             by = c("NOME_STRUT","CD_ATS","CD_ASST"))



t1 = bind_rows(#first_slot |> select(desc_struttura_erogazione,desc_ats_erogazione,lng,lat),
          #second_slot |> select(desc_struttura_erogazione,desc_ats_erogazione,LAT,LONG_),
          third_slot |> select(desc_struttura_erogazione,CD_ATS,LAT,LONG_) |>
            mutate(CD_ATS = as.numeric(CD_ATS)),
          fourth_slot |> select(desc_struttura_erogazione,CD_ATS,LAT,LONG_) |>
            mutate(CD_ATS = as.numeric(CD_ATS)),
          fifth_slot |> select(desc_struttura_erogazione,CD_ATS,LAT,LONG_) |>
            mutate(CD_ATS = as.numeric(CD_ATS))) |> 
  inner_join(strutture |>
               select(CD_ATS,DS_ATS) |>
               st_drop_geometry() |>
               mutate(CD_ATS = as.numeric(CD_ATS)) |> distinct(),
             by = "CD_ATS") |>
  distinct()


t2 = bind_rows(first_slot |> select(desc_struttura_erogazione,desc_ats_erogazione,lng,lat) |>
            rename("LAT" = "lat",
                   "LONG_" = "lng") |>
            mutate(LAT = as.numeric(LAT),
                   LONG_ = as.numeric(LONG_),),
          second_slot |> select(desc_struttura_erogazione,desc_ats_erogazione,LAT,LONG_)) |> distinct()

bind_rows(t1,t2 |> rename("DS_ATS" = "desc_ats_erogazione")) |>
  select(-CD_ATS) |> distinct() |> View()









spec_erogatore |>
  filter(desc_fare_liv_1 == "VISITE" & anno == 2019 & 
           desc_branca == "PNEUMOLOGIA") |>
  inner_join(anagrafica |> select(`Codice Struttura`,Località),
           by = c("cod_struttura_erogazione" = "Codice Struttura")) |>
  View()










