library(data.table)
library(lubridate)
library(duckplyr)
library(readxl)
library(dplyr)
library(arrow)
library(tidyr)
library(sf)

source("src/key_amazon.R")

## Air quality
stazioni = fread("https://www.dati.lombardia.it/resource/ib47-atvt.csv",
                 encoding = "UTF-8")
id_valid_sensors = stazioni |>
  as_duckdb_tibble() |>
  filter(datastop > as.POSIXct("2020-01-01") | is.na(datastop)) |>
  select(idsensore,nometiposensore,idstazione,comune,lat,lng) |>
  pivot_wider(names_from = idsensore,
              values_from = nometiposensore) |> 
  summarise(across(!comune, ~ sum(!is.na(.x)))) |> names()

valori = open_dataset("s3://envbran/AQ/Dati_sensori_aria_2018_2023_20241221.arrow",
                      format = "arrow")
DT_stazioni_valori_2019 = valori |>
  filter(year(Data) == 2019 & 
           idSensore %in% id_valid_sensors[-c(1:3)] & 
              Valore > 0) |>
  collect() |>
  group_by(idSensore) |>
  summarise(across(Valore, ~ mean(.x))) |>
  inner_join(stazioni, by = c("idSensore" = "idsensore")) 

DT_stazioni_valori_2019 |>
  select(-c(unitamisura,idSensore,storico,
            datastart,datastop,utm_nord,utm_est)) |>
  pivot_wider(names_from = "nometiposensore",
              values_from = "Valore") |>
  summarise(across(!c(idstazione,nomestazione,quota,provincia,
                      comune,lat,lng,location), list(
    count = ~ n(),
    napct = ~ round(mean(is.na(.x)) * 100, 2),
    mean = ~ mean(.x, na.rm = TRUE),
    median = ~ median(.x, na.rm = TRUE),
    sd = ~ sd(.x, na.rm = TRUE),
    cv = ~ sd(.x, na.rm = TRUE)/mean(.x, na.rm = TRUE),
    variance = ~ var(.x,  na.rm = TRUE),
    uniquecount = ~ length(unique(.x)),
    freqratio = ~ {
      tbl <- table(.x)
      if(length(tbl) <= 1) return(Inf)
      max(tbl) / (sum(tbl) - max(tbl))
    }
  ), .names = "{.col}_{.fn}")) %>%
  tidyr::pivot_longer(dplyr::everything(), 
                      names_to = c("variable", "statistic"), 
                      names_pattern = "(.+)_(.+)",
                      values_to = "value") |>
  tidyr::pivot_wider(names_from = c("statistic"),
                     values_from = "value")

DT_stazioni_valori_2019 |>
  select(-c(unitamisura,idSensore,storico,
            datastart,datastop,utm_nord,utm_est)) |>
  pivot_wider(names_from = "nometiposensore",
              values_from = "Valore")


library(ggplot2)
strutture = read_sf(dsn = "C:/Users/nicol/Documents/REGIONE_LOMBARDIA/Strutture_sanitarie.shp")
ospedali = read_sf(dsn = "C:/Users/nicol/Documents/REGIONE_LOMBARDIA/Ospedali.shp")
distretti = read_sf(dsn = "C:/Users/nicol/Documents/Github_projects/spatialepi_biostats/data/DISTRETTI/geo_export_1aed6579-bb7e-4f89-a616-cc0af5ebb0f1.shp")

sf::st_crs(ospedali) <- 4326
sf::st_crs(distretti)
# Or transform to a common CRS
ospedali <- sf::st_transform(ospedali, crs = 4326)
sf::st_is_valid(ospedali)
ospedali <- sf::st_make_valid(ospedali)

sf::st_is_empty(ospedali)
sf::st_geometry_type(ospedali)
ospedali <- sf::st_simplify(ospedali, dTolerance = 10)

ospedali <- st_transform(ospedali, crs = st_crs("OGC:CRS84"))

gg_plt <- ggplot() +
  geom_sf(data = distretti,
    aes(geometry = geometry),
    #data = distretto,
    #aes(fill = Manganese),
    fill = 'white',
    color = 'black',
    linewidth = 0.1
  ) +
  geom_sf(data = ospedali,
          aes(geometry = geometry),
          fill = "green") +
  #labs(fill = "Manganese (µg/l):") +
  theme_void() +
  theme()


x = data.table(read_feather("s3://envbran/maps/hospitalization.arrow"))

# specialistica per erogatore
# https://www.dati.lombardia.it/resource/qm4z-s92m.csv

# ricoveri per erogatore
# https://www.dati.lombardia.it/resource/fwpe-xzv8.csv

# Specialistiche ATS milano download
# https://portalestatosalute.ats-milano.it/prestazioni.php

spec_erogatore = fread("https://www.dati.lombardia.it/resource/qm4z-s92m.csv?$limit=5000000")
spec_erogatore = spec_erogatore[anno == 2019]

locations = fread("G:/My Drive/Temporary/Prestazioni di specialistica ambulatoriale.csv")
locations = locations[,.(Codice,Nome,Indirizzo,Comune,Latitudine,Longitudine)]
locations = locations[!duplicated(locations)]

outersect(spec_erogatore$desc_struttura_erogazione,locations$Nome)
intersect(spec_erogatore$desc_struttura_erogazione,locations$Nome)

unique(spec_erogatore$desc_struttura_erogazione)



library(geosphere)
distHaversine(c(lon1, lat1), c(lon2, lat2)) / 1000 # Distance in km

