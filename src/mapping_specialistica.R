library(data.table)
library(duckplyr)
library(tidyr)
library(geojsonsf)
library(lubridate)
library(geojsonio)
library(gtExtras)
library(ggiraph)
library(ggplot2)
library(viridis)
library(duckdb)
library(readxl)
library(dplyr)
library(arrow)
library(broom)
library(sp)
library(sf)

source("src/spatial_library.R")
source("src/key_amazon.R")

# load data for mapping
strutture = read_sf(dsn = "C:/Users/nicol/Documents/REGIONE_LOMBARDIA/Strutture_sanitarie.shp")
spec_erogatore = fread("https://www.dati.lombardia.it/resource/qm4z-s92m.csv?$limit=5000000")

# We are interrested in visits only
erogatore_location = spec_erogatore |>
  #filter(desc_fare_liv_1 == "VISITE" & anno == 2019) |>
  filter(desc_fare_liv_1 == "VISITE" & anno == 2019 & desc_branca == "PNEUMOLOGIA") |>
  select(desc_struttura_erogazione,desc_ats_erogazione,desc_ente) |>
  distinct() |>
  mutate(ID_erogatore = row_number(),
         desc_struttura_erogazione = trimws(gsub('\\s*\\([^)]+\\)\\"', "", 
                                                 desc_struttura_erogazione)))

strutture = strutture |>
  select(NOME_STRUT,DS_ATS,DS_ASST,COMUNE,PROV,LAT,LONG_) |>
  mutate(ID_struttura = row_number(),
         NOME_STRUT = trimws(gsub('\\s*\\([^)]+\\)\\"', "", NOME_STRUT))) |>
  as.data.table()

library(furrr)
library(purrr)

# Set up parallel processing
plan(multisession, workers = availableCores() - 1)  # Use all cores except one

result_exact <- future_map_dfr(seq_along(erogatore_location$desc_struttura_erogazione), function(i) {
  print(i)
  strutture |>
    filter(grepl(erogatore_location$desc_struttura_erogazione[i], 
                 NOME_STRUT, ignore.case = TRUE) &
             DS_ATS == erogatore_location$desc_ats_erogazione[i]) |>
    mutate(original = erogatore_location$desc_struttura_erogazione[i],
           ID_erogatore = erogatore_location$ID_erogatore[i])
}, .options = furrr_options(seed = TRUE))

test1 <- future_map_dfr(unique(result_exact$original), function(i) {
  print(i)
  result_exact |>
    filter(original == i) |>
    mutate(check = n_points_in_circle(cbind(LAT,LONG_),5))
}, .options = furrr_options(seed = TRUE))

# n = numer of elements per group
test1 <- test1 |> add_count(original)
test1 <- test1 |> group_by(original) |> mutate(ID_group = row_number()) |> ungroup()
test1 |> View()
test1 |> filter(check == T & n == 1) |> select(original) |> distinct() #419 unique 
test1 |> filter(check == T & n > 1) |> select(original) |> distinct() #38 unique (select one random location)
test1 |> filter(check == F & n > 1) |> select(original) |> distinct() #16 exclude 

test1 |> filter(check == T & n == 1) |> write.csv("mapped_auto1.csv", row.names = F)
test1 |> filter(check == T & n > 1 & ID_group == 1) |> write.csv("mapped_auto2.csv", row.names = F)

file_list <- c("mapped_auto1.csv", "mapped_auto2.csv")
mapped_auto <- rbindlist(lapply(file_list, fread))

mapped_manual = fread("erogatore_location2.csv")
mapped_manual = mapped_manual |>
  filter(!is.na(LAT)) |> 
  rename("original" = "desc_struttura_erogazione")
  

mapped = bind_rows(mapped_auto,mapped_manual)

final_test = spec_erogatore |>
  filter(desc_fare_liv_1 == "VISITE" & anno == 2019 & desc_branca == "PNEUMOLOGIA") |>
  mutate(desc_struttura_erogazione = trimws(gsub('\\s*\\([^)]+\\)\\"', "", 
                                                 desc_struttura_erogazione))) |>
  inner_join(mapped |> select(original,LAT,LONG_) |> distinct(), 
             by = c("desc_struttura_erogazione" = "original")) 

final_test = final_test |>
  select(desc_struttura_erogazione,cod_class_priorita,qta_tot,LAT,LONG_) |>
  group_by(LAT,LONG_,cod_class_priorita) |>
  summarise(qta_visit = sum(qta_tot)) |>
  ungroup() |>
  mutate(cod_class_priorita = ifelse(cod_class_priorita == "","N",
                                     cod_class_priorita)) |>
  pivot_wider(names_from = cod_class_priorita,
              values_from = qta_visit)

final_test = final_test |> mutate(u_ratio = (U+B)/(N+B+P+U))
final_test |> ggplot(aes(x = u_ratio)) +
  geom_histogram()




