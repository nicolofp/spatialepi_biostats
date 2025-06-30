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


# Health district 
district =  geojson_read("https://www.dati.lombardia.it/resource/9n45-7bpc.geojson",
                         what = "sp")
dt_district = sf::st_as_sf(district)
#dt_district = tidy(district)
# dt_district = data.table(dt_district)
# dt_district = merge(dt_district,
#                     district@data[,c("objectid_1","codice_ats","distretto","descrizion")],
#                     by.x = "id", by.y = "objectid_1")
dt_district$objectid_1 = as.numeric(dt_district$objectid_1)

rico_erogatore = fread("https://www.dati.lombardia.it/resource/fwpe-xzv8.csv?$limit=5000000")
strutture <- rico_erogatore |> 
  select(descrizione_struttura,latitudine_norm,longitudine_norm) |>
  distinct() |>
  drop_na() |>
  st_as_sf(coords = c("longitudine_norm", 
                         "latitudine_norm"),
         crs = 4326)

strutture = read_sf(dsn = "C:/Users/nicol/Documents/REGIONE_LOMBARDIA/Strutture_sanitarie.shp")
ospedali = read_sf(dsn = "C:/Users/nicol/Documents/REGIONE_LOMBARDIA/Ospedali.shp")
ospedali <- st_transform(ospedali, crs = 4326)

ospedali <- st_simplify(ospedali, dTolerance = 0.001)
dt_district |> 
  ggplot(aes(geometry = geometry)) +
  geom_sf(fill = 'white',color = 'black',
          linewidth = 0.1) +
  geom_sf(data = strutture,
          aes(geometry = geometry),
          size = 0.1) + 
  theme_void() +
  theme()

spec_erogatore |>
  #filter(anno == 2019) |>
  select(desc_branca,desc_fare_liv_1,desc_fare_liv_2,desc_fare_liv_3) |>
  distinct() |> View()


list_location = spec_erogatore |>
  filter(anno == 2019) |>
  #& desc_branca == "PNEUMOLOGIA" &
  #          desc_fare_liv_1 == "VISITE") |>
  filter(desc_fare_liv_1 == "VISITE") |>
  select(desc_struttura_erogazione,desc_ats_erogazione,desc_ente) |>
  distinct() |>
  mutate(ID = row_number())
list_location |> View()

# Load data from downloads
Y = fread("G:/My Drive/Temporary/Prestazioni di specialistica ambulatoriale.csv")
Y <- Y |> select(Nome,Comune,Latitudine = Longitudine,Longitudine = Latitudine) |> distinct()

testA = merge(list_location,Y, by.x = "desc_struttura_erogazione", by.y = "Nome") 
testA |> 
  group_by(ID) |>
  summarize(count = n()) |>
  filter(count > 1) |>
  inner_join(testA, by = "ID") |>
  do({ assign("multiple_loc", ., envir = .GlobalEnv); . }) 
  
# 755 locations - total
# 471 locations - mapped univocally
#  83 locations - multiple mapping - 22 can be mapped (same circle)
#                                    61 can be manually mapped
multiple_loc |> select(desc_struttura_erogazione) |> distinct()

test_multi <- future_map_dfr(unique(multiple_loc$desc_struttura_erogazione), function(i) {
  print(i)
  multiple_loc |>
    filter(desc_struttura_erogazione == i) |>
    mutate(check = n_points_in_circle(cbind(Latitudine,Longitudine),5))
}, .options = furrr_options(seed = TRUE))

test_multi |> filter(check == T) |> 
  select(desc_struttura_erogazione) |> 
  distinct() |>
  View()





######################################################################
# Data from shapefile
strutture_r <- strutture |>
  as.data.table() |>
  select(NOME_STRUT,COMUNE,LAT,LONG_) |>
  rename("Nome" = "NOME_STRUT",
         "Comune" = "COMUNE",
         "Latitudine" = "LAT",
         "Longitudine" = "LONG_") |>
  distinct()
strutture_master <- bind_rows(Y,strutture_r) |> distinct()

testB = merge(list_location,strutture_r, by.x = "desc_struttura_erogazione", by.y = "Nome") 

# Try to map list_location with strutture_master
# 1. Check for exact match
# 2. Using fuzzy matching LV distance 

# 1. Check for exact match 
library(furrr)
library(purrr)

# Set up parallel processing
plan(multisession, workers = availableCores() - 1)  # Use all cores except one

result_exact <- future_map_dfr(seq_along(list_location$desc_struttura_erogazione), function(i) {
  print(i)
  strutture_master |>
    filter(grepl(list_location$desc_struttura_erogazione[i], 
                 Nome, ignore.case = TRUE)) |>
    mutate(original = list_location$desc_struttura_erogazione[i])
}, .options = furrr_options(seed = TRUE))

# result_exact <- purrr::map_dfr(seq_along(list_location$desc_struttura_erogazione), function(i) {
#   print(i)
#   strutture_master |>
#     filter(grepl(list_location$desc_struttura_erogazione[i], 
#                  Nome, ignore.case = TRUE)) |>
#     #filter(Nome %like% list_location$desc_struttura_erogazione[i])|>
#     mutate(original = list_location$desc_struttura_erogazione[i])
# })

result_exact |> 
  group_by(original) |>
  summarize(count = n()) |>
  filter(count == 1) |>
  inner_join(result_exact, by = "original") |>
  write_feather("erogatori_loc_exact.arrow")


erogatori_loc_exact = read_feather("erogatori_loc_exact.arrow") 
erogatori_loc_exact |>
  View()

# Now clean mapping tools from mapped names
list_location <- list_location |> 
  filter(!(desc_struttura_erogazione %in% erogatori_loc_exact$original))
strutture_master <- strutture_master |> 
  filter(!(Nome %in% erogatori_loc_exact$Nome))

result_exact |> 
  group_by(original) |>
  summarize(count = n()) |>
  filter(count == 2) |>
  inner_join(result_exact, by = "original") |>
  #do({ assign("test_coordinates", ., envir = .GlobalEnv); . }) 
  group_by(original) |>
  mutate(ID = row_number()) |>
  ungroup() |>
  select(-c(Nome,Comune)) |>
  pivot_wider(names_from = ID,
              values_from = c(Latitudine,Longitudine)) |>
  mutate(distance_km = distVincentySphere(
    cbind(Longitudine_1, Latitudine_1),  # First point coordinates
    cbind(Longitudine_2, Latitudine_2)   # Second point coordinates
  ) / 1000) |> 
  do({ assign("names_2location", ., envir = .GlobalEnv); . }) 
  
# if the two location are less than 10km apart we can randomly pick
# the coordinates of one of the two (we are interested in the district)
# --> potentially we can the same for location with more than 2 points

valid_loc2 = names_2location |> filter(distance_km < 10) |> select(original)
result_exact |> 
  group_by(original) |>
  summarize(count = n()) |>
  filter(count == 2) |>
  inner_join(result_exact, by = "original") |>
  group_by(original) |>
  mutate(ID = row_number()) |>
  ungroup() |>
  filter(original %in% valid_loc2$original & ID == 1) |>
  write_feather("erogatori_loc_2point.arrow")

erogatori_loc_2point = read_feather("erogatori_loc_2point.arrow")

# Analyze problematic 2 point location
names_2location |> filter(distance_km > 10) |> View()
result_exact |> filter(original %like% "SERVICE LAB FLEMING RESEARCH") |> View()
result_exact |> filter(original %like% "CPS 2 - CONCESIO") |> View()
result_exact |> filter(original %like% "FONDAZIONE SALVATORE MAUGERI") |> View() #BRESCIA
result_exact |> 
  filter((original %like% "CENTRO DIALISI AD ASSISTENZA LIMITATA" & Comune %like% "ROMANO") |
           (original %like% "FONDAZIONE SALVATORE MAUGERI" & Comune == "LUMEZZANE") |
           (original %like% "SERVICE LAB FLEMING RESEARCH" & Latitudine != 0) |
           (original %like% "CPS 2 - CONCESIO" & Comune == "NAVE")) |>
  write_feather("erogatori_loc_2point_problems.arrow") 

erogatori_loc_2point_problems = read_feather("erogatori_loc_2point_problems.arrow") 

# Start cleaning location with 3+ points
# Now clean mapping tools from mapped names
list_location <- list_location |> 
  filter(!(desc_struttura_erogazione %in% c(erogatori_loc_2point$original,
                                            erogatori_loc_2point_problems$original)))
strutture_master <- strutture_master |> 
  filter(!(Nome %in% c(erogatori_loc_2point$Nome,
                       erogatori_loc_2point_problems$Nome)))
  
result_exact |> 
  group_by(original) |>
  summarize(count = n()) |>
  filter(count>=3) |>
  inner_join(result_exact, by = "original") |>
  do({ assign("names_3more_pointlocation", ., envir = .GlobalEnv); . }) 

test1 <- future_map_dfr(unique(names_3more_pointlocation$original), function(i) {
  print(i)
  names_3more_pointlocation |>
    filter(original == i) |>
    mutate(check = n_points_in_circle(cbind(Latitudine,Longitudine),5))
  }, .options = furrr_options(seed = TRUE))

test1 |> 
  group_by(original) |>
  mutate(ID = row_number()) |>
  ungroup() |>
  filter(check == T & ID == 1) |>
  write_feather("erogatori_loc_3more_point_circle5.arrow")
  
erogatori_loc_3more_point_circle5.arrow = read_feather("erogatori_loc_3more_point_circle5.arrow")




test1 |> 
  group_by(original) |>
  mutate(ID = row_number()) |>
  ungroup() |>
  filter(check == F) |>
  do({ assign("test2", ., envir = .GlobalEnv); . }) 
  
  
  #write.csv("G:/My Drive/Temporary/manual_mapping.csv")

spec_erogatore |> 
  select(desc_struttura_erogazione,desc_ats_erogazione,desc_ente) |>
  distinct() |>
  filter(desc_struttura_erogazione %in% unique(test2$original)) |>
  inner_join(test2, by = c("desc_struttura_erogazione" = "original")) |>
  write.csv("G:/My Drive/Temporary/manual_mapping.csv")


# Remove - too generic
# POLIAMBULATORIO SPECIALISTICO - 106 match
# CAL - 696
# C.A.L. - 116
# POLIAMBULATORIO - 789
# CPS - 136
# RIABILITA - 116
# NEUROPSICHIATRIA INFANTILE - 135
# 

final = fread("C:/Users/nicol/Documents/mapping_final.csv")

names(final)
names(erogatori_map)
final <- final |>
  select(desc_struttura_erogazione,count,Nome,Comune,Latitudine,Longitudine,ID,check) |>
  rename("original" = "desc_struttura_erogazione")

erogatori_map = bind_rows(erogatori_loc_exact,
                          erogatori_loc_2point,
                          erogatori_loc_2point_problems,
                          erogatori_loc_3more_point_circle5.arrow,
                          final)
erogatori_map |> 
  full_join(list_location, by = c("original" = "desc_struttura_erogazione")) |> 
  View()


