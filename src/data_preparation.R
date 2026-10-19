library(readr)
library(vroom)
library(broom)
library(dplyr)
library(tidyr)
library(INLA)
library(sf)
library(arrow)
library(fmesher)
library(geojsonio)
library(ggplot2)

# Health district (86 different polygons) for Lombardy, Italy
district <- geojson_read(
  "https://www.dati.lombardia.it/resource/9n45-7bpc.geojson",
  what = "sp"
)
dt_district <- sf::st_as_sf(district)
dt_district <- data.table::data.table(dt_district)
dt_district$objectid_1 <- as.numeric(dt_district$objectid_1)

# "Specialistica per erogatore"
spec_erogatore <- open_csv_dataset(
  "data/Specialistica_per_Erogatore_20261017.csv",
  col_types = schema(ANNO = int32(),
                     COD_ATS_EROGAZIONE = string(),
                     COD_CLASSE_PRIORITA = string(),
                     COD_BRANCA = string(),
                     CODICE_ENTE = string(),
                     COD_STRUTTURA_EROGAZIONE = string(),
                     QTA_TOT = int64())
)

# Specialties related to air pollution
# (asthma has no dedicated specialty: it falls under pulmonology/allergy)
branche_inquinamento <- c(
  "PNEUMOLOGIA" = "pneumo",
  "OTORINOLARINGOIATRIA" = "orl",
  "DERMATOLOGIA/ALLERGOLOGIA" = "allergo"
)

visite_urgenti_ats <- spec_erogatore |>
  filter(
    ANNO == 2025,
    COD_CLASSE_PRIORITA %in% c("U", "B"),
    COD_FARE_LIV_1 == "V", # visits only
    DESC_BRANCA %in% names(branche_inquinamento)
  ) |>
  group_by(
    COD_ATS_EROGAZIONE,
    DESC_ATS_EROGAZIONE,
    DESC_BRANCA,
    COD_CLASSE_PRIORITA
  ) |>
  summarise(visite = sum(QTA_TOT), .groups = "drop") |>
  collect() |>
  pivot_wider(
    names_from = COD_CLASSE_PRIORITA,
    values_from = visite,
    values_fill = 0
  ) |>
  rename(molto_urgenti_U = U, urgenti_B = B) |>
  mutate(totale = molto_urgenti_U + urgenti_B) |>
  arrange(COD_ATS_EROGAZIONE, DESC_BRANCA)

# Pulmonology + ENT + allergy visits per structure: urgent vs non-urgent
visite_resp_strutture <- spec_erogatore |>
  filter(
    ANNO == 2025,
    COD_FARE_LIV_1 == "V", # visits only
    DESC_BRANCA %in% names(branche_inquinamento)
  ) |>
  mutate(
    urgenza = if_else(
      COD_CLASSE_PRIORITA %in% c("U", "B"), "urgenti", "non_urgenti"
    )
  ) |>
  group_by(
    COD_ATS_EROGAZIONE,
    DESC_ATS_EROGAZIONE,
    COD_STRUTTURA_EROGAZIONE,
    DESC_STRUTTURA_EROGAZIONE,
    DESC_BRANCA,
    urgenza
  ) |>
  summarise(visite = sum(QTA_TOT), .groups = "drop") |>
  collect() |>
  mutate(branca = unname(branche_inquinamento[DESC_BRANCA])) |>
  select(-DESC_BRANCA) |>
  pivot_wider(
    names_from = c(branca, urgenza),
    values_from = visite,
    values_fill = 0
  ) |>
  mutate(
    across(
      c(
        pneumo_urgenti, pneumo_non_urgenti,
        orl_urgenti, orl_non_urgenti,
        allergo_urgenti, allergo_non_urgenti
      ),
      as.numeric
    ),
    urgenti = pneumo_urgenti + orl_urgenti + allergo_urgenti,
    non_urgenti = pneumo_non_urgenti + orl_non_urgenti + allergo_non_urgenti,
    ratio_urgenti = if_else(
      non_urgenti > 0, urgenti / non_urgenti, NA_real_
    ),
    quota_urgenti = urgenti / (urgenti + non_urgenti)
  )

# Geolocation: verified coordinates, matched on structure code + name
strutture_geo <- read_parquet(
  "data/specialistica_strutture_verified.parquet"
) |>
  select(
    cod_struttura,
    nome_struttura,
    lat,
    lon,
    geo_source,
    tipo_struttura,
    pubb_priv,
    desc_ente,
    ss_comune
  )

visite_resp_geo <- visite_resp_strutture |>
  left_join(
    strutture_geo,
    by = c(
      "COD_STRUTTURA_EROGAZIONE" = "cod_struttura",
      "DESC_STRUTTURA_EROGAZIONE" = "nome_struttura"
    )
  )

# Only geolocated structures, as sf points
visite_resp_sf <- visite_resp_geo |>
  filter(!is.na(lat)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)


# Link each structure to its health district polygon
# (map_point returns the polygon id, which matches objectid_1)
source("src/spatial_library.R")
library(sp)

visite_resp_district <- visite_resp_geo |>
  filter(!is.na(lat)) |>
  rename(lng = lon) |>
  as.data.frame() |>
  map_point(space = district, out_name = "district_id") |>
  mutate(district_id = as.numeric(as.character(district_id))) |>
  rename(lon = lng)

# Yearly mean of the 6 CAMS pollutants at each structure location
# (value of the 0.1 deg grid cell containing the point; grid built by
# src/pollution_grid.R)
library(terra)

if (!file.exists("data/pollution_grid_year.parquet")) {
  source("src/pollution_grid.R")
}
pollution_rast <- read_parquet("data/pollution_grid_year.parquet") |>
  filter(year == 2025) |>
  select(lon, lat, ends_with("_mean")) |>
  as.data.frame() |>
  rast(type = "xyz", crs = "EPSG:4326")

visite_resp_district <- visite_resp_district |>
  bind_cols(
    terra::extract(
      pollution_rast,
      vect(visite_resp_district, geom = c("lon", "lat"), crs = "EPSG:4326"),
      ID = FALSE
    )
  )

# Final modelling dataset (INLA / Stan) -------------------------------------
# - visits: one row per geolocated structure, with counts, covariates,
#   district_idx (1..86, row of `districts`) and struttura_idx (1..n)
# - districts: all 86 district polygons (sf), ordered by district_idx,
#   including districts with no structures
# - nb / adj: queen-contiguity neighbours (spdep) and binary adjacency
#   matrix, in the same order as `districts`
library(spdep)

districts_sf <- st_as_sf(district) |>
  st_make_valid() |>
  mutate(objectid_1 = as.numeric(objectid_1)) |>
  arrange(objectid_1) |>
  mutate(district_idx = row_number())

# snap ~10 m: the Milan Municipi polygons have a small gap with the
# surrounding districts, without it Milan is a separate graph component
districts_nb <- poly2nb(districts_sf, queen = TRUE, snap = 1e-4)
districts_adj <- nb2mat(districts_nb, style = "B", zero.policy = TRUE)

visits_model <- visite_resp_district |>
  left_join(
    st_drop_geometry(districts_sf) |>
      select(objectid_1, district_idx, distretto, codice_ats),
    by = c("district_id" = "objectid_1")
  ) |>
  mutate(
    struttura_idx = row_number(),
    # district in the structure's own ATS: FALSE flags likely wrong geocodes
    ats_match = paste0("030", COD_ATS_EROGAZIONE) == codice_ats
  )

saveRDS(
  list(
    visits = visits_model,
    districts = districts_sf,
    nb = districts_nb,
    adj = districts_adj
  ),
  "data/model_data.rds"
)
