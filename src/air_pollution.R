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

# Health district 
district =  geojson_read("https://www.dati.lombardia.it/resource/9n45-7bpc.geojson",
                         what = "sp")
dt_district = sf::st_as_sf(district)
dt_district = data.table::data.table(dt_district)
dt_district$objectid_1 = as.numeric(dt_district$objectid_1)

# Plot maps 

gg_plt <- dt_district |> 
  ggplot(aes(geometry = geometry)) +
  geom_sf(
    #data = distretto,
    #aes(fill = objectid_1),
    fill = "white",
    color = 'black',
    linewidth = 0.1
  ) +
  geom_sf(data = stazioni_loc,
    aes(geometry = geometry),
    color = 'red',
    size = 0.2
  ) +
  theme_void() +
  ggtitle("Locations of air quality stations") + 
  theme(plot.title = element_text(hjust = 0.5, size = 15))


## "Specialistica per erogatore"
spec_erogatore <- open_csv_dataset(
    "data/Specialistica_per_Erogatore_20261017.csv",
    col_types = schema(ANNO = int32(), COD_ATS_EROGAZIONE = string(),
                       COD_CLASSE_PRIORITA = string(), COD_BRANCA = string(),
                       CODICE_ENTE = string(), COD_STRUTTURA_EROGAZIONE = string(),
                       QTA_TOT = int64())
)

visite_urgenti_ats <- spec_erogatore |>
    filter(ANNO == 2025, COD_CLASSE_PRIORITA %in% c("U", "B"),
           COD_FARE_LIV_1 == "V",                        # visits only
           DESC_BRANCA %in% gruppi_inquinamento$DESC_BRANCA) |>
    group_by(COD_ATS_EROGAZIONE, DESC_ATS_EROGAZIONE, DESC_BRANCA, COD_CLASSE_PRIORITA) |>
    summarise(visite = sum(QTA_TOT), .groups = "drop") |>
    collect() |>
    left_join(gruppi_inquinamento, by = "DESC_BRANCA") |>
    group_by(COD_ATS_EROGAZIONE, DESC_ATS_EROGAZIONE, gruppo, COD_CLASSE_PRIORITA) |>
    summarise(visite = sum(visite), .groups = "drop") |>
    pivot_wider(names_from = COD_CLASSE_PRIORITA, values_from = visite, values_fill = 0) |>
    rename(molto_urgenti_U = U, urgenti_B = B) |>
    mutate(totale = molto_urgenti_U + urgenti_B) |>
    arrange(COD_ATS_EROGAZIONE, gruppo)

## Pulmonology + ENT visits per structure: urgent (U, B) vs non-urgent (D, P, no code)
visite_resp_strutture <- spec_erogatore |>
    filter(ANNO == 2025,
           COD_FARE_LIV_1 == "V",                                # visits only
           DESC_BRANCA %in% c("PNEUMOLOGIA", "OTORINOLARINGOIATRIA")) |>
    mutate(urgenza = if_else(COD_CLASSE_PRIORITA %in% c("U", "B"), "urgenti", "non_urgenti")) |>
    group_by(COD_ATS_EROGAZIONE, DESC_ATS_EROGAZIONE,
             COD_STRUTTURA_EROGAZIONE, DESC_STRUTTURA_EROGAZIONE, DESC_BRANCA, urgenza) |>
    summarise(visite = sum(QTA_TOT), .groups = "drop") |>
    collect() |>
    mutate(branca = if_else(DESC_BRANCA == "PNEUMOLOGIA", "pneumo", "orl")) |>
    select(-DESC_BRANCA) |>
    pivot_wider(names_from = c(branca, urgenza), values_from = visite, values_fill = 0) |>
    mutate(across(c(pneumo_urgenti, pneumo_non_urgenti, orl_urgenti, orl_non_urgenti), as.numeric),
           urgenti       = pneumo_urgenti + orl_urgenti,
           non_urgenti   = pneumo_non_urgenti + orl_non_urgenti,
           ratio_urgenti = if_else(non_urgenti > 0, urgenti / non_urgenti, NA_real_),
           quota_urgenti = urgenti / (urgenti + non_urgenti))

# Geolocation: verified coordinates, matched on structure code + name
strutture_geo <- read_parquet("data/specialistica_strutture_verified.parquet") |>
    select(cod_struttura, nome_struttura, lat, lon, geo_source,
           tipo_struttura, pubb_priv, desc_ente, ss_comune)

visite_resp_geo <- visite_resp_strutture |>
    left_join(strutture_geo,
              by = c("COD_STRUTTURA_EROGAZIONE" = "cod_struttura",
                     "DESC_STRUTTURA_EROGAZIONE" = "nome_struttura"))

# Only geolocated structures, as sf points
visite_resp_sf <- visite_resp_geo |>
    filter(!is.na(lat)) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)


library(terra)

pm25 <- rast("C:/Users/nicol/Documents/data/lombardia_specialistica/19304c41ac733604bd194c7c7e27d085/cams.eaq.ira.ENSa.pm2p5.l0.2025-01.nc")
lomb <- ext(8.4, 11.5, 44.6, 46.7)          # or ext(st_transform(dt_district, 4326)) for the real border
pm25_lomb <- crop(pm25, lomb)

pm25_daily   <- tapp(pm25_lomb, as.Date(time(pm25_lomb)), mean)   # 31 daily layers
pm25_monthly <- mean(pm25_lomb)                                    # one January layer

# value at each air-quality station or facility (sf points must be lon/lat)
pm25_at_pts <- extract(pm25_daily, vect(st_transform(stazioni_loc, 4326)), ID = FALSE)

# mean per health district
district_sf <- st_as_sf(district) |> st_transform(4326)
pm25_district <- extract(pm25_monthly, vect(district_sf), fun = mean, na.rm = TRUE)
# attach the means to the districts
district_sf$pm25_jan <- pm25_district[, 2]



## Download "Ricoveri per erogatore" (skipped if today's file already exists)
ricoveri_url <- "https://www.dati.lombardia.it/api/v3/views/fwpe-xzv8/query.csv"
ricoveri_csv <- file.path("data", paste0("Ricoveri_per_Erogatore_",
                                         format(Sys.Date(), "%Y%m%d"), ".csv"))

if (!file.exists(ricoveri_csv)) {
    dir.create("data", showWarnings = FALSE)
    options(timeout = 3600)   # ~600 MB file, default timeout is 60 s
    download.file(ricoveri_url, ricoveri_csv, mode = "wb")
}
