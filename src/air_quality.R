library(data.table)
library(lubridate)
library(duckplyr)
library(readxl)
library(dplyr)
library(arrow)
library(tidyr)
library(sf)

source("src/key_amazon.R")
source("src/spatial_library.R")

## Air quality
stazioni = fread("https://www.dati.lombardia.it/resource/ib47-atvt.csv",
                 encoding = "UTF-8")
stazioni_valid = stazioni |>
  as_duckdb_tibble() |>
  filter(datastop > as.POSIXct("2020-01-01") | is.na(datastop)) |>
  select(idsensore,nometiposensore,unitamisura,idstazione,comune,lat,lng) 

valori = open_dataset("s3://envbran/AQ/Dati_sensori_aria_2018_2023_20241221.arrow",
                      format = "arrow")
DT_stazioni_valori_2019 = valori |>
  filter(year(Data) == 2019 & 
           idSensore %in% stazioni_valid$idsensore & 
           Valore > 0) |>
  collect() |>
  group_by(idSensore) |>
  summarise(across(Valore, ~ mean(.x))) |>
  inner_join(stazioni_valid, by = c("idSensore" = "idsensore")) 

DT_stazioni_valori_2019 |>
  select(-c(unitamisura,idSensore)) |>
  pivot_wider(names_from = "nometiposensore",
              values_from = "Valore") |> 
  summarise(across(!c(idstazione,comune,lat,lng), list(
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
                      ), .names = "{.col}_{.fn}")) |>
  tidyr::pivot_longer(dplyr::everything(), 
                      names_to = c("variable", "statistic"), 
                      names_pattern = "(.+)_(.+)",
                      values_to = "value") |>
  tidyr::pivot_wider(names_from = c("statistic"),
                     values_from = "value")



DT_stazioni_valori_2019 |>
  select(-c(idSensore,unitamisura)) |>
  filter(nometiposensore %in% c("Biossido di Azoto",
                                "Ossidi di Azoto",
                                "PM10 (SM2005)")) |>
  pivot_wider(names_from = "nometiposensore",
              values_from = "Valore") |> View()

####################################################################################
get_bucket('envbran')
X = read_feather("s3://envbran/AQ/Anagrafica_stime_comunali.arrow")
Y = read_feather("s3://envbran/AQ/Dati_stime_comunali.arrow")

X_red = X |>
  mutate(Data = as.Date(Data, format = "%m/%d/%Y %H:%M:%S"),
         Anno = year(Data)) |>
  filter(Anno == 2019 & Valore > 0 & idOperatore != 12) |>
  select(-c(Data,idOperatore)) |>
  group_by(idSensore, Anno) |>
  summarise(Valore = mean(Valore, na.rm = T)) |>
  ungroup() 

Y_red = Y |>
  inner_join(X_red, by = c("IdSensore" = "idSensore")) |>
  select(Idstazione,Provincia,Comune,Anno,NomeTipoSensore,Valore) |>
  pivot_wider(names_from = NomeTipoSensore,
              values_from = Valore) |>
  unnest() |>
  group_by(Idstazione,Provincia,Comune,Anno) |>
  summarize(across(c(`Biossido di Azoto`, Ozono, 
                     PM10, `Particelle sospese PM2.5`),
                   mean,na.rm = T)) |>
  ungroup()

## Fix comuni
comuni = read_feather("data/municipalities.arrow")
# 020072 - BORGO MANTOVANO - Pieve di Coriano, Revere and Villa Poma
# 013254 - CENTRO VALLE INTELVI - Casasco d'Intelvi, Castiglione d'Intelvi and San Fedele Intelvi

comuni = comuni |>
  add_row(istat = c(20072,13254),
          comune = c("Borgo Mantovano","Centro Valle Intelvi"),
          lng = c("11.038439","8.9794707"),
          lat = c("45.0243268","45.9449504"))

test_point = Y_red |> 
  select(Comune) |>
  distinct() |> # 1567
  inner_join(comuni, by = c("Comune" = "comune")) |>
  select(Comune,lng,lat) |>
  mutate(lng = as.numeric(lng),
         lat = as.numeric(lat))
  

# Fix distretti
#############################################
distretti = read_sf(dsn = "data/DISTRETTI/geo_export_1aed6579-bb7e-4f89-a616-cc0af5ebb0f1.shp")
# combine polygon all together for Milano - different municipi
tmp = st_union(distretti[78:86,])
new_row <- st_sf(
  objectid_1 = 78,
  distretto = "MILANO",
  codice_ats = "030321",
  descrizion = "ATS CITTA' METROPOLITANA DI MILANO",
  shape_leng = NA,
  shape_area = NA,
  shape_len = NA,
  geometry = st_cast(tmp, "MULTIPOLYGON"),  # Ensure MULTIPOLYGON type
  crs = st_crs(distretti)  # Match CRS
)
distretti <- rbind(distretti[1:77,],new_row)
distretti <- st_transform(distretti, crs = 4326)
points_sf <- st_as_sf(test_point, 
                      coords = c("lng", "lat"), 
                      crs = 4326)

sf_use_s2(FALSE)  # Disable s2 to avoid geometry errors, consider plane not curve
                  # good approximation given the distances 
within_result <- st_within(points_sf, distretti, sparse = FALSE)
sf_use_s2(TRUE)   # Re-enable s2

colnames(within_result) <- distretti$distretto
rownames(within_result) <- test_point$Comune

test_dt = data.table(melt(within_result))
test_dt = test_dt[value == T,.(comune = as.character(Var1),
                     distretto = as.character(Var2))]

# Final merge AQ
DT = Y_red |>
  inner_join(test_dt, by = c("Comune"="comune")) |>
  right_join(distretti, by = "distretto") 


# Add distretti indication to health data
health = read_feather("src/pulmonary_visits.arrow")
points_h <- st_as_sf(health |> rename("LONG" = "LONG_"), 
                     coords = c("LONG", "LAT"), 
                     crs = 4326)

sf_use_s2(FALSE)  # Disable s2 to avoid geometry errors, consider plane not curve
# good approximation given the distances 
within_result <- st_within(points_h, distretti, sparse = FALSE)
sf_use_s2(TRUE)   # Re-enable s2

colnames(within_result) <- distretti$distretto
rownames(within_result) <- 1:166

test_dt = data.table(melt(within_result))
test_dt = test_dt[value == T,.(ID_row = as.numeric(Var1),
                               distretto = as.character(Var2))]
setkey(test_dt,ID_row)

DT_health = bind_cols(test_dt,health)
DT_health = DT_health |>
  inner_join(distretti, by = c("distretto"))

DT_health |> 
  group_by(objectid_1) |>
  summarise(N_tot = n(),
            Max = max(u_ratio),
            Min = min(u_ratio),
            Mean = mean(u_ratio)) |> 
  ggplot(aes(x = Mean)) +
  geom_histogram()


#Data = 
DT |> 
  select(distretto,objectid_1,`Biossido di Azoto`,
         Ozono,PM10,`Particelle sospese PM2.5`) |>
  group_by(distretto,objectid_1) |>
  summarise(across(everything(), mean, na.rm = T)) |>
  ungroup() |>
  inner_join(DT_health |>
               group_by(distretto,objectid_1) |>
               summarise(u_ratio = mean(u_ratio, na.rm = T)) |>
               ungroup(), 
             by = c("distretto","objectid_1")) |>
  ggplot(aes(x = u_ratio, y = `Particelle sospese PM2.5`)) +
  geom_point() +
  geom_smooth(method = "lm")
  
# test tokens

