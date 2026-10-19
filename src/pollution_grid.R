library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(arrow)

# CAMS European AQ interim reanalysis, ensemble, surface level, 0.1 deg grid,
# already cropped to Lombardy (lon 8.4-11.5, lat 44.6-46.7).
# One NetCDF per month; for each pollutant: monthly mean of daily means,
# plus n_days / n_hours with valid data.
cams_dir <- "C:/Users/nicol/Documents/data/cams_aq_2025/lombardia_netcdf"

pollutants <- c(
  "particulate_matter_2.5um" = "pm25",
  "particulate_matter_10um" = "pm10",
  "nitrogen_dioxide" = "no2",
  "ozone" = "o3",
  "carbon_monoxide" = "co",
  "sulphur_dioxide" = "so2"
)

# Health districts (86 polygons) for Lombardy, to link grid cells
district_sf <- st_read("data/DISTRETTI.geojson", quiet = TRUE) |>
  st_transform(4326) |>
  mutate(objectid_1 = as.numeric(objectid_1))

cams_files <- list.files(cams_dir, "_monthly_mean[.]nc$", full.names = TRUE)

# Long table: one row per grid cell x month x pollutant
cell_month <- lapply(cams_files, function(f) {
  r <- rast(f)
  grid <- as.data.frame(r[[1]], xy = TRUE, cells = TRUE, na.rm = FALSE)[1:3]
  lapply(names(pollutants), function(v) {
    tibble(
      grid,
      variable = v,
      mean = values(r[[v]])[, 1],
      n_days = values(r[[paste0(v, "_n_days")]])[, 1],
      n_hours = values(r[[paste0(v, "_n_hours")]])[, 1],
      year = as.integer(format(time(r)[1], "%Y")),
      month = as.integer(format(time(r)[1], "%m"))
    )
  }) |>
    bind_rows()
}) |>
  bind_rows() |>
  mutate(pollutant = unname(pollutants[variable]))

# Yearly mean per pollutant per grid cell = mean of all valid daily means
# (monthly means weighted by their number of valid days).
# Coverage: valid days / months contributing to the yearly mean.
pollution_grid <- cell_month |>
  filter(!is.na(mean), n_days > 0) |>
  group_by(cell, x, y, year, pollutant) |>
  summarise(
    mean = sum(mean * n_days) / sum(n_days),
    n_days = sum(n_days),
    n_months = n_distinct(month),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = pollutant,
    values_from = c(mean, n_days, n_months),
    names_glue = "{pollutant}_{.value}"
  ) |>
  rename(lon = x, lat = y)

# District of each cell centre (NA = cell centre outside Lombardy)
cell_pts <- st_as_sf(
  pollution_grid,
  coords = c("lon", "lat"), crs = 4326, remove = FALSE
)
within_idx <- st_within(cell_pts, district_sf)
pollution_grid$district_id <- district_sf$objectid_1[
  vapply(within_idx, \(i) if (length(i)) i[1] else NA_integer_, integer(1))
]

write_parquet(pollution_grid, "data/pollution_grid_year.parquet")
