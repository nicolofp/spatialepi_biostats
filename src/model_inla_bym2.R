library(dplyr)
library(INLA)

# BYM2 negative binomial model (INLA) --------------------------------------
# y: urgent visits per structure, offset: log(total visits)
# covariates: PM2.5 and O3 (CAMS yearly mean at structure, standardised)
# spatial effect: BYM2 on the 86 health districts (structures nested in them)
m <- readRDS("data/model_data.rds")

# PM2.5 (urban/traffic) and O3 (rural/secondary) only: pm10, no2 and co are
# collinear with pm25 (r >= 0.86, VIF > 10), so2 adds little beyond no2
pollutants <- c("pm25_mean", "o3_mean")

dt_model <- m$visits |>
  filter(urgenti + non_urgenti > 0) |>
  mutate(
    y = urgenti,
    E = urgenti + non_urgenti,
    across(all_of(pollutants), ~ as.numeric(scale(.x)), .names = "z_{.col}")
  )

# Adjacency of the health districts (same order as district_idx)
g <- inla.read.graph(m$adj)

formula_bym2 <- y ~ 1 +
  z_pm25_mean + z_o3_mean +
  f(
    district_idx,
    model = "bym2",
    graph = g,
    scale.model = TRUE,
    constr = TRUE,
    values = m$districts$district_idx, # all 86 districts, also empty ones
    hyper = list(
      prec = list(prior = "pc.prec", param = c(1, 0.01)),
      phi = list(prior = "pc", param = c(0.5, 2 / 3))
    )
  )

fit_inla <- inla(
  formula_bym2,
  # negative binomial: Poisson dispersion was ~30 (structure heterogeneity)
  family = "nbinomial",
  data = dt_model,
  offset = log(E),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(compute = TRUE)
)

summary(fit_inla)

# Rate ratio per 1 SD increase of each pollutant
rr_inla <- fit_inla$summary.fixed |>
  as.data.frame() |>
  tibble::rownames_to_column("term") |>
  transmute(
    term,
    rr = exp(mean),
    rr_lower = exp(`0.025quant`),
    rr_upper = exp(`0.975quant`)
  )
rr_inla

# District relative risk: first 86 rows of the bym2 effect = total effect
district_re_inla <- fit_inla$summary.random$district_idx[
  seq_len(nrow(m$districts)),
] |>
  transmute(district_idx = ID, rr = exp(mean))

saveRDS(fit_inla, "data/fit_inla_bym2_nb.rds")
