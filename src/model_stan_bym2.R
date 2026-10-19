library(dplyr)
library(cmdstanr)
library(posterior)
library(INLA)
library(Matrix)

# BYM2 Poisson model (Stan) ------------------------------------------------
# y: urgent visits per structure, offset: total visits
# covariates: 5 CAMS pollutants (yearly mean at structure, standardised)
# spatial effect: BYM2 on the 86 health districts (structures nested in them)
m <- readRDS("data/model_data.rds")

# pm10 excluded: collinear with pm25 (r = 1.00 at structure locations)
pollutants <- c(
  "pm25_mean", "no2_mean", "o3_mean", "co_mean", "so2_mean"
)

dt_model <- m$visits |>
  filter(urgenti + non_urgenti > 0)

x <- scale(as.matrix(dt_model[, pollutants]))

# Edge list of the health-district adjacency (node1 < node2)
adj <- m$adj
edges <- which(adj == 1 & upper.tri(adj), arr.ind = TRUE)
N_district <- nrow(adj)

# BYM2 scaling factor: geometric mean of the ICAR marginal variances
Q <- Diagonal(N_district, rowSums(adj)) - Matrix(adj, sparse = TRUE)
Q_pert <- Q + Diagonal(N_district) * max(diag(Q)) * sqrt(.Machine$double.eps)
Q_inv <- inla.qinv(Q_pert, constr = list(A = matrix(1, 1, N_district), e = 0))
scaling_factor <- exp(mean(log(diag(Q_inv))))

stan_data <- list(
  N = nrow(dt_model),
  N_district = N_district,
  N_edges = nrow(edges),
  node1 = edges[, 1],
  node2 = edges[, 2],
  district = dt_model$district_idx,
  y = as.integer(dt_model$urgenti),
  E = dt_model$urgenti + dt_model$non_urgenti,
  K = ncol(x),
  x = unname(x),
  scaling_factor = scaling_factor
)

mod <- cmdstan_model("stan/bym2/bym2_poisson_structures.stan")

fit_stan <- mod$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  seed = 2025
)

fit_stan$diagnostic_summary()

# Rate ratio per 1 SD increase of each pollutant
rr_stan <- fit_stan$summary(
  c("beta0", "betas", "sigma", "rho", "rr"),
  "mean", ~ quantile(.x, c(0.025, 0.975)), "rhat", "ess_bulk"
)
rr_stan$term <- c(
  "beta0", paste0("beta_", pollutants), "sigma", "rho",
  paste0("rr_", pollutants)
)
rr_stan

fit_stan$save_object("data/fit_stan_bym2.rds")
