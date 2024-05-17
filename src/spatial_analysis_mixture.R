library(spdep)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(data.table)
color_scheme_set("brightblue")

source("src/hospitalization.R")

# Build adjagency matrix
district_nb = poly2nb(district,snap=0.0002)
district_nb = nb2mat(district_nb)
A = rowSums(district_nb != 0) * district_nb 

dt = merge(DTh, wq,
           by = c("district_id"),
           all = T)
dt$district_id = as.numeric(as.character(dt$district_id))
dt = dt[, lapply(.SD, function(x) replace(x, is.na(x), median(x, na.rm = TRUE)))]
dt[,':='(ratio_endo = 100*round(h_endo/h_all,2),
         ratio_heart = 100*round(h_heart/h_all,2),
         ratio_lungs = 100*round(h_lungs/h_all,2),
         Manganese_q = 10*ecdf(Manganese)(Manganese),
         Piombo_q = 10*ecdf(Piombo)(Piombo),
         Solfati_q = 10*ecdf(Solfati)(Solfati))]

check_cmdstan_toolchain(fix = TRUE)
file = "stan/poisson_m/poisson_w.stan"
mod = cmdstan_model(file)

x = as.matrix(dt[,.(Manganese_q,Piombo_q,Solfati_q)])
W = A

full_d = list(n = NROW(dt),                                 # number of observations
              p = 3,                                        # number of coefficients
              X = x,                                        # design matrix
              y = dt$h_heart,                                
              W = A)                                        # adjacency matrix

fit = mod$sample(
  data = full_d,
  chains = 4,
  thin = 1,
  iter_warmup = 500,
  iter_sampling = 1000,
  parallel_chains = 4,
  refresh = 100,
  max_treedepth = 20
)

saveRDS(fit,"src/heart_MnPbSol.rds")

results_table = lapply(1:NROW(pollutants),function(i){
  qtl10 = quantile(fit$draws("beta"),c(0.1))
  qtl90 = quantile(fit$draws("beta"),c(0.9))
  data.table(fit$summary("beta")[,2:6],
             q10 = qtl10,
             q90 = qtl90,
             fit$summary("beta")[,7:10])
})
results_table = rbindlist(results_table)
