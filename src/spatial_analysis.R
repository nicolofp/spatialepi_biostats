library(spdep)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(data.table)
color_scheme_set("brightblue")

source("src/health_data.R")
source("src/pollution_data.R")

# Build adjagency matrix
district_nb = poly2nb(district,snap=0.0002)
district_nb = nb2mat(district_nb)
A = rowSums(district_nb != 0) * district_nb 

check_cmdstan_toolchain(fix = TRUE)
file = "stan/spatial_regression.stan"
mod = cmdstan_model(file)

dt = merge(DT_health, pollution,
           by = c("district_id","codice_ats",
                  "distretto","descrizion"),
           all = T)

pollutants = names(dt)[8:38]
# tmp_res = lapply(pollutants, function(i){
#   x = as.matrix(pollution[,c(i),with=F])
#   W = A 
#   X = apply(x,2,scale)
#   
#   full_d = list(n = NROW(dt),                                 # number of observations
#                 p = 1,                                        # number of coefficients
#                 X = X,                                        # design matrix
#                 y = dt$ratio,                                 # observed number of cases
#                 log_offset = log(rep(13,NROW(dt))),           # log(expected) num. cases
#                 W = A)                                        # adjacency matrix
#   
#   fit = mod$sample(
#     data = full_d,
#     chains = 4,
#     thin = 1,
#     iter_warmup = 500,
#     iter_sampling = 1000,
#     parallel_chains = 4,
#     refresh = 100,
#     max_treedepth = 20
#   )
#   
#   return(fit)
# })
# saveRDS(tmp_res,"src/results.rds")

fit = readRDS("src/results.rds")
results_table = lapply(1:NROW(pollutants),function(i){
  qtl10 = quantile(fit[[i]]$draws("beta"),c(0.1))
  qtl90 = quantile(fit[[i]]$draws("beta"),c(0.9))
  data.table(variable = pollutants[i],
             fit[[i]]$summary("beta")[,2:6],q10 = qtl10,
             q90 = qtl90,fit[[i]]$summary("beta")[,7:10])
})
results_table = rbindlist(results_table)
