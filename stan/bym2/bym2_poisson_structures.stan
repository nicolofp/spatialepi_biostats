// BYM2 Poisson model with structure-level observations nested in districts.
// y[i]: urgent visits of structure i, offset: total visits (urgent + non-urgent)
// The spatial effect is defined on the district graph (N_district nodes);
// several structures can share the same district, districts without
// structures are informed by their neighbours only.
data {
  int<lower=0> N; // num structures (observations)
  int<lower=1> N_district; // num districts (graph nodes)
  int<lower=0> N_edges;
  array[N_edges] int<lower=1, upper=N_district> node1; // node1[i] adjacent to node2[i]
  array[N_edges] int<lower=1, upper=N_district> node2; // and node1[i] < node2[i]
  array[N] int<lower=1, upper=N_district> district; // district of each structure

  array[N] int<lower=0> y; // urgent visits
  vector<lower=0>[N] E; // offset: total visits
  int<lower=1> K; // num covariates
  matrix[N, K] x; // design matrix (standardised pollutants)

  real<lower=0> scaling_factor; // scales the variance of the spatial effects
}
transformed data {
  vector[N] log_E = log(E);
}
parameters {
  real beta0; // intercept
  vector[K] betas; // covariates

  real<lower=0> sigma; // overall standard deviation
  real<lower=0, upper=1> rho; // proportion spatially structured vs. unstructured variance

  vector[N_district] theta; // heterogeneous effects
  vector[N_district] phi; // spatial effects
}
transformed parameters {
  vector[N_district] convolved_re;
  // variance of each component should be approximately equal to 1
  convolved_re = sqrt(1 - rho) * theta + sqrt(rho / scaling_factor) * phi;
}
model {
  y ~ poisson_log(log_E + beta0 + x * betas + convolved_re[district] * sigma);

  // ICAR prior for phi (up to proportionality)
  target += -0.5 * dot_self(phi[node1] - phi[node2]);
  // soft sum-to-zero constraint on phi
  sum(phi) ~ normal(0, 0.001 * N_district); // equivalent to mean(phi) ~ normal(0,0.001)

  beta0 ~ normal(0.0, 2.5); // log share of urgent visits: centred on 0, wide
  betas ~ normal(0.0, 1.0);
  theta ~ normal(0.0, 1.0);
  sigma ~ normal(0, 1.0);
  rho ~ beta(0.5, 0.5);
}
generated quantities {
  real logit_rho = log(rho / (1.0 - rho));
  vector[K] rr = exp(betas); // rate ratio per 1 SD of each pollutant
  vector[N_district] district_rr = exp(convolved_re * sigma); // district relative risk
  vector[N] mu = exp(log_E + beta0 + x * betas + convolved_re[district] * sigma);
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = poisson_log_lpmf(y[i] | log(mu[i]));
  }
}
