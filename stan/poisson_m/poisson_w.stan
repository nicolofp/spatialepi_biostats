data {
  int<lower=1> n;
  int<lower=1> p;
  matrix[n, p] X;
  array[n] int<lower=0> y;
  matrix<lower=0, upper=1>[n, n] W;
}
transformed data {
  vector[n] zeros;
  matrix<lower=0>[n, n] D;
  {
    vector[n] W_rowsums;
    for (i in 1 : n) {
      W_rowsums[i] = sum(W[i,  : ]);
    }
    D = diag_matrix(W_rowsums);
  }
  zeros = rep_vector(0, n);
}
parameters {
  simplex[p] we;
  vector[p] Dalp;
  real beta0;
  real beta1;
  vector[n] phi;
  real<lower=0> tau;
  real<lower=0, upper=1> alpha;
}
model {
  Dalp ~ gamma(2,2);
  we ~ dirichlet(Dalp);
  alpha ~ beta(2,2);
  phi ~ multi_normal_prec(zeros, tau * (D - alpha * W));
  beta1 ~ normal(0, 10);
  beta0 ~ normal(0, 10); 
  tau ~ gamma(2, 2);
  y ~ poisson_log(beta0 + beta1*(X*we) + phi);
}

