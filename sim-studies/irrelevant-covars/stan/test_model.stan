data {
  int<lower=0> N;   // number of data items
  int<lower=0> K;   // number of predictors
  matrix[N, K] x;   // predictor matrix
  vector[N] y;      // outcome vector
}
parameters {
  real alpha;           // intercept
  vector[K] beta;       // coefficients for predictors
  real<lower=0> sigma;  // error scale
}
model {
  // priors
  alpha ~ normal(0,1);
  beta ~ normal(0,1);
  sigma ~ normal(0,1);
  y ~ normal(x * beta + alpha, sigma);  // likelihood
}
generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(y[n] | x[n,] * beta + alpha, sigma);
  }
}
