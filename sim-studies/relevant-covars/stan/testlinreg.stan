data {
  int<lower=0> n_train;
  int<lower=0> n_covars;
  int<lower=0> n_test;
  vector[n_train] obs_train;
  vector[n_test] obs_test;
  matrix[n_train, n_covars] covars_train;
  matrix[n_test, n_covars] covars_test;
}

parameters {
  real alpha;
  vector[n_covars] beta;
  real<lower=0> sigma;
}

model {
  // priors
  alpha ~ normal(0,1);
  beta ~ normal(0,1);
  sigma ~ normal(0,1);
  // model
  obs_train ~ normal(alpha + covars_train * beta, sigma);
}

generated quantities {
  vector[n_train] log_lik;
  for (n in 1:n_train){
    log_lik[n] = normal_lpdf(obs_train[n] | alpha + covars_train[n,] * beta, sigma);
  }
  vector[n_test] log_lik_test;
  for (n in 1:n_test){
    log_lik_test[n] = normal_lpdf(obs_test[n] | alpha + covars_test[n,] * beta, sigma);
  }
}
