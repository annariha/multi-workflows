data {
  int<lower=0> n_obs;
  vector[n_obs] obs;
  vector[n_obs] covars;
}

parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}

model {
  // priors
  alpha ~ normal(0,1);
  beta ~ normal(0,1);
  sigma ~ normal(0,1);
  // 
  obs ~ normal(alpha + beta * covars, sigma);
}

generated quantities {
  vector[n_obs] log_lik;
  for (n in 1:n_obs){
    log_lik[n] = normal_lpdf(obs[n] | alpha + beta * covars[n], sigma);
  }
}
