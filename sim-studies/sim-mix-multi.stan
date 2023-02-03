// simulate data from two Gaussian mixture
// data
data {
 int<lower = 0> N; // number of obs
 //vector[N] y; // outcome 
 //int<lower = 0, upper = 1> run_estimation; // a switch to evaluate the likelihood
}

// parameters 
parameters {
  vector[2] mu;
  real<lower=0> sigma[2];
  real<lower=0, upper=1> theta;
}

model {
 // prior
 sigma ~ lognormal(0, 2);
 mu ~ normal(0, 2);
 // theta ~ beta(5, 5); // use this instead of log(lambda) or log_mix()?
 // likelihood 
  //if(run_estimation==1){
  //for (n in 1:N) {
    //target += log_sum_exp(log(0.3) + normal_lpdf(y[n] | mu[1], sigma[1]),
                          //log(0.7) + normal_lpdf(y[n] | mu[2], sigma[2]));
    //}
  //}
}

// generated quantities
generated quantities {
  vector[N] y_sim;
    for (n in 1:N) {
      y_sim[n] = log_sum_exp(log(0.3) + normal_rng(mu[1], sigma[1]), 
                              log(0.7) + normal_rng(mu[2], sigma[2]));
  }
}

