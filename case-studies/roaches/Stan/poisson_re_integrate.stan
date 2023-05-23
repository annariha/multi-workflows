// Aki Vehtari - roaches example 
// Poisson regression with hierarchical intercept ("random effect")
functions {
  real integrand(
    real z, 
    real notused, 
    array[] real theta, 
    array[] real also_notused,
    array[] int yi) {
      real sigmaz = theta[1];
      real linpred_minus_re = theta[2]; 
      return exp(normal_lpdf(z|0,sigmaz)+poisson_log_lpmf(yi[1] | linpred_minus_re+z));
  }
}
data {
  int<lower=0> N;           // number of data points
  int<lower=0> P;           // number of covariates
  matrix[N,P] X;            // covariates
  array[N] int<lower=0> y;  // target
  vector[N] offsett;        // offset (offset variable name is reserved)
}
parameters {
  real alpha;               // intercept
  vector[P] beta;           // slope
  vector[N] z;              // individual intercept ("random effect")
  real<lower=0> sigmaz;     // prior scale for z
}
model {
  // priors
  alpha ~ normal(0, 3);  
  beta ~ normal(0, 3);    
  z ~ normal(0, sigmaz);  
  sigmaz ~ normal(0, 1);
  // observation model
  y ~ poisson_log_glm(X, z+offsett+alpha, beta); 
}
generated quantities {
  // log_lik for PSIS-LOO
  vector[N] log_lik;
  for (i in 1:N) {
    // for debugging
    //print("loop iteration: ", i);
    //print("sigmaz: ", sigmaz, " offset: ", offsett[i], " alpha: ", alpha, " beta: ", beta, " Xi: ", X[i,],  " yi: ", y[i]);
    // z as posterior draws, this would be challenging for PSIS-LOO (and WAIC)
    // log_lik[i] = poisson_log_glm_lpmf({y[i]} | X[i,], z[i]+offsett[i]+alpha, beta);
    // we can integrate each z[i] out with 1D adaptive quadrature 
    log_lik[i] = log(integrate_1d(integrand,
                              negative_infinity(),
                              positive_infinity(),
                              {sigmaz, alpha + offsett[i] + X[i,] * beta},
                              {0.},
                              {y[i]},
                  sqrt(sqrt(machine_precision())) // increase relative tolerance, default=sqrt(machine_precision())
                  )
                  );
     // for debugging
     //print("loglik_", i, ":", log_lik[i]);
  }
}
