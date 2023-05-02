// generated with brms 2.19.0
// adjusted for integrated loo following https://avehtari.github.io/modelselection/roaches.html#5_Poisson_model_with_%E2%80%9Crandom_effects%E2%80%9D_and_integrated_LOO

functions {
  // function should return the value of the integrand evaluated at point of individual intercept ("random effect")
  real integrand(
    real r_3_1,
    real notused,
    array[] real theta,
    array[] real Xci,
    array[] int Yi){
      real sd_3 = theta[1];
      real mu = theta[2];
      vector[ntheta-2] b = to_vector(theta[3:ntheta]);
      return exp(
        student_t_lpdf(sd_3 | 3, 0, 2.5) - 1 * student_t_lccdf(0 | 3, 0, 2.5) + std_normal_lpdf(z_3[1]) + poisson_log_glm_lpmf(Yi | to_row_vector(Xci), mu, b));
  }
}
data {
  int<lower=1> N;  // total number of observations
  int Y[N];  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  int<lower=1> J_2[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_1;
  // data for group-level effects of ID 3
  int<lower=1> N_3;  // number of grouping levels
  int<lower=1> M_3;  // number of coefficients per level
  int<lower=1> J_3[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_3_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  int Kc = K - 1;
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // population-level effects
  real Intercept;  // temporary intercept for centered predictors
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // standardized group-level effects
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  vector[N_2] z_2[M_2];  // standardized group-level effects
  vector<lower=0>[M_3] sd_3;  // group-level standard deviations
  vector[N_3] z_3[M_3];  // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_1;  // actual group-level effects
  vector[N_2] r_2_1;  // actual group-level effects
  vector[N_3] r_3_1;  // actual group-level effects
  real lprior = 0;  // prior contributions to the log posterior
  r_1_1 = (sd_1[1] * (z_1[1]));
  r_2_1 = (sd_2[1] * (z_2[1]));
  r_3_1 = (sd_3[1] * (z_3[1]));
  lprior += student_t_lpdf(Intercept | 3, 1.4, 2.5);
  lprior += student_t_lpdf(sd_1 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_2 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_3 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept;
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_2_1[J_2[n]] * Z_2_1[n] + r_3_1[J_3[n]] * Z_3_1[n];
    }
    target += poisson_log_glm_lpmf(Y | Xc, mu, b);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(z_1[1]);
  target += std_normal_lpdf(z_2[1]);
  target += std_normal_lpdf(z_3[1]);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  // log_lik for PSIS-LOO
  vector[N] log_lik;
  for (i in 1:N) {
    // z as posterior draws, this would be challenging for PSIS-LOO (and WAIC)
    // log_lik[i] = poisson_log_glm_lpmf({Y[i]} | Xc[i,], mu, b);
    // we can integrate each z[i] out with 1D adaptive quadrature 
    log_lik[i] = log(integrate_1d(
      integrand,
      negative_infinity(),
      positive_infinity(),
      append_array({sd_3}, append_array({mu}, to_array_1d(b))),
      to_array_1d(Xc[i,]),
      {Y[i]}
      )
    );
  }
}
