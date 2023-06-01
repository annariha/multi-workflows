# setup ####

# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(here, tictoc, future, furrr, purrr, parallel, 
               brms, Matrix, tidyverse, cmdstanr, spatstat)

# set seed
set.seed(42424242)

# set # of cores 
nc <- detectCores() - 2
options(mc.cores = nc) 

# load functions
source(here::here("case-studies", "epilepsy", "R", "build_name.R"))
source(here::here("case-studies", "epilepsy", "R", "build_fit.R"))

# load combinations df ####
combinations_df <- read_rds(here::here("case-studies", "epilepsy", "data", "prelim", "combinations_df.rds"))
# models with obs-level random intercept
models_with_obs_randint <- combinations_df |> filter(obs != "") 

# test: get one brms model 
model_5 <- build_fit(models_with_obs_randint[5,], dataset = brms::epilepsy)

# get stan code 
brms::stancode(model_5)

# get posterior draws
draws_df_5 <- posterior::as_draws_df(model_5)

# reformat draws to combine z's (and beta's) as vectors for each iteration in each chain
input_df_5 <- draws_df_5 |> 
  tidyr::nest(rs = starts_with("r_obs"),
              sd = matches("sd_obs__Intercept"),
              intercept = matches("b_Intercept"),
              beta_trt = matches("b_Trt1")) |>
  mutate(rs = map(rs, unlist),
         sd = map_dbl(sd, ~matrix(unlist(.x), ncol = 1)),
         intercept = map_dbl(intercept, ~matrix(unlist(.x), ncol = 1)),
         beta_trt = map_dbl(beta_trt, ~matrix(unlist(.x), ncol = 1))) |>
  rowwise() |>
  mutate(zs = list(unlist(rs) / sd))

head(input_df_5)

# extract linpred 
# from brms docs: "[posterior] draws before applying any link functions or other transformations"
lin_pred <- brms::posterior_linpred(model_5)

# Now: without obs-level random intercept
# standardized group-level effects
zs_df <- data.frame(matrix(unlist(input_df_5$zs), ncol=236, byrow=T))
lin_pred_witho <- lin_pred - zs_df # different values across iterations, and observations

# compute integrated loo

# define integrand as function  ####
# in Stan code: exp(std_normal_lpdf(z_1) + poisson_log_lpmf(Yi[1] | r_1_1 + linpred_minus_re))

# evaluate integral with integrate()
# from docs: "globally adaptive interval subdivision is used in connection with extrapolation by Wynn's Epsilon algorithm, with the basic step being Gauss-Kronrod quadrature"
integrand <- function(zs, 
                      y, 
                      linpreds_minus_re){
  
  # function defines integrand for integrate()
  # in Stan code: std_normal_lpdf(z_1)
  z_term <- dnorm(zs,
                  mean = 0, 
                  sd = 1,
                  log = TRUE)
  
  # in Stan code: poisson_log_lpmf(Yi[1] | r_1_1 + linpred_minus_re)
  fit_term <- dpois(x = y, 
                    lambda = exp(zs + linpreds_minus_re),
                    log = TRUE)
  
  result = exp(z_term + fit_term)
  return(result)
}
# evaluate with integrate()
test <- integrate(integrand, 
          lower = -Inf, 
          upper = Inf,
          y = as.numeric(brms::epilepsy$count[1]), 
          linpreds_minus_re = lin_pred_witho[1,1])
str(test)
# compare to Gauss-Hermite quadrature
# in Stan code: exp(std_normal_lpdf(z_1) + poisson_log_lpmf(Yi[1] | r_1_1 + linpred_minus_re))
integrand_ghq <- function(zs, 
                          y, 
                          linpreds_minus_re){
  
  # function defines integrand for gauss.hermite()
  # in Stan code: poisson_log_lpmf(Yi[1] | r_1_1 + linpred_minus_re)
  fit_term <- dpois(x = y, 
                    lambda = exp(zs + linpreds_minus_re))
  
  result = fit_term
  return(result)
}
# evaluate integral with Gauss-Hermite Quadrature approximation
# from docs: "approximation to the expected value of any function of a normally-distributed random variable, using Gauss-Hermite quadrature."
gauss.hermite(integrand_ghq, 
              mu = 0, 
              sd = 1, 
              y = as.numeric(brms::epilepsy$count[1]), 
              linpreds_minus_re = lin_pred_witho[1,1], 
              order = 20)

# compare the above results to rnorm()
z_norm <- rnorm(100000)
y = as.numeric(brms::epilepsy$count[1])
linpreds_minus_re <- lin_pred_witho[1,1]

mean(dpois(x = y, 
           lambda = exp(z_norm + linpreds_minus_re)))
