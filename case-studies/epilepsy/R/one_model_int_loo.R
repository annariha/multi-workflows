# setup ####

# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")

pacman::p_load(here, tictoc, future, furrr, purrr, parallel, brms, Matrix, tidyverse, 
               tidybayes, transport, loo, multiverse, priorsense, cmdstanr)

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
# "[posterior] draws before applying any link functions or other transformations"
lin_pred <- brms::posterior_linpred(model_5)

# 4000 rows 
NROW(lin_pred)
# 236 columns 
NCOL(lin_pred)

# test 
# helper function for factor->numeric conversion, see https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-integer-numeric-without-loss-of-information
# as.double.factor <- function(x) {as.numeric(levels(x))[x]}
# unlist(input_df_5[1,]$rs)[2] + input_df_5[1,]$intercept + as.double.factor(brms::epilepsy$Trt[2]) * input_df_5[1,]$beta_trt
# -> gives the same results

# without obs-level random intercept
rs_df <- data.frame(matrix(unlist(input_df_5$rs), ncol=236, byrow=T))
zs_df <- data.frame(matrix(unlist(input_df_5$zs), ncol=236, byrow=T))
lin_pred_without <- lin_pred - rs_df # same value in each iteration 
lin_pred_test <- lin_pred - (zs_df * input_df_5$sd) # this is the same as substracting rs_df
lin_pred_witho <- lin_pred - zs_df # different values across iterations, and observations

# compute integrated loo

# define integrand as function  ####
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

library(spatstat)

gauss.hermite(integrand_ghq, 
              mu = 0, 
              sd = 1, 
              y = as.numeric(brms::epilepsy$count[1]), 
              linpreds_minus_re = lin_pred_witho[1,1], 
              order = 10)

