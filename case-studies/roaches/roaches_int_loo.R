# setup ####
library(rstanarm)
library(brms)
library(cmdstanr)
options(mc.cores = parallel::detectCores())
library(loo)
library(ggplot2)
library(bayesplot)
theme_set(bayesplot::theme_default(base_family = "sans"))

# load data
data(roaches)
# Roach1 is very skewed and we take a square root
roaches$sqrt_roach1 <- sqrt(roaches$roach1)

# stan file
poisson_re_int <- here::here("case-studies", "roaches", "poisson_re_integrate.stan")

# compile model, prepare data, sample ####
#modpri <- cmdstanr::cmdstan_model(stan_file = poisson_re_int, compile_standalone = TRUE)
modpri <- cmdstanr::cmdstan_model(stan_file = poisson_re_int)

modpri$expose_functions(global = TRUE)
modpri$functions

datap <- list(N=dim(roaches)[1], 
              P=3,
              offsett=log(roaches$exposure2),
              X=roaches[,c('sqrt_roach1','treatment','senior')],
              y=roaches$y)

fitpri <- modpri$sample(data = datap, refresh=0, chains=8, parallel_chains=4)
# throws exceptions: 
# Exception: integrate: error estimate of integral 3.97631e-13 exceeds the given relative tolerance times norm of integral

loocmd <- function(fit) {
  loo(fit$draws("log_lik"), r_eff=relative_eff(fit$draws("log_lik")))
}

(loopri <- loocmd(fitpri))
# Error in n_eff_vec/S : non-numeric argument to binary operator

log_lik_int <- fitpri$draws("log_lik")

# extract results ####

# get draws
draws_df <- fitpri$draws(format = "data.frame") |>
  as_tibble() 

# reformat draws to combine z's (and beta's) as vectors for each iteration in each chain
draws_df <- draws_df |> 
  select(-matches("log_lik")) |>
  tidyr::nest(zs = starts_with("z"),
              sigmaz = matches("sigmaz"),
              alpha = matches("alpha"),
              beta = matches("beta")) |>
  mutate(zs = map(zs, unlist),
         sigmaz = map(sigmaz, ~matrix(unlist(.x), ncol = 1)),
         alpha = map(alpha, ~matrix(unlist(.x), ncol = 1)),
         beta = map(beta, ~matrix(unlist(.x), ncol = 3)))

# summarise draws
draws_summary <- posterior::summarise_draws(fitpri) |> as_tibble()

# zs - zs_vec
# manual part: what parameters are needed 
# replace random effect 
# test
zs_vec <- 
muz_j <- 0 
sigmaz_j <- 
yj <- 
lin_pred_ <- (offsettj + alpha) + Xj * beta

# define integrand as function  ####
# in Stan code: normal_lpdf(z|0,sigmaz)+poisson_log_glm_lpmf(yi | to_row_vector(Xi), z+offsetti_plus_alpha, beta)
integrand <- function(zs_vec, 
                      muz_j, 
                      sigmaz_j,
                      yj, 
                      Xj,
                      offsettj_plus_alpha,
                      beta){
  
  # function defines integrand for integrate()
  # combines prior on obs-level random effect z and likelihood
  
  # in Stan code: normal_lpdf(z|0,sigmaz)
  z_term <- dnorm(zs_vec,
                  mean = muz_j, 
                  sd = sigmaz_j,
                  log = TRUE)
  
  # in Stan code: poisson_log_glm_lpmf(yi | to_row_vector(Xi), z+offsetti_plus_alpha, beta)
  fit_term <- dpois(x = yj, 
                    lambda = exp(zs_vec + offsettj_plus_alpha + Xj * beta),
                    log = TRUE)
  
  #dlik(yj, link(zs_vec + linpred_minus_re))
  
  result = exp(z_term + fit_term)
  return(result)
}

integrate(integrand, 
          lower = -(9 * sigmaz_j), 
          upper = 9 * sigmaz_j,
          muz_j = 0, # random intercept mean (here: 0)
          sigmaz_j = sigmaz_j, # scale for z  
          yj = yj, # datap$y[j], # target
          Xj = Xj,
          offsettj_plus_alpha = offsettj_plus_alpha,
          beta = beta,
          #dlik=function(y, linpred){dpois(x=y, lambda=linpred, log=TRUE)},
          #dlik=function(y, linpred){dnegb(x=y, lambda=linpred, shape=shape, log=TRUE)}
          )

# test: the result of integrate() should roughly be the same as mean(dpois()) with zs_vec = rnorm(10000)
zs_vec <- rnorm(10000)
mean(dpois(x = yj, 
           lambda = exp(zs_vec + offsettj_plus_alpha + Xj * beta),
           log = FALSE))
# repeat with one posterior draw
# compare with log_lik value 

# get log-likelihood
posterior::mutate_variables(x, log_lik = )