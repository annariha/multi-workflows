# "noise-mining" - all covariates are irrelevant 
# How big is the influence of noise due to number of models? 
# several models with 1 covariate each 

library(tidyverse)
library(patchwork)
# generate data 
set.seed(42424242)

n_obs = 1000
n_covars = 100

# 1. mean of each covariate 
mu = rnorm(n = n_covars, mean = 10, sd = 100)
# 2. sd of each covariate 
sigma = 10
# 3. Covariance matrix for all covariates 
Sigma = diag(x = sigma, nrow = n_covars, ncol = n_covars)
# in case we want to add correlation between covariates 
rho = 0 
Sigma[Sigma == 0] <- rho
# 4. generate a matrix of covariate values by sampling N observations from a multivariate Normal distribution 
covars_matrix = MASS::mvrnorm(n = n_obs, mu = mu, Sigma = Sigma)

# 5. set true model parameters 
alpha = rep(rnorm(n = 1, mean = 0, sd = 1), times = n_obs)
betas = rnorm(n = n_covars, mean = 0, sd = 1)
error_var = 1 
epsilon = rnorm(n = n_obs, mean = 0, sd = error_var)

# 6. generate true outcome using all covariates
y_true = alpha + covars_matrix %*% betas + epsilon 

################################################################################
y = x + epsilon
# normally distributed error
# true x = sum of all delta x_i's
# x_0 = 1
# x_i = sum of delta x_i's up until i 
x = c()
x = sum()
x[0:10]
x[]
################################################################################

get_data_list <- function(values_covar, y_true){
  data_list <- list(N = NROW(values_covar), 
                    K = NCOL(values_covar), 
                    y = as.vector(y_true),
                    x = values_covar)
  return(data_list)
}
################################################################################

# compile the model 
model = cmdstanr::cmdstan_model(here::here("sim-studies", "irrelevant-covars", "stan", "test_model.stan"))

n_obs<-100
set.seed(7268534)
y <- rnorm(n_obs)
loos<-list()

for (i in 1:100) {
  print(i)
  set.seed(6782345+i)
  data_list <- list(N = n_obs, 
                    K = 1, 
                    y = y,
                    x = as.matrix(rnorm(n_obs),nrow=n_obs,ncol=1))
  fit <- model$sample(data = data_list,
                       seed = i,
                       chains = 1,
                       parallel_chains = 1,
                       refresh = 0, show_messages=FALSE, show_exceptions=FALSE)
  loos[[i]]<-fit$loo()
}


# Idea: every model "sees" only part of the data
pred1 <- matrix(covars_matrix[,1])
pred2 <- matrix(covars_matrix[,2])
pred3 <- matrix(covars_matrix[,3])
pred4 <- matrix(covars_matrix[,4])
pred5 <- matrix(covars_matrix[,5])

# data for Stan
data1 <- get_data_list(pred1, y_true)
data2 <- get_data_list(pred2, y_true)
data3 <- get_data_list(pred3, y_true)
data4 <- get_data_list(pred4, y_true)
data5 <- get_data_list(pred5, y_true)

# data for Stan 
data_list <- list(N = NROW(pred), 
                  K = NCOL(pred), 
                  y = as.vector(y_true),
                  x = pred)

# compile the model 
model = cmdstanr::cmdstan_model(here::here("sim-studies", "irrelevant-covars", "stan", "test_model.stan"))

#model$print()

# fit the model 
fit_1 = model$sample(data = data1,
                     seed = 42424242,
                     chains = 4,
                     parallel_chains = 4,
                     refresh = 500)

fit_2 = model$sample(data = data2,
                     seed = 42424242,
                     chains = 4,
                     parallel_chains = 4,
                     refresh = 500)

fit_3 = model$sample(data = data3,
                     seed = 42424242,
                     chains = 4,
                     parallel_chains = 4,
                     refresh = 500)

fit_4 = model$sample(data = data4,
                     seed = 42424242,
                     chains = 4,
                     parallel_chains = 4,
                     refresh = 500)

fit_5 = model$sample(data = data5,
                     seed = 42424242,
                     chains = 4,
                     parallel_chains = 4,
                     refresh = 500)

# .numargs = digits(n=3) 
fit_1$summary()

# get elpd

get_loo_object <- function(fit){
  loo = loo::loo(fit$draws("log_lik"), r_eff = loo::relative_eff(exp(fit$draws("log_lik"))))
  return(loo)
}

loo_1 <- loo::loo(fit_1$draws("log_lik"), r_eff = loo::relative_eff(exp(fit_1$draws("log_lik"))))
loo_2 <- loo::loo(fit_2$draws("log_lik"), r_eff = loo::relative_eff(exp(fit_2$draws("log_lik"))))
loo_3 <- loo::loo(fit_3$draws("log_lik"), r_eff = loo::relative_eff(exp(fit_3$draws("log_lik"))))
loo_4 <- loo::loo(fit_4$draws("log_lik"), r_eff = loo::relative_eff(exp(fit_4$draws("log_lik"))))
loo_5 <- get_loo_object(fit_5)


get_comparisons <- function(loo_object, ...){
  if (is_list(loo_object)){
    loo_objects_list = loo_object
  } else {
    loo_objects_list = list(loo_object, ...)
  }
  
  loo_bb_weights <- loo::loo_model_weights(loo_objects_list, method="pseudobma")
  pseudobma_weights = loo::loo_model_weights(loo_objects_list, method="pseudobma", BB = FALSE)
  
  # get loo comparison df with number of Pareto khats > 0.7, LOO-BB weights, pseudo-BMA weights
  comparison_df <- loo::loo_compare(loo_objects_list) |>
    data.frame() |>
    tibble::rownames_to_column() |>
    left_join(data.frame(loo_bb_weights, rowname = names(loo_bb_weights)), by=join_by(rowname)) |>
    left_join(data.frame(pseudobma_weights, rowname = names(pseudobma_weights)), by=join_by(rowname)) |>
    mutate(n_high_pareto_ks = purrr::map_dbl(purrr::map(loo_objects_list, ~.x$diagnostics$pareto_k), ~sum(.x>0.7)))
  
  return(comparison_df)
}

get_comparisons(loo_1, loo_2)

loo_objects <- list(loo_1, loo_2, loo_3, loo_4, loo_5)
loo_bb_weights <- loo::loo_model_weights(loo_objects, method="pseudobma")
pseudobma_weights = loo::loo_model_weights(loo_objects, method="pseudobma", BB = FALSE)
#n_high_pareto_ks = purrr::map_dbl(purrr::map(loo_objects, ~.x$diagnostics$pareto_k), ~sum(.x>0.7)) 
#names(n_high_pareto_ks) <- names(loo_objects)

# get loo comparison df with number of Pareto khats > 0.7, LOO-BB weights, pseudo-BMA weights
comparison_df <- loo::loo_compare(loo_objects) |>
  data.frame() |>
  tibble::rownames_to_column() |>
  left_join(data.frame(loo_bb_weights, rowname = names(loo_bb_weights)), by=join_by(rowname)) |>
  left_join(data.frame(pseudobma_weights, rowname = names(pseudobma_weights)), by=join_by(rowname)) |>
  mutate(n_high_pareto_ks = purrr::map_dbl(purrr::map(loo_objects, ~.x$diagnostics$pareto_k), ~sum(.x>0.7)))

# get plot 
source(here::here("sim-studies", "irrelevant-covars", "scripts", "plot_elpddiffs_loobb.R"))
get_plot_elpddiffs_loobb(comparison_df)

# do this for all models 
test_data <- as_tibble(covars_matrix) |>
  pivot_longer(cols = everything(), 
               names_to = "selected_covars",
               values_to = "value") |>
  group_by(selected_covars) |>
  nest() |>
  rename(values_covar = data) |>
  mutate(y_true = list(y_true)) |>
  mutate(data_list = purrr::map2(values_covar, y_true, get_data_list))

# to get a named list for Stan
test_data[1,]$data_list[[1]]
# same as 
purrr::pluck(test_data[1,]$data_list)

test_data <- data.frame(model_id = 1:NROW(covars_matrix), selected_covars = c())

# final output: model id, elpd, se

ggplot()
# inspect posterior draws 
bayesplot::mcmc_hist(fit_1$draws("alpha")) 
#+ vline_at(alpha, size = 1.5)
bayesplot::mcmc_hist(fit_1$draws("beta")) 
#+ vline_at(beta, size = 1.5)
bayesplot::mcmc_hist(fit_1$draws("sigma")) 
#+ vline_at(sigma, size = 1.5)

# number of models = number of covariates 
k = n_covars

# generate "sub-datasets" as input for each stan file 
dataset <- list(N = n_obs, y = )
# compile 
model <- cmdstanr::cmdstan_model(stan_file = , exe_file = )

# fit 


# one could extend this to sth more "multiverse-like" by having more structure/connections between models 
# e.g., generating data with Student t, fitting some models with Normal and some with Student t

# if some covariates are relevant, we need to set a signal-to-noise ratio and decide how covariates are related 
