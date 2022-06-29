#! /usr/bin/Rscript --vanilla

# load packages 
if(!requireNamespace("pacman"))install.packages("pacman")
pacman::p_load(here, tictoc, purrr, parallel, brms, Matrix, tidyverse, tidybayes, transport, bayesplot, cowplot, loo, multiverse, priorsense)

# set seed
set.seed(42424242)

# set # of cores 
nc <- detectCores() - 1

# data from brms:
# https://cran.r-project.org/web/packages/bayesian/vignettes/GetStarted.html
# https://paul-buerkner.github.io/brms/reference/epilepsy.html
dat <- brms::epilepsy

# initialize multiverse 
M_epi = multiverse()

tic()
inside(M_epi, {

  # priors via brms
  #prior_epi_brms <- c(set_prior("normal(0,5)", class = "b"),
  #                    set_prior("normal(0,10)", class = "Intercept"),
  #                    set_prior("cauchy(0,5)", class = "sd"))
  
  # preprocessing: normalizing Age & Base
  dat <- dat %>% 
    mutate(Age_n =(dat$Age - mean(dat$Age)) / sd(dat$Age), 
           Base_n = (dat$Base - mean(dat$Base)) / sd(dat$Base))
  
  # log_Age_c + log_Base4_c * Trt_c + (1 | patient) + (1 | visit) + (1 | obs)
  
  # fit model with default settings
  mod_epi <- brm(count ~ 
                   branch(formula,
                          "eq1" ~ Trt + Base_n + Age_n + (1 | patient) + (1 | visit) + (1 | obs),
                          "eq2" ~ Trt + Base_n + Age_n + (1 | patient) + (1 | visit), 
                          "eq3" ~ Trt + Base_n + Age_n + (1 | patient), 
                          "eq4" ~ Trt + Base_n + Age_n,
                          "eq5" ~ Trt + Base_n,
                          "eq6" ~ Trt),
                 family = 
                   branch(family, 
                          "poisson" ~ poisson(),
                          "negbinom" ~ negbinomial()
                          ),
                 data = dat, 
                 cores = nc,
                 save_pars = save_pars(all = TRUE))
})

# run multiverse 
M_epi %>% execute_multiverse()
toc()

# access results
# M %>% multiverse::expand() %>% multiverse::extract_variables(mod_epi)
# Error in `list_sizes()`:
# ! `x$mod_epi` must be a vector, not a <brmsfit> object.
# fixed when %>% unnest_wider("extracted") is removed

source("helper_functions.R")

multi_epi <- M_epi %>% 
  multiverse::expand() %>% 
  extract_vars_df(mod_epi)

################################################################################
# add metrics 
tic()
multi_dict_epi <- evaluate_multiverse(multi_epi, mod_epi, outcome = dat$count)
toc()

write_rds(multi_dict_epi, here::here("results", "multi_dict_epi.rds"))
################################################################################
# (not yet in function): tail ESS
tailess <- purrr::map_dbl(multi$mod_epi, posterior::ess_tail)

# Why is tail ESS = NA? 
# from help: "If any of the draws is non-finite, that is, NA, NaN, Inf, 
# or -Inf, the returned output will be (numeric) NA. Also, if all draws within 
# any of the chains of a variable are the same (constant), the returned 
# output will be (numeric) NA as well."

################################################################################
# prior sensitivity 

# get prior sensitivity of model parameters for each model
priorsense_params <- purrr::map(multi$mod_epi, priorsense::powerscale_sensitivity)

# unpack priorsense object
ps_params <- purrr::map(purrr::map(multi$mod_epi, priorsense::powerscale_sensitivity), "sensitivity")

# all vars (and info) for which diagnosis != "-"
pdc_params <- purrr::map(ps_params, ~filter(.x, diagnosis != "-"))

# count vars with diagnosis == "prior-data conflict"
count_pdc <- purrr::map(purrr::map(ps_params, ~filter(.x, diagnosis == "prior-data conflict")), count) %>% unlist()

# count vars with diagnosis != "-" 
count_ps <- purrr::map(purrr::map(ps_params, ~filter(.x, diagnosis != "-")), count) %>% unlist()

# What is class 'powerscaling_data' class? -> prediction = ...
################################################################################
# y_rep, mean_yrep 
# add posterior draws to data
# dat_yrep <- dat %>%
# add_predicted_draws(mod_epi)
################################################################################
# quantitative ppc - divergences/distances between data distribution and model 

# only first row 
wasserstein1d(dat$count, multi$y_rep[[1]][1])

# get wasserstein distances between data sample and average posterior sample 
# needs to vectors of same dimension -> use average posterior cdf 
dist_ws <- purrr::map(mean_yrep, wasserstein1d, a = dat$count) %>% unlist()

# What is "high" distance i.e. model needs to be investigated? Wasserstein dist alone is not helpful! 

# "cjs_dist" Cumulative Jensen-Shannon distance
# "hellinger_dist": Hellinger distance.
# "kl_div": KL divergence

################################################################################
loo_results <- purrr::map(multi$mod_epi, loo)

# loo: always use "moment_match = TRUE"? 
# This takes ages!
tic()
loo_results_mm <- purrr::map(multi$mod_epi, loo, moment_match = TRUE)
toc()

elpd_loo <- purrr::map(purrr::map(loo_results, "estimates"), 1) # extract 1st element - this is "Estimate" for elpd_loo
se_elpd_loo <- purrr::map(purrr::map(loo_results, "estimates"), 4) # extract 4th element - this is "SE" for elpd_loo

# pareto k should be < 0.7 - "good", "ok", "bad", "very bad"
# count "bad"+"very bad" 
pareto_ks <- purrr::map(purrr::map(loo_results, "diagnostics"), "pareto_k")
flag_pareto_ks <- purrr::map_dbl(purrr::map(purrr::map(loo_results, "diagnostics"), "pareto_k"), ~sum(.x > 0.7))

################################################################################
# https://stackoverflow.com/questions/61297357/how-can-i-use-purrr-to-extract-an-attribute-in-a-deeply-nested-list-column-in-r
################################################################################

# Teemu's paper
# posterior distribution - data distribution 
# uniformity check

# if ... > ... check ppc plots 
bayesplot::pp_check(multi$mod_epi[[1]])
bayesplot::pp_check(multi$mod_epi[[2]])

fit <- multi$mod_epi[[1]]
pp_check(fit)  # shows dens_overlay plot by default
# pp_check(fit, type = "error_hist", ndraws = 11)
pp_check(fit, type = "scatter_avg", ndraws = 100)
pp_check(fit, type = "stat_2d")
pp_check(fit, type = "rootogram")
pp_check(fit, type = "loo_pit_qq")

# with y and y_rep 
# -> bayesplot tools 
bayesplot::ppc_ecdf_overlay(dat$count, multi$y_rep[[1]])
bayesplot::ppc_ecdf_overlay(dat$count, multi$y_rep[[2]])

y_dats <- multi$dat_yrep[[1]]$count
yreps <- multi$y_rep[[1]]

# brms
#prior_epi_brms <- c(set_prior("normal(0,5)", class = "b"),
#                    set_prior("normal(0,10)", class = "Intercept"),
#                    set_prior("cauchy(0,5)", class = "sd"))