#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman"))install.packages("pacman")
pacman::p_load(here, tictoc, purrr, parallel, brms, Matrix, tidyverse, 
               tidybayes, transport, loo, multiverse, priorsense, cmdstanr)

# run once
# cmdstanr::install_cmdstan()

# set seed
set.seed(42424242)

# set # of cores 
nc <- detectCores() - 1

# data from brms: https://paul-buerkner.github.io/brms/reference/epilepsy.html
dat <- brms::epilepsy

# initialise multiverse ####
M_epi = multiverse()

tic()
inside(M_epi, {
  
  # same data for all universes
  dat <- dat 
  
  # compile models with different modelling choices 
  fit_empt <- brm(count ~ branch(formula,
                                  "eq1" ~ Trt,  
                                  "eq2" ~ Trt + zBase, 
                                  "eq3" ~ Trt + zAge,
                                  "eq4" ~ Trt + zBase + zAge,
                                  "eq5" ~ zAge + zBase * Trt,
                                  "eq6" ~ Trt + zBase + (1 | patient), 
                                  "eq7" ~ zBase * Trt + (1 | patient),
                                  "eq8" ~ Trt + zAge + (1 | patient),
                                  "eq9" ~ Trt + zBase + zAge + (1 | patient),
                                  "eq10" ~ zAge + zBase * Trt + (1 | patient),
                                  "eq11" ~ Trt + zBase + zAge + (1 | patient) + (1 | visit),
                                  "eq12" ~ zAge + zBase * Trt + (1 | patient) + (1 | visit),
                                  "eq13" ~ Trt + zBase + zAge + (1 | patient) + (1 | visit) + (1 | obs),
                                  "eq14" ~ zAge + zBase * Trt + (1 | patient) + (1 | visit) + (1 | obs)),
                 data = dat, 
                 family = branch(family, 
                                 "poisson" ~ poisson(),
                                 "negbinom" ~ negbinomial()),
                 chains = 0,
                 backend = "cmdstanr")
  
  # to avoid recompiling store compiled model and read back in for each universe 
  write_rds(fit_empt, here::here("fit_empty.rds"))
  fit_empty <- read_rds(here::here("fit_empty.rds"))
  
  # use update to sample without recompiling and cmdstanr as backend
  mod_epi <- update(fit_empty, 
                    recompile = FALSE,
                    chains = 4, 
                    cores = nc,
                    save_pars = save_pars(all = TRUE),
                    seed = 12345678, # every universe uses same seed for MCMC
                    backend = "cmdstanr")
})
toc() 

# run multiverse analysis ####
tic()
M_epi %>% execute_multiverse()
toc()

# priors <- c(set_prior("normal(0,5)", class = "b"),
#   set_prior("normal(0,10)", class = "Intercept"),
#   set_prior("cauchy(0,5)", class = "sd"))

#prior = branch(prior, 
#"default" ~ NULL %when% formula, 
#"r2d2"~ set_prior(R2D2(mean_R2 = 0.8, prec_R2 = 10))  %when%, 
#"r2d2m2" ~ %when% ),

# M %>% multiverse::expand() %>% multiverse::extract_variables(mod_epi)
# Error in `list_sizes()`:
# ! `x$mod_epi` must be a vector, not a <brmsfit> object.
# fixed when %>% unnest_wider("extracted") is removed

source(here::here("helper-functions.R"))
source(here::here("case-studies", "epilepsy", "R", "evaluate_universe.R"))

# extract variables of interest from multiverse object ####
multi_epi <- M_epi %>% 
  multiverse::expand() %>% 
  extract_vars_df(mod_epi)

# store results 
write_rds(multi_epi, here::here("case-studies", "epilepsy", "results", "multiverse_epi.rds"))

# just for testing
multi_epi <- read_rds(here::here("case-studies", "epilepsy", "results", "multiverse_epi.rds"))

# evaluate models #### 

# this takes long, since recompiling with rstan atm 
tic()
multi_dict_epi <- purrr::map_dfr(multi_epi$mod_epi, evaluate_universe)
toc()

# add additional info for dictionary
multi_dict_epi <- multi_dict_epi %>% 
  mutate(model_id = row_number(),
         family = multi_epi$family,
         formula = multi_epi$formula) %>% 
  select(model_id, family, formula, everything())

# store results 
write_rds(multi_dict_epi, here::here("case-studies", "epilepsy", "results", "multi_dict_epi.rds"))

# old 
# multi_dict_epi <- evaluate_multiverse(multi = multi_epi, mod = mod_epi, outcome = dat$count)