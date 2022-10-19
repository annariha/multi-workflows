#! /usr/bin/Rscript --vanilla

# load packages 
if(!requireNamespace("pacman"))install.packages("pacman")

pacman::p_load(here, tictoc, purrr, parallel, brms, Matrix, tidyverse, 
               tidybayes, transport, loo, multiverse, priorsense)

# set seed
set.seed(42424242)

# set # of cores 
nc <- detectCores() - 1

# data from brms: https://paul-buerkner.github.io/brms/reference/epilepsy.html
dat <- brms::epilepsy

# initialize multiverse 
M_epi = multiverse()

tic()
inside(M_epi, {

  # preprocessing: normalizing Age & Base
  dat <- dat %>% 
    mutate(Age_n =(dat$Age - mean(dat$Age)) / sd(dat$Age), 
           Base_n = (dat$Base - mean(dat$Base)) / sd(dat$Base))
  
  # priors <- c(set_prior("normal(0,5)", class = "b"),
  #   set_prior("normal(0,10)", class = "Intercept"),
  #   set_prior("cauchy(0,5)", class = "sd"))
  
  #prior = branch(prior, 
  #"default" ~ NULL %when% formula, 
  #"" ~  %when% ),
  
  # set_prior(R2D2(mean_R2 = 0.8, prec_R2 = 10))
                      
  # compile models with different modelling choices 
  fit_empty <- brm(count ~ branch(formula,
                                "eq1" ~ Trt,
                                "eq2" ~ Trt + Base_n, 
                                "eq3" ~ Trt + Age_n,
                                "eq4" ~ Trt + Base_n + Age_n, 
                                "eq5" ~ Trt + Base_n + (1 | patient), 
                                "eq6" ~ Trt + Age_n + (1 | patient),
                                "eq7" ~ Trt + Base_n + Age_n + (1 | patient),
                                "eq8" ~ Trt + Base_n + Age_n + (1 | patient) + (1 | visit),
                                "eq9" ~ Trt + Base_n + Age_n + (1 | patient) + (1 | visit) + (1 | obs)),
                 data = dat, 
                 family = branch(family, 
                                 "poisson" ~ poisson(),
                                 "negbinom" ~ negbinomial()),
                 chains = 0)
  
  # store compiled model
  write_rds(fit_empty, here::here("fit_empty.rds"))
  fit_empty <- read_rds(here::here("fit_empty.rds"))
  
  # use update to sample without recompiling 
  mod_epi <- update(fit_empty, 
                    recompile = FALSE,
                    chains = 4, 
                    cores = nc,
                    save_pars = save_pars(all = TRUE),
                    seed = 12345678)
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

# extract variables of interest from multiverse object
multi_epi <- M_epi %>% 
  multiverse::expand() %>% 
  extract_vars_df(mod_epi)

# add metrics 
tic()
multi_dict_epi <- evaluate_multiverse(multi = multi_epi, mod = mod_epi, outcome = dat$count)
toc()

# store results 
write_rds(multi_epi, here::here("case-study-epilepsy", "results", "multiverse_epi.rds"))
write_rds(multi_dict_epi, here::here("case-study-epilepsy", "results", "multi_dict_epi.rds"))
