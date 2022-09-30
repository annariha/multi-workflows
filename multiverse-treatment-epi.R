#! /usr/bin/Rscript --vanilla

# load packages 
if(!requireNamespace("pacman"))install.packages("pacman")

pacman::p_load(here, tictoc, purrr, parallel, brms, Matrix, tidyverse, 
               tidybayes, transport, bayesplot, cowplot, RColorBrewer,
               loo, multiverse, priorsense)

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
  
  # fit model with default settings
  mod_epi <- brm(count ~ 
                   branch(formula,
                          "eq1" ~ Trt,
                          "eq2" ~ Trt + Base_n, 
                          "eq3" ~ Trt + Age_n,
                          "eq4" ~ Trt + Base_n + Age_n, 
                          "eq5" ~ Trt + Base_n + (1 | patient), 
                          "eq6" ~ Trt + Age_n + (1 | patient),
                          "eq7" ~ Trt + Base_n + Age_n + (1 | patient),
                          "eq8" ~ Trt + Base_n + Age_n + (1 | patient) + (1 | visit),
                          "eq9" ~ Trt + Base_n + Age_n + (1 | patient) + (1 | visit) + (1 | obs)),
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

write_rds(multi_epi, here::here("results", "multiverse_epi.rds"))

# add metrics 
tic()
multi_dict_epi <- evaluate_multiverse(multi = multi_epi, mod = mod_epi, outcome = dat$count)
toc()

write_rds(multi_dict_epi, here::here("results", "multi_dict_epi.rds"))

################################################################################

# get vector of all names despite "treat"
param_names <-
  map(multi_epi$mod_epi, posterior::variables) %>% 
  unlist() %>% 
  unique() %>%
  stringr::str_remove(., "b_")
param_names[2] <- "treat"

# only plot treatment effect
drop_vec <- param_names[! param_names %in% c("treat")]

# ungrouped 
posterior_plot_ungrouped <- do.call(
  compare_posteriors, 
  list(multi_epi$mod_epi, dropvars = c(drop_vec, "(Intercept)")))

save_plot(here::here("figures", "post_treat_epi_all.png"), 
          posterior_plot_ungrouped, 
          base_height = 5, 
          base_aspect_ratio = 1.4)

# grouped 
to_mods <- as_labeller(c(`TRUE` = "poisson", `FALSE` = "negbinom"))
posterior_plot_treat <- do.call(compare_posteriors, 
  list(multi_epi$mod_epi, dropvars = c(drop_vec, "(Intercept)"))) + 
  facet_grid(rows = NULL, 
             vars(model %in% c(1,3,5,7,9,11,13,15,17)), 
             scales = "free", 
             labeller = to_mods)

save_plot(here::here("figures", "post_treat_epi.png"), 
          posterior_plot_treat, 
          base_height = 5, 
          base_aspect_ratio = 1.4)