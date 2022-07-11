#! /usr/bin/Rscript --vanilla

# load packages 
if(!requireNamespace("pacman"))install.packages("pacman")
pacman::p_load(here, parallel, haven, tictoc, knitr, tidyverse, tidybayes, brms, bayesplot, cowplot, loo, purrr, multiverse)

# set seed
set.seed(42424242)

# set # of cores 
nc <- parallel::detectCores() - 1

# load data 
path <- here::here("data", "pnas_povreductioneeg.dta")
data_eeg <- read_dta(path)

# data without NAs in absalpha
withoutNA <- !is.na(data_eeg$absalpha)
data_eeg <- data_eeg[withoutNA,]

# load some helper functions for preprocessing, plotting
source("helper_functions.R")

# create multiverse
# 1. pre_score definition
# 2. include different variables
# 3. normal model vs. log-normal model

M_eeg = multiverse()

tic()
inside(M_eeg, {
  
  # preprocessing (from Andrew Gelman's example on blog)
  df <- data_eeg %>%
    mutate(
      girl = as.numeric(cfemalea0),
      birthweight = as.numeric(cweightlba0_r),
      gestage = as.numeric(cgestagewksa0_r),
      momedu = mean_impute(as.numeric(momeduyrsa0)),
      income = mean_impute(as.numeric(hhrevisedincomea0)),
      white = as.numeric(mracea0) == 1,
      black = as.numeric(mracea0) == 2,
      momhealth = as.numeric(mgoodhealtha0),
      smoking = as.numeric(mcigduringavgwka0_r),
      drinking = as.numeric(malcduringavgwka0_r)
    )
  
  # 1. different pre_score definitions - pre_score1 from Andrews code
  df <- df %>%
    mutate(pre_score = branch(pre_score_calculation,
                    "prescore_ag" ~ standardize(birthweight) + standardize(gestage) + 
                      standardize(momedu) + standardize(income) + white - black + 
                      standardize(momhealth) - standardize(smoking) - standardize(drinking),
                    #,"pre_score2" ~ standardize(momedu) + standardize(income) + white - black
                    "prescore_h" ~ standardize(birthweight) + standardize(gestage) +
                      standardize(momhealth) - standardize(smoking) - standardize(drinking)
                    ))
  
  # 2. include different variables: treat, pre_score, girl 
  # 3. normal vs. log-normal model 
  mod_eeg <- brm(absalpha ~ 
               branch(formula, 
                      "eq_1" ~ treat,
                      "eq_2" ~ treat + pre_score,
                      "eq_3" ~ treat + pre_score + girl,
                      "eq_4" ~ treat + girl + birthweight + gestage + 
                      momedu + income + white + black + momhealth + 
                      smoking + drinking
                      ),
             data = df, 
             family = 
               branch(family, 
                      "normal" ~ gaussian(), 
                      "lognormal" ~ lognormal()),
             cores = nc,
             save_pars = save_pars(all = TRUE))
})

# check multiverse settings
# expand(M)

# execute entire multiverse analysis
execute_multiverse(M_eeg)
toc()

# extract brmsfit-objects 
multi_eeg <- M_eeg %>% 
  multiverse::expand() %>% 
  extract_vars_df(mod_eeg)

# evaluate 
tic()
multi_dict_eeg <- evaluate_multiverse(multi_eeg, mod_eeg, outcome = data_eeg$absalpha)
toc()

write_rds(multi_dict_eeg, here::here("results", "multi_dict_eeg.rds"))

################################################################################

# lognormal() instead of log(outcome)
# models are not nested anymore BUT target var is on same scale 
# focus on log-normal model as normal model is probably not true

################################################################################

# plot all posterior results from list of models 
do.call(compare_posteriors, list(d$mod, dropvars = c("(Intercept)")))
ggsave("post_plot.png")

# plot in two facets: one for normal, one for lognormal 
to_mods <- as_labeller(c(`TRUE` = "normal", `FALSE` = "lognormal"))
do.call(compare_posteriors, list(d$mod, dropvars = c("(Intercept)"))) + 
  facet_grid(rows = NULL, vars(model %in% c(1,3,5,7)), 
             scales = "free",
             labeller = to_mods)

ggsave("post_plot_mods.png")

# get vector of all names despite "treat"
param_names <-
  map(d$mod, posterior::variables) %>% 
  unlist() %>% 
  unique() %>%
  stringr::str_remove(., "b_")

drop_vec <- param_names[! param_names %in% c("treat")]

to_mods <- as_labeller(c(`TRUE` = "normal", `FALSE` = "lognormal"))
do.call(compare_posteriors, list(d$mod, dropvars = c(drop_vec, "(Intercept)"))) + 
  facet_grid(rows = NULL, vars(model %in% c(1,3,5,7)), 
             scales = "free",
             labeller = to_mods)

ggsave("post_plot_mods2.png")

# compare models using loo-cv 
names(d$loo_results) <- paste("Model", seq_along(1:NROW(d))) # add names to identify models in loo-output 
loo_compare(d$loo_results)

# ppc_dens_overlay()

# vector of obs. outcome values alpha
y_alpha <- as.vector(data_eeg$absalpha)

# Next: 
# 1. customize plots 
# e.g. if range of Intercept > range of other vars -> exclude Int from plot
# e.g. drop misspecified models for plotting 
# 2. add grouping: simple, medium, complex model 
# 3. compare all models to (1) model average and (2) encompassing model (if existent)