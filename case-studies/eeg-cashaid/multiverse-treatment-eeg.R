#! /usr/bin/Rscript --vanilla

# load packages 
if(!requireNamespace("pacman"))install.packages("pacman")
pacman::p_load(here, haven, tictoc, purrr, parallel, brms, Matrix, tidyverse, 
               tidybayes, transport, bayesplot, cowplot, RColorBrewer,
               loo, multiverse, priorsense)

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

write_rds(multi_eeg, here::here("results", "multiverse_eeg.rds"))

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

# get vector of all names despite "treat"
param_names <-
  map(multi_eeg$mod_eeg, posterior::variables) %>% 
  unlist() %>% 
  unique() %>%
  stringr::str_remove(., "b_")

# only plot treatment effect
drop_vec <- param_names[! param_names %in% c("treat")]

# ungrouped 
posterior_plot_ungrouped <- do.call(
  compare_posteriors, 
  list(multi_eeg$mod_eeg, dropvars = c(drop_vec, "(Intercept)")))

save_plot(here::here("figures", "post_treat_eeg_all.png"), 
          posterior_plot_ungrouped, 
          base_height = 5, 
          base_aspect_ratio = 1.4)

# grouped 
to_mods <- as_labeller(c(`TRUE` = "normal", `FALSE` = "lognormal"))
posterior_plot_grouped <- do.call(
  compare_posteriors, 
  list(multi_eeg$mod_eeg, dropvars = c(drop_vec, "(Intercept)"))) + 
  facet_grid(rows = NULL, vars(model %in% c(1,3,5,7,9,11,13,15)), 
             scales = "free",
             labeller = to_mods)

save_plot(here::here("figures", "post_treat_eeg.png"), 
          posterior_plot_grouped, 
          base_height = 5, 
          base_aspect_ratio = 1.4)