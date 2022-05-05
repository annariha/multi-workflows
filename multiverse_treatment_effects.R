# load packages 
if(!requireNamespace("pacman"))install.packages("pacman")
pacman::p_load(here, haven, tictoc, knitr, tidyverse, tidybayes, brms, bayesplot, cowplot, loo, purrr, multiverse)

# load data 
path <- here::here("Data", "159422-V2", "pnas_povreductioneeg.dta")
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

# lognormal() instead of log(outcome)
# models are not nested anymore BUT target var is on same scale 
# focus on log-normal model as normal model is probably not true

M = multiverse()

inside(M, {
  
  # preprocessing (from Andrews code)
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
                    "pre_score1" ~ standardize(birthweight) + standardize(gestage) + 
                      standardize(momedu) + standardize(income) + white - black + 
                      standardize(momhealth) - standardize(smoking) - standardize(drinking)
                    #,"pre_score2" ~ standardize(momedu) + standardize(income) + white - black
                    #,"pre_score3" ~ standardize(birthweight) + standardize(gestage) + 
                      #standardize(momhealth) - standardize(smoking) - standardize(drinking)
                    ))
  
  # 2. include different variables: treat, pre_score, girl 
  # 3. normal vs. log-normal model 
  mod <- brm(formula = 
               branch(outcome,
                      "abs" ~ absalpha
                      #,"contrast" ~ log(absalpha) + log(absgamma) - log(abstheta)
                      ) ~ 
               branch(predictors, 
                      "eq_1" ~ treat,
                      "eq_2" ~ treat + pre_score,
                      "eq_3" ~ treat + pre_score + girl
                      #,"eq_4" ~ treat + girl + birthweight + gestage + 
                        #momedu + income + white + black + momhealth + 
                        #smoking + drinking
                      ),
             data = df, 
             family = 
               branch(obs_model, 
                      "normal" ~ gaussian(), 
                      "lognormal" ~ lognormal()))
  
  # check convergence diagnostics: n_eff, Rhat 
  neffs_mod <- neff_ratio(mod)
  rhats_mod <- rhat(mod)
  # evaluate with loo-package
  loo_results <- loo(mod)

})

# check multiverse settings
expand(M)

# execute entire multiverse analysis
tic()
execute_multiverse(M)
toc()

# access results 
multiverse_table <- multiverse::expand(M) %>% 
  extract_variables(mod, neffs_mod, rhats_mod, loo_results)

# save results 
saveRDS(multiverse_table, "multiverse-ex1.rds")

# access list of brms-fits (mod) and loo results
d <- multiverse_table %>%
  select(.universe, 
         pre_score_calculation, 
         predictors, 
         obs_model, 
         mod, 
         neffs_mod, 
         rhats_mod, 
         loo_results) %>%
  arrange(predictors)

# check convergence diagnostics 
d$neffs_mod
# Is effective sample size >0.5?
# effective sample size: 
# estimate of # of independent draws from the posterior distribution of the estimand of interest

d$rhats_mod
# Are they close to 1?
test <- unlist(d$rhats_mod)

# plot all posterior results from list of models 
do.call(compare_posteriors, d$mod)
ggsave("post_plot.png")

# plot in two facets: one for normal, one for lognormal 
do.call(compare_posteriors, d$mod) + 
  facet_grid(rows = NULL, cols = vars(model %in% c(1,3,5)), scales = "free")

d_ln_mod <- d %>%
  filter(obs_model == "lognormal")

do.call(compare_posteriors, d_ln_mod$mod)
ggsave("post_plot_lnmod.png")

# Next step: build in facet option into plot-function! 
# exclude intercept 
# put intercept in extra plot 

# compare models using loo-cv 
names(d$loo_results) <- paste("Model", seq_along(1:NROW(d))) # add names to identify models in loo-output 
loo_compare(d$loo_results)

# vector of obs. outcome values alpha
y_alpha <- as.vector(data_eeg$absalpha)

# add grouping: simple, medium, complex model 

# compare all models to 
# 1. model average 
# 2. encompassing model