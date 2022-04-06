# load packages 
if(!requireNamespace("pacman"))install.packages("pacman")
pacman::p_load(here, haven, tictoc, knitr, tidyverse, tidybayes, rstanarm, bayesplot, cowplot, loo, purrr, multiverse)

# load data 
path <- here::here("Data", "159422-V2", "pnas_povreductioneeg.dta")
data_eeg <- read_dta(path)

# data without NAs in absalpha
withoutNA <- !is.na(data_eeg$absalpha)
data_eeg <- data_eeg[withoutNA,]

# helper functions (from Andrews code)
mean_impute <- function(a) ifelse(is.na(a), mean(a[!is.na(a)]), a)
standardize <- function(a) (a - mean(a))/(2*sd(a))

# create multiverse
# 1. different pre_score definitions
# 2. contrasts as outcome 
# 3. include different variables
# 4. normal model vs. log-normal model

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
                      standardize(momhealth) - standardize(smoking) - standardize(drinking),
                    "pre_score2" ~ standardize(momedu) + standardize(income) + white - black,
                    "pre_score3" ~ standardize(birthweight) + standardize(gestage) + 
                      standardize(momhealth) - standardize(smoking) - standardize(drinking)))
  
  # 2. contrasts as alternative outcome 
  # 3. include different variables: treat, pre_score, girl 
  # 4. normal vs. log-normal model 
  mod <- stan_glm(branch(outcome, 
                         "abs" ~ absalpha, 
                         "log" ~ log(absalpha), 
                         "contrast" ~ log(absalpha) + log(absgamma) - log(abstheta)) ~ 
                    branch(predictors, 
                           "eq_1" ~ treat,
                           "eq_2" ~ treat + pre_score,
                           "eq_3" ~ treat + pre_score + girl),
                  family = 
                    branch(obs_model, 
                           "normal" ~ gaussian(link = "identity") 
                           #,"log_normal" ~ gaussian(link = "log") 
                           # this does not converge with default settings
                           ),
                  data=df, 
                  refresh=0)
  
  # matrix of draws from the posterior pred. distribution
  yrep <- posterior_predict(mod, draws = 1000)
  # posterior results for alpha
  posterior_mean_outcome <- mean(as.array(mod))
  # posterior treatment effect
  posterior_mean_treat <- mod$coefficients["treat"]
  
})

# check multiverse settings
expand(M)

# execute entire multiverse analysis
tic()
execute_multiverse(M)
toc()

# access results 
multiverse_table <- multiverse::expand(M) %>% 
  extract_variables(mod, posterior_mean_outcome, posterior_mean_treat, yrep)

# save results 

# inspect posterior treatment effect sizes
multiverse_table %>%
  select(pre_score_calculation, 
         outcome, 
         predictors, 
         obs_model, 
         posterior_mean_treat, 
         posterior_mean_outcome, 
         yrep) %>%
  arrange(outcome)

# vector of obs. outcome values alpha
y_alpha <- as.vector(data_eeg$absalpha)

