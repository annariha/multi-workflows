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
# 1.. different pre_score definitions
# 2. contrasts as outcome 

#eeg_1$contrast <- log(eeg_1$absalpha) + log(eeg_1$absgamma) - log(eeg_1$abstheta)
#M4 <- stan_glm(contrast ~ treat, data=eeg_1, refresh=0)
#M5 <- stan_glm(contrast ~ treat + pre_score, data=eeg_1, refresh=0)

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
  #df <- df %>%
  #  mutate(contrast = log(absalpha) + log(absgamma) - log(abstheta))

  # 3. include different variables: treat, pre_score, girl 
  # 4. normal vs. log-normal model 
  mod <- stan_glm(branch(outcome, "abs" ~ absalpha, "log" ~ log(absalpha)) ~ 
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
  
  # extract posterior predictions 
  # vector of outcome values
  y_alpha <- as.vector(absalpha)
  
  # matrix of draws from the posterior pred. distribution
  yrep_alpha <-posterior_predict(mod, draws = 1000)
  
  # posterior results 
  posterior_alpha <- as.array(mod)
  mean(posterior_alpha)
  
  # outputs of interest
  out <- list(
    y_alpha = y_alpha,
    yrep_alpha = yrep_alpha,
    posterior_alpha = posterior_alpha)
  
})

# check multiverse settings
expand(M)

# execute entire multiverse analysis
tic()
execute_multiverse(M)
toc()

# access results 
expand(M) %>% select(.results)
