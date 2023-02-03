#! /usr/bin/Rscript --vanilla

# setup ####
# load packages 
if(!requireNamespace("pacman"))install.packages("pacman")
pacman::p_load(here, haven, tictoc, purrr, parallel, brms, Matrix, tidyverse, 
               tidybayes, transport, bayesplot, cowplot, RColorBrewer,
               loo, multiverse, priorsense)
# run once
# cmdstanr::install_cmdstan()

# set seed
set.seed(42424242)

# set # of cores 
nc <- parallel::detectCores() - 1

# load data 
path <- here::here("case-studies", "eeg-cashaid", "data", "pnas_povreductioneeg.dta")
data_eeg <- read_dta(path)

# helper functions (from Andrew Gelman's code)
mean_impute <- function(a) ifelse(is.na(a), mean(a[!is.na(a)]), a)
standardize <- function(a) (a - mean(a))/(2*sd(a))

# data without NAs in absalpha
withoutNA <- !is.na(data_eeg$absalpha)
data_eeg <- data_eeg[withoutNA,]
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
    drinking = as.numeric(malcduringavgwka0_r),
    prescore1 = standardize(birthweight) + standardize(gestage) + 
      standardize(momedu) + standardize(income) + white - black + 
      standardize(momhealth) - standardize(smoking) - standardize(drinking),
    prescore2 = standardize(birthweight) + standardize(gestage) +
      standardize(momhealth) - standardize(smoking) - standardize(drinking)
  )

# create combinations ####

# main interests: 
# 1. different pre_score definitions, prescore1 from Andrews code
# 2. include different variables
# 3. normal vs. log-normal model 

families <- list(poisson = gaussian(), 
                 lognormal = lognormal())

# colnames_epi <- names(dat)

combinations_df <- expand.grid(
  # pre-score
  prescore = c("", "prescore1", "prescore2"),
  # model family
  family = names(families),
  # fixed effects 
  treat = c("treat"), 
  girl = c("", "girl"),
  birthweight = c("", "birthweight"),
  gestage = c("", "gestage"),
  momedu = c("", "momedu"), 
  income = c("", "income"),
  white = c("", "white"),
  black = c("", "black"), 
  momhealth = c("", "momhealth"),  
  smoking = c("", "smoking"),
  drinking = c("", "drinking")
)

# remove models with prescore and same vars
combinations_df <- combinations_df %>%
  # filter out rows with prescore1 and all vars starting from birthweight
  filter(!(prescore == "prescore1" & birthweight == "birthweight")) %>%
  filter(!(prescore == "prescore1" & gestage == "gestage")) %>%
  filter(!(prescore == "prescore1" & momedu == "momedu")) %>%
  filter(!(prescore == "prescore1" & income == "income")) %>%
  filter(!(prescore == "prescore1" & white == "white")) %>%
  filter(!(prescore == "prescore1" & black == "black")) %>%
  filter(!(prescore == "prescore1" & momhealth == "momhealth")) %>%
  filter(!(prescore == "prescore1" & smoking == "smoking")) %>%
  filter(!(prescore == "prescore1" & drinking == "drinking")) %>%
  filter(!(prescore == "prescore2" & birthweight == "birthweight")) %>%
  filter(!(prescore == "prescore2" & gestage == "gestage")) %>%
  filter(!(prescore == "prescore2" & momhealth == "momhealth")) %>%
  filter(!(prescore == "prescore2" & smoking == "smoking")) %>%
  filter(!(prescore == "prescore2" & drinking == "drinking"))

