#! /usr/bin/Rscript --vanilla

# using models and some code from https://avehtari.github.io/modelselection/bodyfat.html
# load packages 
if(!requireNamespace("pacman"))install.packages("pacman")
pacman::p_load(here, tictoc, purrr, parallel, brms, Matrix, tidyverse, tidybayes, transport, bayesplot, cowplot, loo, multiverse, priorsense)

# set seed
set.seed(42424242)

# set # of cores 
nc <- detectCores() - 1

# helper functions
source("helper_functions.R")

# load data 
dat <- read.table(here::here("data", "bodyfat.txt"), header = T, sep = ";")
dat[,4:19] <- scale(dat[,4:19])
# no-one can have 0% body fat
dat <- dat[dat$siri>0,]
# y <- dat[,"siri"]
dat <- as.data.frame(dat)
n <- nrow(dat)
# horseshoe: prior guess for the number of relevant variables
p0 <- 5
tau0 <- 0.03

# initialize multiverse 
M_bf = multiverse()

tic()
inside(M_bf, {
  
  # list of predictors
  pred <- branch(preds, 
                 "all" ~ c("age", "weight", "height", "neck", "chest", "abdomen", "hip", "thigh", "knee", "ankle", "biceps", "forearm", "wrist"), 
                 #"abd-height1" ~ c("height", "abdomen", "wrist", "age", "neck", "forearm", "chest"), # model finally chosen by the authors 
                 "abd-height2" ~ c("height", "abdomen"), # according to the authors: "abdomen and height as two central IVs"
                 "projpred" ~ c("weight", "abdomen")) 
  
  # target <- "siri"
  eq <- formula(paste("siri ~", paste(pred, collapse = " + ")))
  p <- length(pred)
  # global scale 
  tau0 <<- p0 / (p-p0) * 1/sqrt(n)
  if (tau0 <=0){
    tau0=1
  }
  
  # par_ratio=ratio of # of expected nonzero coeffs to expected zero coeffs
  # ratio <- p0 / (p-p0)
  # global scale
  # tau0 <- p0 / (p-p0) * 1/sqrt(n) 
  print(p) 
  print(tau0) 
  
  test = prior(horseshoe(scale_global=tau0))
  #tau0 = p0/(p-p0) * 1/sqrt(n)
  #prior = prior(horseshoe(scale_global=tau0))
  
  priors <- branch(prior_setting, 
                   "default" ~ NULL %when% (preds %in% c("abd-height2", "projpred")), 
                   "horseshoe" ~ test %when% (preds %in% c("all", "abd-height1")))
  
  mod_bf <- brm(
    formula = eq, 
    data = dat, 
    prior = priors, 
    cores = nc,
    save_pars = save_pars(all = TRUE), 
    seed = 12345678)

})

expand(M_bf)
# run multiverse 
M_bf %>% execute_multiverse()
toc()

# expand multiverse and extract model objects
multi_bf <- M_bf %>% 
  multiverse::expand() %>% 
  extract_vars_df(mod_bf)

# save intermediate results for second-level repro
  write_rds(multi_epi, here::here("results", "multiverse_epi.rds"))

# add metrics 
tic()
multi_dict_epi <- evaluate_multiverse(multi = multi_bf, mod = mod_bf, outcome = dat$siri)
toc()

# save intermediate results for second-level repro
write_rds(multi_dict_epi, here::here("results", "multi_dict_epi.rds"))

###############################################################################

# list of predictors
pred <- c("age", "weight", "height", "neck", "chest", "abdomen", "hip", "thigh", "knee", "ankle", "biceps", "forearm", "wrist")
target <- "siri"
# formula <- formula(paste("siri ~", paste(pred, collapse = " + ")))
p <- length(pred)
p0 = 5 # horseshoe: prior guess for the number of relevant variables
tau0 <- p0/(p-p0) * 1/sqrt(n)

# fit biggest model
tic()
fit_all_hs <- brm(
  formula = formula(paste("siri ~", paste(pred, collapse = " + "))),
  data = dat, 
  prior = prior(horseshoe(scale_global=tau0)),
  cores = nc,
  save_pars = save_pars(all = TRUE), 
  seed = 12345678)
toc()
# 54.597 sec elapsed