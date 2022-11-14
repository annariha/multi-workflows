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

# data
dat <- brms::epilepsy 

# create combinations ####

families <- list(poisson = poisson(), 
                 negbinomial = negbinomial())

# colnames_epi <- names(dat)

combinations_df <- expand.grid(
  family = names(families),
  # fixed effects 
  Trt = c("", "Trt"), 
  zBase = c("", "zBase"),
  zAge = c("", "zAge"),
  # random effects 
  patient = c("", "(1 | patient)"),
  visit = c("", "(1 | visit)"),
  obs = c("", "(1 | obs)")
  )

# add interaction effect in the rows where treatment was left out, (i.e., where Trt == "")
combinations_df<- combinations_df %>% 
  mutate(zBaseTrt = factor(
    case_when(
      Trt == "Trt" ~ "",
      Trt == "" ~ "zBase * Trt"))) %>% 
  # reorder to have family and treatment effects first 
  select(family, Trt, zBaseTrt, everything())

# create formulas ####

build_brms_formula <- function(row, ...){
  outcome = "count"
  # which cells in the row are not "family" and non-empty?
  in_id <- c(which(names(row) != "family" & row != ""))
  # cells that are included in the formula
  covars <- row[in_id]
  # extract levels for formula
  covars <- as.character(unlist(covars))
  # paste formula
  formula1 = paste(outcome, "~", paste(covars, collapse = "+")) 
  # turn into brms-formula
  fam = as.character(unlist(row["family"]))
  formula = brmsformula(as.formula(formula1), family=fam)
  out <- formula 
} 

# for testing
row <- combinations_df[100,]
formula <- build_brms_formula(row)

for (i in 1:NROW(combinations_df)){
  combinations_df$formula[i,] = build_brms_formula(combinations_df[i,])
}

# create formula for each row 
combinations_df <- combinations_df %>% 
  mutate(formula = )

# fit model for each combination 

# fit_model(row) 

# 