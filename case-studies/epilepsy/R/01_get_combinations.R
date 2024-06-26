#! /usr/bin/Rscript --vanilla

# set environment profile for epilepsy case studies 
Sys.setenv(RENV_PROFILE = "case-study-epilepsy")
  
# load packages 
if(!requireNamespace("pacman")) install.packages("pacman")

pacman::p_load(here, tictoc, future, furrr, purrr, parallel, brms, Matrix, tidyverse, 
               tidybayes, transport, loo, multiverse, priorsense, cmdstanr,
               ggdendro, cowplot)
# run once
# cmdstanr::install_cmdstan()

# load custom functions
source(here::here("case-studies", "epilepsy", "R", "build_name.R"))

# set seed
set.seed(42424242)

# how is the outcome called in the data ####
#dat <- brms::epilepsy 
outcome_str <- "count"

# create dataframe of combinations of model components ####
combinations_df <- expand.grid(
  family = names(list(poisson = poisson(), negbinomial = negbinomial())),
  prior = list(brms_default = "NULL", brms_horseshoe = "horseshoe(3)"),
  # population-level effects 
  Trt = c("", "Trt"), 
  zBase = c("", "zBase"),
  zAge = c("", "zAge"),
  # varying effects
  patient = c("", "(1 | patient)"),
  visit = c("", "(1 | visit)"),
  obs = c("", "(1 | obs)")
)

combinations_df <- combinations_df |> 
  # add interaction effect in the rows where treatment was left out, (i.e., where Trt == "")
  mutate(zBaseTrt = factor(
    case_when(
      Trt == "Trt" ~ "",
      Trt == "" ~ "zBase * Trt"))) |> 
  # filter out rows with interaction and zBase
  filter(!(zBaseTrt == "zBase * Trt" & combinations_df$zBase == "zBase"))

combinations_df <- combinations_df |>  
  # add outcome name 
  mutate(outcome = rep_len(outcome_str, NROW(combinations_df))) |>
  # add prior names for easier summarising, plotting etc. 
  mutate(priors = names(combinations_df$prior)) |>
  # reorder to have outcome name, family and treatment effects first 
  select(outcome, family, priors, prior, Trt, zBaseTrt, everything())

# set row names to model names, needed for plotting
rownames(combinations_df) <- apply(combinations_df, 1, build_name)

# store combinations dataframe to use in other scripts ####
filedir = here::here("case-studies", "epilepsy", "results", "prelim")
if (!dir.exists(filedir)) {dir.create(filedir)}
write_rds(combinations_df, here::here("case-studies", "epilepsy", "results", "prelim", "combinations_df.rds"))